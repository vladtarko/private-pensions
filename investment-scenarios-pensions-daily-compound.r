# Investment scenarios: pensions and inheritances -- reinvested total returns
#
# Based on investment-scenarios-pensions.r with these data corrections:
#   1. Investment growth includes stock-price changes AND reinvested dividends:
#      daily S&P 500 Total Return index data where complete years are available,
#      published annual reinvested total returns for the earlier history.
#   2. Published BLS CPI-U observations replace provisional recent CPI values.
#   3. Fund expenses reduce gross portfolio growth before inflation adjustment.
#
# This models a dividend-reinvesting index-fund strategy, before investor taxes
# (e.g. a tax-deferred account). The benchmark reinvests dividends on ex-dates;
# an actual fund's distribution dates and tracking error can differ slightly.
# Pre-1957 returns describe the predecessor index, not a then-existing ETF.
# Retirement scenarios differ in their REAL interest rate and bequest target.
# Bequests can be specified in dollars or as a share of wealth at age 65.
# See pension-calculation-explained.md for the retirement mathematics; its
# numerical examples were calculated using the earlier data methodology.
#
# Pipeline:  get_sp500() / get_cpi()  ->  add_real_growth()
#         ->  accumulate()  (work period, once: calls wealth_path() per window)
#         ->  run_scenario()  (retirement: calls retirement() per window)
#         ->  summarize_scenario()  (stats for one scenario)
#         ->  summarize_scenarios()  (all scenarios, optional start-year filter)
#         ->  summarize_bequest_scenarios()  (combined bequest tradeoff table)
#
# All monetary inputs and outputs are in constant 2024 purchasing-power dollars.
# Annual saving is $6,500 and initial wealth is $1,000 in these units.

library(tidyverse)
library(tidyquant)
library(readxl)

# data functions -------------------------------------------------------------

# Compound daily index changes for complete calendar years. A preceding-year
# observation is required so the first trading day's return is included.
get_daily_index_growth <- function(symbol, from, to) {
  prices <- tq_get(symbol, get = "stock.prices",
                   from = paste0(year(as.Date(from)) - 1L, "-01-01"), to = to)
  if (!is.data.frame(prices) || nrow(prices) < 2L) {
    stop("Download failed or returned insufficient data for ", symbol, ".")
  }
  if (anyNA(prices$close) || any(!is.finite(prices$close)) ||
      any(prices$close <= 0) || anyDuplicated(prices$date)) {
    stop("Invalid index levels or duplicate dates for ", symbol, ".")
  }
  first_available_year <- year(min(prices$date))

  prices |>
    arrange(date) |>
    mutate(
      year       = year(date),
      growth_day = close / lag(close) - 1
    ) |>
    filter(!is.na(growth_day)) |>
    group_by(year) |>
    summarise(
      days_in_year = n(),
      # Actual compounding, never compounding the arithmetic mean return.
      growth       = prod(1 + growth_day) - 1,
      .groups      = "drop"
    ) |>
    filter(year > first_available_year, year >= year(as.Date(from)),
           year < year(as.Date(to)))
}

# Published annual S&P returns explicitly include reinvested dividends.
# https://www.slickcharts.com/sp500/returns (historical series starts in 1926).
# Returns are published to 0.01 percentage-point precision. They are already
# compounded annual total returns, not dividend yields to add to price growth.
get_annual_total_returns <- function() {
  tables <- rvest::read_html("https://www.slickcharts.com/sp500/returns") |>
    rvest::html_table()
  matches <- keep(tables, \(x) all(c("Year", "Total Return") %in% names(x)))
  if (length(matches) != 1L) {
    stop("Cannot identify the published annual S&P total-return table.")
  }
  result <- matches[[1]] |>
    transmute(year = as.integer(Year),
              published_total_growth = parse_number(`Total Return`) / 100) |>
    arrange(year)
  if (anyNA(result$year) || anyDuplicated(result$year) ||
      any(!is.finite(result$published_total_growth) | result$published_total_growth <= -1)) {
    stop("Invalid published annual S&P total-return data.")
  }
  result
}

# Nominal S&P 500 total returns, before fund fees and investor taxes.
# ^SP500TR already includes dividend reinvestment: do NOT add dividends again.
# Yahoo's TR history starts Jan 4, 1988; without Dec 1987, its 1988 return is
# incomplete. Use published annual returns for 1928-1988 and daily TR from
# 1989 onward. Keep daily-compounded ^GSPC price returns for comparison only.
get_sp500 <- function(from = "1900-01-01", to = "2025-01-01") {
  price <- get_daily_index_growth("^GSPC", from, to) |>
    rename(price_growth = growth)
  total <- get_daily_index_growth("^SP500TR", from, to) |>
    select(year, daily_total_growth = growth)
  annual <- get_annual_total_returns()
  if (nrow(price) == 0L) stop("No complete investment years in the requested range.")

  result <- price |>
    left_join(total, by = "year") |>
    left_join(annual, by = "year") |>
    mutate(
      growth = coalesce(daily_total_growth, published_total_growth),
      return_source = if_else(!is.na(daily_total_growth),
                              "Yahoo ^SP500TR: compounded daily total returns",
                              "Slickcharts: annual reinvested total returns")
    ) |>
    arrange(year)
  if (any(!is.finite(result$growth) | result$growth <= -1) ||
      any(!is.finite(result$published_total_growth)) ||
      any(diff(result$year) != 1)) {
    stop("Complete consecutive annual total-return coverage is required.")
  }
  # Independent overlap check; allow 2 basis points for rounded index levels
  # and the annual table's two-decimal percentage rounding.
  overlap <- result |> filter(!is.na(daily_total_growth))
  if (any(abs(overlap$daily_total_growth - overlap$published_total_growth) > 0.0002)) {
    stop("Daily and published annual total returns disagree; inspect the sources.")
  }
  result
}

# Shiller's historical CPI plus published BLS CPI-U for 2022-2024.
# CUUR0000SA0: US city average, all items, all urban consumers, not seasonally
# adjusted (1982-84 = 100), matching Shiller's modern CPI series.
# Sources: http://www.econ.yale.edu/~shiller/data.htm
#          https://www.bls.gov/developers/api_signature_v2.htm
get_cpi <- function() {
  
  shiller_file <- tempfile(fileext = ".xls")
  on.exit(unlink(shiller_file), add = TRUE)
  download.file("http://www.econ.yale.edu/~shiller/data/ie_data.xls",
                destfile = shiller_file,
                mode     = "wb",
                quiet    = TRUE)
  
  # Read as text so spreadsheet footnotes do not trigger numeric-cell warnings;
  # those non-data rows become NA and are discarded below.
  cpi <- read_excel(shiller_file, sheet = "Data", skip = 7,
                    col_types = "text", .name_repair = "unique_quiet") |>
    select(Date, CPI) |>
    mutate(
      Date = suppressWarnings(as.numeric(Date)),
      CPI  = suppressWarnings(as.numeric(CPI))
    ) |>
    filter(!is.na(Date), !is.na(CPI)) |>
    transmute(year = as.integer(floor(Date)),
              month = as.integer(round((Date - floor(Date)) * 100)), CPI) |>
    filter(year < 2022)  # BLS supplies all observations from 2022 onward

  # Fetch actual monthly observations: no extrapolation or placeholder CPI.
  bls <- jsonlite::fromJSON(paste0(
    "https://api.bls.gov/publicAPI/v2/timeseries/data/CUUR0000SA0",
    "?startyear=2022&endyear=2024"
  ))
  if (!identical(bls$status, "REQUEST_SUCCEEDED") ||
      is.null(bls$Results$series$data)) {
    stop("BLS CPI download failed; actual 2022-2024 CPI is required.")
  }
  recent_cpi <- as_tibble(bls$Results$series$data[[1]]) |>
    filter(period %in% sprintf("M%02d", 1:12)) |>
    transmute(year = as.integer(year), month = as.integer(sub("M", "", period)),
              CPI = as.numeric(value))
  if (!setequal(recent_cpi$year, 2022:2024)) {
    stop("BLS did not return all requested CPI years (2022-2024).")
  }

  monthly <- bind_rows(cpi, recent_cpi)
  coverage <- monthly |>
    summarise(rows = n(), months = n_distinct(month), .by = year)
  if (any(!is.finite(monthly$CPI) | monthly$CPI <= 0) ||
      any(!monthly$month %in% 1:12) ||
      any(coverage$rows != 12L | coverage$months != 12L)) {
    stop("CPI must contain twelve unique, valid monthly observations per year.")
  }

  # Retain annual-average CPI inflation for comparability with the original
  # model; it is an annual approximation, rather than December-to-December CPI.
  monthly |>
    summarise(cpi = mean(CPI), .by = year) |>
    arrange(year) |>
    mutate(inflation = cpi / lag(cpi) - 1)
}

# add inflation and gross real total returns (expenses are applied in wealth_path)
add_real_growth <- function(sp500_summary, cpi_annual) {
  if (anyDuplicated(cpi_annual$year)) {
    stop("CPI data must have one observation per year.")
  }
  result <- sp500_summary |>
    left_join(select(cpi_annual, year, inflation), by = "year") |>
    mutate(real_growth = (1 + growth) / (1 + inflation) - 1)
  if (any(!is.finite(result$real_growth))) {
    stop("Valid inflation data are required for every investment year.")
  }
  result
}


# simulation functions -------------------------------------------------------

# wealth progression over ages 25-65 for the 40-year window starting in
# `start_year`, investing in the S&P 500 with all dividends reinvested.
# fee is an annual expense ratio, modeled as continuously accruing expenses:
#   net real gross return = (1 + nominal total return) * exp(-fee) / (1 + inflation)
# This avoids subtracting a nominal expense rate from an already-deflated return.
# Contributions arrive at each year-end, in constant 2024 dollars.
wealth_path <- function(start_year, sp500_summary, window_size = 40,
                        save = 6500, initial = 1000, fee = 0.002) {
  if (length(fee) != 1L || !is.finite(fee) || fee < 0 || fee >= 1) {
    stop("fee must be an annual expense ratio between 0 (inclusive) and 1.")
  }
  window <- sp500_summary |>
    filter(year >= start_year, year < start_year + window_size) |>
    arrange(year)
  if (nrow(window) != window_size || any(diff(window$year) != 1)) {
    stop("The requested window must contain consecutive annual returns.")
  }
  interest <- (1 + window$real_growth) * exp(-fee) - 1

  wealth <- numeric(window_size + 1L)
  wealth[1] <- initial       # initial inheritance

  for (t in 2:length(wealth)) {
    wealth[t] <- wealth[t-1] + wealth[t-1] * interest[t-1] + save
  }
  
  wealth
}

# work period for all 40-year windows: wealth at age 65 per start year
accumulate <- function(sp500_summary, window_size = 40,
                       save = 6500, initial = 1000, fee = 0.002) {
  
  start_years <- min(sp500_summary$year):(max(sp500_summary$year) - window_size + 1)
  
  tibble(
    start_year = start_years,
    wealth_65  = map_dbl(start_years, 
                         ~ tail(wealth_path(.x, sp500_summary, window_size,
                                            save = save, initial = initial, fee = fee), n = 1))
  )
}

# retirement: pension over ages 66-100 and inheritance (wealth left at 100),
# given wealth at age 65 and a REAL interest rate earned during retirement.
#
# Timing convention: the age-100 pension IS paid out before the inheritance
# is measured (inheritance = wealth at 100 minus the age-100 pension).
#
# The desired bequest at age 100 is a CHOICE argument:
#   bequest        = target inheritance in dollars (2024 dollars), or
#   bequest_share  = target inheritance as a share of wealth at 65
#   bequest = NULL (default): no target; the original life-expectancy rule,
#                    inheritance is whatever wealth is left at 100
#
# The life-expectancy rule sets the SHAPE of the pension path:
#   pension[i] = lambda * wealth[i] / life_exp[i]
# and lambda (the "withdrawal factor") is solved so that the inheritance
# equals the desired bequest exactly. lambda = 1 is the original rule,
# lambda > 1 spends more aggressively, lambda < 1 spends conservatively.
#
# Note: for a smooth "die with zero" path, use life_exp_100 = 1 -- with the
# original rule (lambda = 1) the inheritance is then exactly zero, without
# the extreme front-loading a lambda-solved bequest = 0 would need.
# life_exp_100 = 1 implies zero inheritance only when lambda = 1;
# a positive bequest target still solves for a different lambda.
retirement <- function(wealth_65, interest = 0.01, 
                       bequest = NULL, bequest_share = NULL,
                       life_exp_65 = 25, life_exp_100 = 8,
                       return_path = FALSE) {
  
  age      <- 65:100
  n        <- length(age)
  life_exp <- seq(from = life_exp_65, to = life_exp_100, length.out = n)
  
  # remaining life expectancy must stay >= 1: pension = wealth / life_exp,
  # so life_exp = 0 means an infinite pension, and life_exp < 1 means
  # withdrawing more than the current balance (wealth turns negative).
  # to "die at 100 with nothing left", use life_exp_100 = 1 (smooth) or
  # bequest = 0 (lambda-solved).
  if (any(life_exp < 1)) {
    stop("life_exp must stay >= 1 (got down to ", round(min(life_exp), 2), 
         "). pension = wealth / life_exp: life_exp < 1 withdraws more than ",
         "the balance, and life_exp = 0 divides by zero. ",
         "To die with nothing left, use life_exp_100 = 1 or bequest = 0 instead.")
  }
  
  # bequest can be given as a share of wealth at 65
  if (!is.null(bequest_share)) bequest <- bequest_share * wealth_65
  
  # maximum possible bequest: consume nothing, compound everything
  max_bequest <- wealth_65 * (1 + interest)^(n - 1)
  
  if (!is.null(bequest) && (bequest < 0 || bequest > max_bequest)) {
    stop("bequest of ", round(bequest), " is infeasible: wealth at 65 is ", 
         round(wealth_65), " (max possible bequest: ", round(max_bequest), ")")
  }
  
  # with pension[i] = lambda * wealth[i] / life_exp[i], the recursion gives
  #   wealth[i] = wealth[i-1] * (1 + interest - lambda / life_exp[i-1])
  # the pension at age 65 is zero (first withdrawal is at age 66), and the
  # age-100 pension is paid before the inheritance is measured, so
  #   inheritance = wealth_65 * (1 + interest)
  #                 * prod(1 + interest - lambda / life_exp[-c(1, n)])
  #                 * (1 - lambda / life_exp[n])
  # strictly decreasing in lambda -> unique solution, found with uniroot()
  if (is.null(bequest)) {
    lambda <- 1     # original rule: no bequest target
  } else {
    terminal_wealth <- function(lambda) {
      wealth_65 * (1 + interest) * 
        prod(1 + interest - lambda / life_exp[-c(1, n)]) * 
        (1 - lambda / life_exp[n])
    }
    lambda_max <- min((1 + interest) * min(life_exp[-c(1, n)]), life_exp[n])
    lambda <- uniroot(
      \(l) terminal_wealth(l) - bequest,
      interval = c(0, lambda_max * 1.0001),
      tol      = 1e-10
    )$root
  }
  
  wealth   <- numeric(n)
  pension  <- numeric(n)
  wealth[1] <- wealth_65
  
  for (i in 2:n) {
    wealth[i]  <- wealth[i-1] + wealth[i-1]*interest - pension[i-1]
    pension[i] <- lambda * wealth[i] / life_exp[i]
  }
  
  if (return_path) {
    return(tibble(
      age = age, life_exp = life_exp,
      wealth_before_pension = wealth, pension = pension,
      wealth_after_pension = wealth - pension,
      withdrawal_factor = lambda
    ))
  }

  tibble(
    avg_pension       = mean(pension[age > 65]),
    inheritance       = tail(wealth, n = 1) - tail(pension, n = 1),
    withdrawal_factor = lambda
  )
}

# one scenario: retirement outcomes for every window,
# given the real interest rate during the pension period (and, optionally,
# a desired bequest in dollar terms or as a share of wealth at 65, and/or
# a different life-expectancy schedule)
run_scenario <- function(accum, interest_retire, 
                         bequest = NULL, bequest_share = NULL,
                         life_exp_65 = 25, life_exp_100 = 8) {
  
  accum |> 
    mutate(retirement = map(wealth_65, retirement, 
                            interest      = interest_retire, 
                            bequest       = bequest, 
                            bequest_share = bequest_share,
                            life_exp_65   = life_exp_65,
                            life_exp_100  = life_exp_100)) |> 
    unnest(retirement)
}

# summary statistics of avg_pension and inheritance for one scenario
summarize_scenario <- function(pensions) {
  
  pensions |> 
    pivot_longer(
      cols      = c(avg_pension, inheritance), 
      names_to  = "outcome", 
      values_to = "value"
    ) |> 
    summarise(
      min    = min(value),
      q1     = quantile(value, 0.25),
      median = median(value),
      mean   = mean(value),
      q3     = quantile(value, 0.75),
      max    = max(value),
      sd     = sd(value),
      .by = outcome
    )
}

# run all scenarios and summarize pension outcomes per scenario,
# optionally restricting the analysis to given window start years
# (a single year, a range, or any vector: 1984, 1950:1985, c(1930, 1960), ...)
summarize_scenarios <- function(accum, scenarios, start_years = NULL) {
  
  if (!is.null(start_years)) {
    accum <- accum |> filter(start_year %in% start_years)
  }
  
  map(scenarios, ~ run_scenario(accum, .x)) |> 
    map(summarize_scenario) |> 
    list_rbind(names_to = "scenario") |> 
    mutate(interest_retire = scenarios[scenario], .after = scenario)
}


# one tradeoff table: the same rate and window selection for every bequest row
# The 0% case uses the 25 -> 1 life-expectancy schedule; positive targets use
# 25 -> 8. This compares spending policies as well as bequest targets.
run_bequest_tradeoff <- function(accum, interest_retire, start_years = NULL,
                                bequest_shares = c(`10%` = 0.10, `20%` = 0.20,
                                                   `30%` = 0.30)) {
  if (!is.null(start_years)) {
    if (length(start_years) == 0 || anyNA(start_years) ||
        !all(start_years %in% accum$start_year)) {
      stop("start_years must select available window start years in accum.")
    }
    accum <- accum |> filter(start_year %in% .env$start_years)
  }

  bind_rows(
    run_scenario(accum, interest_retire, life_exp_100 = 1) |>
      mutate(bequest = "0%"),
    map(bequest_shares, \(b) run_scenario(accum, interest_retire, bequest_share = b)) |>
      list_rbind(names_to = "bequest")
  ) |>
    mutate(interest_retire = .env$interest_retire)
}


# combined pension and inheritance statistics for every scenario x bequest
# start_years: NULL for all windows, a single year, or a vector such as 1960:1985
# Returns the final numeric table, ready for further analysis or tinytable::tt().
summarize_bequest_scenarios <- function(
  accum, 
  scenarios = c(
    safe   = 0.01,   # bank deposits / CDs
    medium = 0.04,
    bold   = 0.07
  ), 
  start_years = NULL,
  bequest_shares = c(`10%` = 0.10, `20%` = 0.20, `30%` = 0.30)
) {
  map(scenarios, \(rate) {
    run_bequest_tradeoff(accum, rate, start_years = start_years,
                         bequest_shares = bequest_shares) |>
      group_by(bequest) |>
      group_modify(~ summarize_scenario(.x)) |>
      ungroup()
  }) |>
    list_rbind(names_to = "scenario") |>
    select(outcome, scenario, bequest, min, median, mean, max, sd) |>
    mutate(outcome = ifelse(outcome == "avg_pension", "Pension", "Inheritance")) |>
    arrange(desc(outcome))
}


# analysis -------------------------------------------------------------------

# Allow investment-synthetic.r to load the model without running these tables.
if (!exists("investment_functions_only", inherits = FALSE) ||
    !isTRUE(investment_functions_only)) {

# data (downloaded once, shared by all scenarios)
cpi_annual <- get_cpi()
sp500_summary <- get_sp500() |>
  add_real_growth(cpi_annual)

# work period (identical across retirement scenarios): wealth at 65 per window
# The benchmark is gross of fees. Apply this fee once, including to reinvested
# dividends. Actual fund returns already net of expenses would require fee = 0.
annual_fee <- 0.002       # 0.2% per year; adjustable for the fund being modeled
accum <- accumulate(sp500_summary, fee = annual_fee)

# Audit the effect of dividends using exactly the same saving, fees and CPI.
accum_price_only <- sp500_summary |>
  mutate(real_growth = (1 + price_growth) / (1 + inflation) - 1) |>
  accumulate(fee = annual_fee)
dividend_comparison <- accum |>
  rename(wealth_65_total_return = wealth_65) |>
  left_join(rename(accum_price_only, wealth_65_price_only = wealth_65), by = "start_year") |>
  mutate(dividend_reinvestment_gain = wealth_65_total_return - wealth_65_price_only)

dividend_comparison |> filter(start_year %in% c(1930, 1960, 1984, 1985))

# scenarios: real interest rate during the pension period
scenarios <- c(
  `safe (1%)`   = 0.01,   # bank deposits / CDs
  `medium (4%)` = 0.04,
  `bold (7%)`   = 0.07
)

# run all scenarios
scenario_results <- map(scenarios, ~ run_scenario(accum, .x))

# full results: every window x every scenario
pensions <- scenario_results |> 
  list_rbind(names_to = "scenario")

# check out the accumulated wealth
pensions |> filter(start_year > 1980)

# summary statistics of avg_pension and inheritance per scenario
# restrict the windows with start_years, e.g. start_years = 1950:1985
scenario_summary <- summarize_scenarios(accum, scenarios)

scenario_summary

# same table, rounded to the nearest thousand
scenario_summary |> 
  mutate(across(min:sd, ~ round(.x, digits = -3)))

# example: post-1965 windows only
summarize_scenarios(accum, scenarios, start_years = 1965:1985) |> 
  # mutate(across(min:sd, ~ round(.x, digits = -3))) |> 
  select(-c(q1, q3, interest_retire))


# A: scenarios x bequest levels ----------------------------------------------

# each scenario under two bequest targets:
#   "die with zero": smooth version -- life_exp_100 = 1 gives inheritance
#                    exactly 0 under the original rule (lambda = 1)
#   "leave 25%":     bequest target = 25% of wealth at 65 (lambda-solved)
pensions_bequest <- bind_rows(
  map(scenarios, \(r) run_scenario(accum, r, life_exp_100 = 1)) |> 
    list_rbind(names_to = "scenario") |> 
    mutate(bequest = "die with zero"),
  map(scenarios, \(r) run_scenario(accum, r, bequest_share = 0.25)) |> 
    list_rbind(names_to = "scenario") |> 
    mutate(bequest = "leave 25%")
)

pensions_bequest

# summary statistics per bequest target x scenario
summary_A <- pensions_bequest |> 
  group_by(bequest, scenario) |> 
  group_modify(~ summarize_scenario(.x)) |> 
  ungroup() |> 
  mutate(interest_retire = scenarios[scenario], .after = scenario)

summary_A |> 
    select(-c(q1, q3, interest_retire))

# same table, rounded to the nearest thousand
summary_A |> 
  mutate(across(min:sd, ~ round(.x, digits = -3))) |> 
  select(-c(q1, q3, interest_retire))


# B: bequest tradeoff (all scenarios) -----------------------------------------

# NULL = all windows; use start_years = 1960:1985 to match the original
# script's final "post 1960" summaries, or start_years = 1984 for one window.
summary_B_all <- summarize_bequest_scenarios(
  accum, scenarios, start_years = NULL)

summary_B_all |> 
  tinytable::tt(caption = "All years") |> 
  tinytable::format_tt(
    digits = 2,
    num_mark_big = ","
  ) |> 
  print(output = "markdown")

summary_B_pre60 <- summarize_bequest_scenarios(
  accum, scenarios, start_years = 1930:1959)

summary_B_pre60 |> 
  tinytable::tt(caption = "1930s-1950s") |> 
  tinytable::format_tt(
    digits = 2,
    num_mark_big = ","
  ) |> 
  print(output = "markdown")

summary_B_post60 <- summarize_bequest_scenarios(
  accum, scenarios, start_years = 1960:1985)

summary_B_post60 |> 
  tinytable::tt(caption = "Post 1960s") |> 
  tinytable::format_tt(
    digits = 2,
    num_mark_big = ","
  ) |> 
  print(output = "markdown")
}
