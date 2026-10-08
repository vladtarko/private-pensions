# Investment scenarios: pensions and inheritances
#
# Same analysis as 11-investment-variability.r, but organized in functions so
# that scenarios are easy to set up. The work period always invests in the
# S&P 500 (real growth, minus fees); scenarios differ only in the REAL
# interest rate earned during the pension (retirement) period. The desired
# inheritance (bequest) is a choice argument: it can be set in dollar terms
# or as a share of wealth at 65 (see pension-calculation-explained.md for
# the math). The default (bequest = NULL) is the original rule, where the
# inheritance is simply whatever wealth is left at age 100.
#
# Pipeline:  get_sp500() / get_cpi()  ->  add_real_growth()
#         ->  accumulate()  (work period, once: calls wealth_path() per window)
#         ->  run_scenario()  (retirement: calls retirement() per window)
#         ->  summarize_scenario()  (stats for one scenario)
#         ->  summarize_scenarios()  (all scenarios, optional start-year filter)
#         ->  summarize_bequest_scenarios()  (combined bequest tradeoff table)
#
# All values are in real terms (2024 dollars).

library(tidyverse)
library(tidyquant)
library(readxl)

# data functions -------------------------------------------------------------

# download S&P 500 daily data and compute annual (nominal) growth
# Yahoo Finance daily data for ^GSPC starts on 1927-12-30, so asking
# for data from 1900 gives us everything available.
get_sp500 <- function(from = "1900-01-01", to = "2025-01-01") {
  
  sp500 <- tq_get("^GSPC", get = "stock.prices", from = from, to = to)
  
  sp500 |> 
    mutate(
      year       = year(date),
      growth_day = (adjusted - lag(adjusted)) / lag(adjusted)
    ) |> 
    na.omit() |> 
    group_by(year) |> 
    summarise(
      days_in_year = n(),
      
      # compound growth, from daily to year
      growth       = (1 + mean(growth_day)) ^ days_in_year - 1,
      .groups      = "drop"
    ) |> 
    filter(days_in_year > 100)  # keep only complete years
}

# download Robert Shiller's CPI data (monthly, since 1871) and compute
# annual inflation. http://www.econ.yale.edu/~shiller/data.htm
get_cpi <- function() {
  
  shiller_file <- tempfile(fileext = ".xls")
  download.file("http://www.econ.yale.edu/~shiller/data/ie_data.xls", 
                destfile = shiller_file, 
                mode     = "wb",
                quiet    = TRUE)
  
  cpi <- read_excel(shiller_file, sheet = "Data", skip = 7) |> 
    select(Date, CPI) |> 
    mutate(
      Date = as.numeric(Date),
      CPI  = as.numeric(CPI)
    ) |> 
    filter(!is.na(Date), !is.na(CPI))
  
  # annual CPI = average of the monthly values within the year
  cpi_annual <- cpi |> 
    mutate(year = floor(Date)) |> 
    summarise(cpi = mean(CPI), .by = year)
  
  # Shiller's file currently ends in Sept 2023; approximate 2024 CPI with the
  # latest available value so results are expressed in ~2024 dollars
  cpi_annual |> 
    bind_rows(tibble(year = 2024, cpi = 306.1275)) |> 
    arrange(year) |> 
    mutate(inflation = cpi / lag(cpi) - 1)
}

# add inflation and real (inflation-adjusted) growth to the S&P 500 summary
add_real_growth <- function(sp500_summary, cpi_annual) {
  
  sp500_summary |> 
    left_join(select(cpi_annual, year, inflation), by = "year") |> 
    mutate(real_growth = (1 + growth) / (1 + inflation) - 1)
}


# simulation functions -------------------------------------------------------

# wealth progression over ages 25-65 for the 40-year window starting in
# `start_year`, investing in the S&P 500 (real growth minus fees)
wealth_path <- function(start_year, sp500_summary, window_size = 40,
                        save = 6500, initial = 1000, fee = 0.002) {
  
  interest <- sp500_summary |> 
    filter(year >= start_year, year < start_year + window_size) |> 
    pull(real_growth) - fee
  
  age    <- 25:65
  wealth <- numeric(length(age))
  wealth[1] <- initial       # initial inheritance
  
  for (t in 2:length(age)) {
    wealth[t] <- wealth[t-1] + wealth[t-1] * interest[t-1] + save
  }
  
  wealth
}

# work period for all 40-year windows: wealth at age 65 per start year
accumulate <- function(sp500_summary, window_size = 40) {
  
  start_years <- min(sp500_summary$year):(max(sp500_summary$year) - window_size + 1)
  
  tibble(
    start_year = start_years,
    wealth_65  = map_dbl(start_years, 
                         ~ tail(wealth_path(.x, sp500_summary, window_size), n = 1))
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
                       life_exp_65 = 25, life_exp_100 = 8) {
  
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

# data (downloaded once, shared by all scenarios)
sp500_summary <- get_sp500() |> 
  add_real_growth(get_cpi())

# work period (identical across scenarios): wealth at 65 per 40-year window
accum <- accumulate(sp500_summary)

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
