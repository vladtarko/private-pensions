library(tidyverse)
library(tidyquant)
library(collapse)
library(readxl)
library(ggrepel)


# S&P 500 index -------------------------------------------------

# Ticker symbol for the S&P 500. Gujarat State Petroleum Corporation.
# Yahoo Finance daily data for ^GSPC starts on 1927-12-30, so asking
# for data from 1900 gives us everything available.
sp500 <- tq_get("^GSPC", 
                get  = "stock.prices", 
                from = "1900-01-01", 
                to   = "2025-01-01")

head(sp500)

sp500 <- sp500 |> 
  mutate(
    year       = year(date),
    growth_day = (adjusted - lag(adjusted)) / lag(adjusted)
  )

sp500_summary <- sp500 |>
  na.omit() |> 
  group_by(year) |> 
  summarise(
    days_in_year = n(),
    value        = mean(adjusted),
    
    # compound growth, from daily to year
    growth       = (1 + mean(growth_day)) ^ days_in_year - 1
  ) |> 
  filter(days_in_year > 100)  # keep only complete years

# annual growth available for 1928-2024
range(sp500_summary$year)

sp500_summary$growth |> mean()
sp500_summary$growth |> sd()


# since 1960

sp500_summary60s <- sp500 |>
  na.omit() |> 
  filter(year > 1959) |> 
  group_by(year) |> 
  summarise(
    days_in_year = n(),
    value        = mean(adjusted),
    
    # compound growth, from daily to year
    growth       = (1 + mean(growth_day)) ^ days_in_year - 1
  ) |> 
  filter(days_in_year > 100)  # keep only complete years

# annual growth available for 1928-2024
range(sp500_summary60s$year)

sp500_summary60s$growth |> mean()
sp500_summary60s$growth |> sd()


# inflation (CPI) --------------------------------------------------

# Robert Shiller's data (Irrational Exuberance): monthly CPI since 1871.
# http://www.econ.yale.edu/~shiller/data.htm
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

# rio::export(cpi, "/home/vladtarko/Dropbox/BACKUP/WORK/social-security/data/shiller-inflation.csv")

# annual CPI = average of the monthly values within the year
cpi_annual <- cpi |> 
  mutate(year = floor(Date)) |> 
  summarise(cpi = mean(CPI), .by = year)

# Shiller's file currently ends in Sept 2023; approximate 2024 CPI with the
# latest available value (CPI was roughly flat over that period) so that 2024
# inflation is computable and all results can be expressed in ~2024 dollars.
# Re-run with an updated Shiller file to make this exact.
cpi_annual <- cpi_annual |> 
  bind_rows(tibble(year = 2024, cpi = 306.1275)) |> 
  arrange(year) |> 
  mutate(inflation = cpi / lag(cpi) - 1)

# add inflation and real (inflation-adjusted) growth to the S&P 500 summary
sp500_summary <- sp500_summary |> 
  left_join(select(cpi_annual, year, inflation), by = "year") |> 
  mutate(real_growth = (1 + growth) / (1 + inflation) - 1)

sp500_summary$real_growth |> mean()
sp500_summary$real_growth |> sd()


# work period ----------------------------------------------------

# NOTE: everything below is in REAL terms (2024 dollars):
#   - wealth accumulates at the real S&P 500 growth rate (minus 0.2% fees)
#   - the $6,500 annual saving is a constant real amount
#   - pensions are in 2024 dollars and directly comparable across windows

# all possible 40-year windows: start years 1928 ... 1985
window_size <- 40
start_years <- min(sp500_summary$year):(max(sp500_summary$year) - window_size + 1)

# wealth progression over ages 25-65 for the 40-year window starting in `start_year`
wealth_path <- function(start_year) {
  
  interest <- sp500_summary |> 
    filter(year >= start_year, year < start_year + window_size) |> 
    pull(real_growth) - 0.002
  
  age    <- 25:65
  save   <- 6500
  wealth <- numeric(length(age))
  wealth[1] <- 1000       # initial inheritance
  
  for (t in 2:length(age)) {
    wealth[t] <- wealth[t-1] + wealth[t-1] * interest[t-1] + save
  }
  
  wealth
}

# merge all wealth progressions in the same dataframe (one variable per window)
df1 <- tibble(age = 25:65)

for (y in start_years) {
  df1[[as.character(y)]] <- wealth_path(y)
}

df1

# longer format for plotting
df1_long <- df1 |> 
  pivot_longer(
    cols      = -age, 
    names_to  = "start_year", 
    values_to = "wealth"
  ) |> 
  mutate(start_year = as.numeric(start_year))

# all windows on a single graph
df1_long |> 
  ggplot(aes(x = age, y = wealth, color = start_year, group = start_year)) +
  geom_line() +
  scale_color_viridis_c() +
  scale_y_continuous(labels = scales::dollar) +
  labs(
    x = "Age",
    y = "Wealth (2024 dollars)",
    color = "Start\nyear:"
  ) +
  theme_minimal(base_size = 16)

# same, faceted by decade of the start year
# (the 1920s and 1980s facets have fewer than 10 windows: 2 and 6)
df1_long |> 
  mutate(decade = paste0(start_year %/% 10 * 10, "s")) |> 
  ggplot(aes(x = age, y = wealth, color = start_year, group = start_year)) +
  geom_line() +
  facet_wrap(~ decade) +
  scale_color_viridis_c() +
  scale_y_continuous(labels = scales::dollar) +
  labs(
    x = "Age",
    y = "Wealth (2024 dollars)",
    color = "Start\nyear:"
  ) +
  theme_minimal(base_size = 16)

# average wealth progression per decade (one line per facet)
df1_long |> 
  mutate(decade = paste0(start_year %/% 10 * 10, "s")) |> 
  summarise(wealth = mean(wealth), .by = c(decade, age)) |> 
  ggplot(aes(x = age, y = wealth)) +
  geom_line(color = "royalblue") +
  facet_wrap(~ decade) +
  scale_y_continuous(labels = scales::dollar) +
  labs(
    x = "Age",
    y = "Wealth (2024 dollars)"
  ) +
  theme_minimal(base_size = 16)

df1_long |> 
  mutate(decade = paste0(start_year %/% 10 * 10, "s")) |> 
  summarise(wealth = mean(wealth), .by = c(decade, age)) |> 
  mutate(decade_lbl = ifelse(age == max(age), decade, NA)) |> 
  ggplot(aes(x = age, y = wealth, color = decade, label = decade_lbl)) +
  geom_line() +
  geom_text_repel(nudge_x = 1, size = 5) +
  scale_y_continuous(labels = scales::dollar, breaks = seq(250000, 2000000, 250000)) +
  labs(
    x = "Age",
    y = "Wealth (in 2024 dollars)",
    title = "Growth of wealth",
    subtitle = "based on different starting work years, saving $6500/yr (2024 dollars) and investing in the S&P 500 index"
  ) +
  theme_minimal(base_size = 16) +
  theme(legend.position = "none")


# retirement ----------------------------------------------

# pension over ages 66-100 and inheritance (wealth left at age 100),
# given wealth at age 65
# wealth compounds at 1% real interest (safe assets, e.g. bank deposits/CDs);
# all values are in 2024 dollars
#
# NOTE on timing convention: the age-100 pension is computed but never
# subtracted, so "inheritance" here is wealth at 100 BEFORE the final
# pension is paid. Two implications: (1) life_exp cannot be extended to
# life_exp_100 = 0 (division by zero, wealth turns negative); (2) this model
# cannot "die with zero" -- for that, and for bequest targets in general,
# use investment-scenarios-pensions.r, where the age-100 pension is paid
# out first (inheritances there are lower by a factor of 1 - 1/8 = 7/8).
retirement <- function(wealth_65) {
  
  age      <- 65:100
  interest <- 0.01
  life_exp <- seq(from = 25, to = 8, length.out = length(age))
  wealth   <- numeric(length(age))
  pension  <- numeric(length(age))
  wealth[1] <- wealth_65
  
  for (i in 2:length(age)) {
    wealth[i]  <- wealth[i-1] + wealth[i-1]*interest - pension[i-1]
    pension[i] <- wealth[i] / life_exp[i]
  }
  
  tibble(
    avg_pension  = mean(pension[age > 65]),
    inheritance  = tail(wealth, n = 1)
  )
}

# redo retirement for each window: average pension and inheritance per time series
pensions <- tibble(
  start_year = start_years,
  wealth_65  = unlist(df1[nrow(df1), -1])
) |> 
  mutate(retirement = map(wealth_65, retirement)) |> 
  unnest(retirement)

pensions |> view()

# summary statistics of the average pensions
pensions |> 
  summarise(
    min    = min(avg_pension),
    q1     = quantile(avg_pension, 0.25),
    median = median(avg_pension),
    mean   = mean(avg_pension),
    q3     = quantile(avg_pension, 0.75),
    max    = max(avg_pension),
    sd     = sd(avg_pension)
  )

# summary statistics of the inheritances
pensions |> 
  summarise(
    min    = min(inheritance),
    q1     = quantile(inheritance, 0.25),
    median = median(inheritance),
    mean   = mean(inheritance),
    q3     = quantile(inheritance, 0.75),
    max    = max(inheritance),
    sd     = sd(inheritance)
  )

# best and worst historical windows
pensions |> slice_min(avg_pension, n = 5)
pensions |> slice_max(avg_pension, n = 5)

# same table, rounded to the nearest thousand
pensions_rounded <- pensions |> 
  mutate(across(-start_year, ~ round(.x, digits = -3)))

# rio::export(pensions_rounded, "/home/vladtarko/Dropbox/BACKUP/WORK/social-security/data/pensions.csv")


# post 1960

# summary statistics of the average pensions
pensions |> 
  filter(start_year > 1959) |> 
  summarise(
    min    = min(avg_pension),
    q1     = quantile(avg_pension, 0.25),
    median = median(avg_pension),
    mean   = mean(avg_pension),
    q3     = quantile(avg_pension, 0.75),
    max    = max(avg_pension),
    sd     = sd(avg_pension)
  )

# summary statistics of the inheritances
pensions |> 
  filter(start_year > 1959) |> 
  summarise(
    min    = min(inheritance),
    q1     = quantile(inheritance, 0.25),
    median = median(inheritance),
    mean   = mean(inheritance),
    q3     = quantile(inheritance, 0.75),
    max    = max(inheritance),
    sd     = sd(inheritance)
  )
