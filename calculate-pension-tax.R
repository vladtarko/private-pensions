# Tax and subsidy calculations for the post-1960 synthetic pension scenarios.
# Run from this scripts directory. Requires investment-synthetic.r and its
# reference model in the same folder; downloads historical data on each run.
library(tidyverse)
source("investment-synthetic.r", local = TRUE)

pension_floor <- 25000
bootstrap_reps <- 5000L
bootstrap_seed <- 456L

# Policy 1: a floor on the lifetime-average annual pension. All amounts are
# undiscounted constant-2024-dollar amounts, excluding inheritance.
# Tax only the excess over the lifetime floor, not the entire pension.
person <- cumulative_pensions_post60 |>
  mutate(
    lifetime_floor = pension_floor * retirement_years,
    subsidy = pmax(lifetime_floor - cumulative_pension, 0),
    taxable_excess = pmax(cumulative_pension - lifetime_floor, 0)
  )

tax_rate <- sum(person$subsidy) / sum(person$taxable_excess)
if (!is.finite(tax_rate) || tax_rate > 1) {
  stop("Insufficient above-floor pension income to finance the lifetime floor.")
}
person <- person |>
  mutate(tax = tax_rate * taxable_excess,
         net_lifetime_pension = cumulative_pension + subsidy - tax,
         net_avg_pension = net_lifetime_pension / retirement_years)

lifetime_tax_summary <- person |>
  summarise(
    n_scenarios = n(),
    n_subsidized = sum(subsidy > 0),
    n_taxed = sum(tax > 0),
    total_subsidy = sum(subsidy),
    taxable_excess = sum(taxable_excess),
    tax_rate = tax_rate,
    min_after_tax_average = min(net_avg_pension)
  )
lifetime_tax_summary

# Percentile confidence intervals: resample whole scenarios, not their annual
# pension payments. Conditional on the fitted model; excludes uncertainty in
# historical return parameters and is not a future-population prediction interval.
set.seed(bootstrap_seed)
n <- nrow(person)
indices <- matrix(sample.int(n, n * bootstrap_reps, replace = TRUE), nrow = n)
boot_subsidy <- colSums(matrix(person$subsidy[indices], nrow = n))
boot_excess <- colSums(matrix(person$taxable_excess[indices], nrow = n))
boot_rate <- if_else(boot_excess > 0, boot_subsidy / boot_excess,
                     if_else(boot_subsidy == 0, 0, Inf))
# Empirical quantiles avoid interpolating across an infinite upper endpoint.
tax_confidence_intervals <- tibble(
  metric = c("Lifetime subsidy", "Tax rate on excess"),
  estimate = c(sum(person$subsidy), tax_rate),
  ci_lower = c(quantile(boot_subsidy, .025, type = 1),
               quantile(boot_rate, .025, type = 1)),
  ci_upper = c(quantile(boot_subsidy, .975, type = 1),
               quantile(boot_rate, .975, type = 1))
)
tax_confidence_intervals

# Policy 2: guarantee the floor in EVERY year, regardless of lifetime averages.
# Rates balance the budget separately at each retirement age.
annual <- retirement_paths_post60 |>
  filter(age > 65) |>
  mutate(subsidy = pmax(pension_floor - pension, 0),
         taxable_excess = pmax(pension - pension_floor, 0))

tax_by_age <- annual |>
  summarise(n_scenarios = n(), n_subsidized = sum(subsidy > 0),
            mean_pension = mean(pension), subsidy = sum(subsidy),
            taxable_excess = sum(taxable_excess), .by = age) |>
  mutate(tax_rate = if_else(taxable_excess > 0, subsidy / taxable_excess,
                            if_else(subsidy == 0, 0, Inf)),
         feasible = tax_rate <= 1)

tax_by_age

annual_floor_summary <- annual |>
  summarise(total_subsidy = sum(subsidy),
            taxable_excess = sum(taxable_excess),
            pooled_tax_rate = total_subsidy / taxable_excess)
annual_floor_summary

# The pooled annual-floor rate is only undiscounted lifetime accounting:
# it requires saving early tax surpluses to fund later shortfalls. These 100
# simulations are alternative histories, not independent households sharing
# common market shocks; this is an illustrative redistribution exercise.
stopifnot(abs(sum(person$tax) - sum(person$subsidy)) < 1e-6,
          min(person$net_avg_pension) >= pension_floor - 1e-8)
