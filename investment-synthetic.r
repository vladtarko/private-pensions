# Synthetic investment paths calibrated to historical real S&P total returns.
# Run from this directory so the reference script below can be found.
# All monetary amounts are in constant 2024 purchasing-power dollars.

library(tidyverse)

# Reuse the documented data sources, reinvested dividends, CPI adjustment, and
# pension/bequest rule. Load functions only, without running the reference tables.
model <- new.env(parent = globalenv())
model$investment_functions_only <- TRUE
sys.source("investment-scenarios-pensions-daily-compound.r", envir = model)


# settings -------------------------------------------------------------------

n_scenarios <- 100L
seed <- 123L
initial_wealth <- 1000
annual_saving <- 6500
annual_fee <- 0.002       # same 0.2% work-period expense ratio as the reference model
retirement_interest <- 0.01
bequest_share <- 0.10     # share of EACH simulated path's wealth at age 65


# historical calibration -----------------------------------------------------

sp500_summary <- model$get_sp500() |>
  model$add_real_growth(model$get_cpi())

# Each observation is one calendar year's inflation-adjusted TOTAL return
# (including reinvested dividends), before fund fees. Sample SD uses n - 1.
# Daily gross returns are compounded within each year; we then take the
# arithmetic mean/SD across years to parameterize rnorm() for annual returns.
growth_stats <- sp500_summary |>
  summarise(
    first_year = min(year),
    last_year = max(year),
    years = n(),
    mean_growth = mean(real_growth),
    sd_growth = sd(real_growth)
  )

growth_stats
growth_stats |>
  transmute(first_year, last_year, years,
            mean_return = scales::percent(mean_growth, accuracy = 0.01),
            sd_return = scales::percent(sd_growth, accuracy = 0.01))


# simulation and plotting functions ------------------------------------------

simulate_work_period <- function(n_scenarios, mean_growth, sd_growth,
                                 initial_wealth = 1000, annual_saving = 6500,
                                 annual_fee = 0.002, seed = 123L) {
  set.seed(seed)
  map(seq_len(n_scenarios), function(s) {
    # 40 independent annual returns, giving balances at ages 25 through 65.
    real_return <- rnorm(40L, mean = mean_growth, sd = sd_growth)
    # A normal distribution is unbounded. Do not silently truncate or resample
    # impossible equity returns, since that would change the requested model.
    if (any(real_return <= -1)) {
      stop("A normal draw implies a loss of 100% or more; inspect the simulation assumptions.")
    }
    net_return <- (1 + real_return) * exp(-annual_fee) - 1
    wealth <- purrr::accumulate(
      net_return,
      \(previous, r) previous * (1 + r) + annual_saving,
      .init = initial_wealth
    )
    tibble(
      scenario = s,
      age = 25:65,
      wealth = wealth,
      # A return is recorded against the ENDING age of its investment year.
      real_return = c(NA_real_, real_return),
      net_return = c(NA_real_, net_return)
    )
  }) |>
    list_rbind()
}

plot_wealth_paths <- function(paths, mean_path, title, subtitle, caption) {
  final_age <- max(paths$age)
  age_span <- diff(range(paths$age))

  # Min/max come from individual paths; mean comes from the black mean line.
  # Keep labels NA at every age except the last (65 or 100, depending on plot).
  endpoint_labels <- paths |>
    summarise(Min = min(wealth), Max = max(wealth), .by = age) |>
    pivot_longer(c(Min, Max), names_to = "statistic", values_to = "wealth") |>
    bind_rows(mean_path |> mutate(statistic = "Mean")) |>
    mutate(year_label = if_else(
      age == final_age,
      paste0(statistic, ": ", scales::dollar(wealth, accuracy = 1)),
      NA_character_
    ))

  ggplot(paths, aes(x = age, y = wealth, group = scenario)) +
    geom_line(color = "grey65", linewidth = 0.4, alpha = 0.55) +
    geom_line(data = mean_path, aes(x = age, y = wealth),
              inherit.aes = FALSE, color = "black", linewidth = 1.2) +
    ggrepel::geom_text_repel(
      data = endpoint_labels, aes(x = age, y = wealth, label = year_label),
      inherit.aes = FALSE, na.rm = TRUE,
      nudge_x = age_span * 0.04, direction = "y", hjust = 0,
      min.segment.length = 0, max.overlaps = Inf, seed = 123,
      box.padding = 0.4, size = 7, color = "black"
    ) +
    scale_y_log10(labels = scales::label_dollar()) +
    coord_cartesian(xlim = c(min(paths$age), final_age + age_span * 0.23)) +
    labs(x = "Age", y = "Wealth (2024 dollars)",
         title = title, subtitle = subtitle, caption = caption) +
    theme_minimal(base_size = 24) +
    theme(panel.grid.minor = element_blank(),
          plot.caption = element_text(hjust = 0))
}


# working life: 100 synthetic paths ------------------------------------------

# IID normal returns are a stylized illustration, not a full market-risk model:
# they do not reproduce historical sequencing, fat tails, or volatility clustering.
work_paths <- simulate_work_period(
  n_scenarios = n_scenarios,
  mean_growth = growth_stats$mean_growth,
  sd_growth = growth_stats$sd_growth,
  initial_wealth = initial_wealth,
  annual_saving = annual_saving,
  annual_fee = annual_fee,
  seed = seed
)

# The black line is the arithmetic mean of the 100 SIMULATED wealth balances
# at each age, not a separate deterministic path earning the mean return.
mean_work_path <- work_paths |>
  summarise(wealth = mean(wealth), .by = age)

calibration_label <- paste0(
  "Annual real total returns: mean ",
  scales::percent(growth_stats$mean_growth, accuracy = 0.1),
  ", SD ", scales::percent(growth_stats$sd_growth, accuracy = 0.1),
  " (", growth_stats$first_year, "-", growth_stats$last_year, ")"
)

work_caption <- paste0(
  "Gray: individual paths. Black: mean wealth at each age.\n",
  scales::dollar(initial_wealth), " initially; ",
  scales::dollar(annual_saving), " saved at each year-end; ",
  scales::percent(annual_fee, accuracy = 0.1), " annual fund fee."
)

plot_work <- plot_wealth_paths(
  work_paths, mean_work_path,
  title = paste(n_scenarios, "simulated investment paths"),
  subtitle = calibration_label,
  caption = work_caption
) +
  scale_x_continuous(breaks = seq(25, 65, 10))

plot_work


# working life: calibration using 1960 onward ---------------------------------

# Restrict the HISTORICAL RETURN sample, then simulate new 40-year paths.
# This is not a filter on simulated ages or historical 40-year window starts.
growth_stats_post60 <- sp500_summary |>
  filter(year >= 1960) |>
  summarise(
    first_year = min(year),
    last_year = max(year),
    years = n(),
    mean_growth = mean(real_growth),
    sd_growth = sd(real_growth)
  )

growth_stats_post60 |>
  transmute(first_year, last_year, years,
            mean_return = scales::percent(mean_growth, accuracy = 0.01),
            sd_return = scales::percent(sd_growth, accuracy = 0.01))

# Reset to the same seed: identical underlying standard-normal shocks,
# transformed using the post-1960 mean and SD, isolate the calibration effect.
work_paths_post60 <- simulate_work_period(
  n_scenarios = n_scenarios,
  mean_growth = growth_stats_post60$mean_growth,
  sd_growth = growth_stats_post60$sd_growth,
  initial_wealth = initial_wealth,
  annual_saving = annual_saving,
  annual_fee = annual_fee,
  seed = seed
)

mean_work_path_post60 <- work_paths_post60 |>
  summarise(wealth = mean(wealth), .by = age)

plot_work_post60 <- plot_wealth_paths(
  work_paths_post60, mean_work_path_post60,
  title = paste(n_scenarios, "simulated investment paths: post-1960 calibration"),
  subtitle = paste0(
    "Annual real total returns: mean ",
    scales::percent(growth_stats_post60$mean_growth, accuracy = 0.1),
    ", SD ", scales::percent(growth_stats_post60$sd_growth, accuracy = 0.1),
    " (", growth_stats_post60$first_year, "-", growth_stats_post60$last_year, ")"
  ),
  caption = work_caption
) +
  scale_x_continuous(breaks = seq(25, 65, 10))

plot_work_post60


# retirement: safe return, 10% inheritance -----------------------------------

# Reuse the SAME 100 working-life paths, rather than drawing another sample.
# No additional saving after age 65. Retirement earns a constant 1% real return;
# only the working-life returns are stochastic in this illustration.
retirement_paths <- work_paths |>
  filter(age == 65) |>
  transmute(scenario, wealth_65 = wealth) |>
  mutate(path = map(wealth_65, \(w) model$retirement(
    wealth_65 = w,
    interest = retirement_interest,
    bequest_share = bequest_share,
    life_exp_65 = 25,
    life_exp_100 = 8,
    return_path = TRUE
  ))) |>
  unnest(path)

# Plot remaining wealth AFTER the pension assigned to each retirement age.
# At age 65 that pension is zero; at age 100 this is the actual inheritance.
# Payment timing and the lambda-solved withdrawal rule are those documented in
# technical-appendix.md and used in the reference scenario analysis.
lifecycle_paths <- bind_rows(
  work_paths |> select(scenario, age, wealth),
  retirement_paths |>
    filter(age > 65) |>   # age 65 already appears in work_paths
    transmute(scenario, age, wealth = wealth_after_pension)
)

mean_lifecycle_path <- lifecycle_paths |>
  summarise(wealth = mean(wealth), .by = age)

plot_lifecycle <- plot_wealth_paths(
  lifecycle_paths, mean_lifecycle_path,
  title = "Synthetic investment paths through retirement",
  subtitle = paste0("Retirement at 65: ",
                    scales::percent(retirement_interest, accuracy = 0.1),
                    " real return; bequest at 100 = ",
                    scales::percent(bequest_share, accuracy = 1),
                    " of wealth at 65"),
  caption = paste0("Gray: the same ", n_scenarios,
                   " paths. Black: mean wealth at each age.\n",
                   "Retirement balances are shown after pension withdrawals.")
) +
  geom_vline(xintercept = 65, linetype = "dashed", color = "grey40") +
  scale_x_continuous(breaks = c(seq(25, 95, 10), 100))

plot_lifecycle

# Per-simulation results, for inspecting the pension and terminal target.
synthetic_pensions <- retirement_paths |>
  summarise(
    wealth_65 = dplyr::first(wealth_65),
    avg_pension = mean(pension[age > 65]),
    inheritance = wealth_after_pension[age == 100],
    .by = scenario
  )


# retirement: post-1960 calibration ------------------------------------------

# Continue the existing post-1960 working-life simulations, with no new draws.
# "Post-1960" refers to the 1960-2024 calibration sample, not a simulated
# calendar starting year. Retirement uses the same safe rate and bequest rule.
retirement_paths_post60 <- work_paths_post60 |>
  filter(age == 65) |>
  transmute(scenario, wealth_65 = wealth) |>
  mutate(path = map(wealth_65, \(w) model$retirement(
    wealth_65 = w,
    interest = retirement_interest,
    bequest_share = bequest_share,
    life_exp_65 = 25,
    life_exp_100 = 8,
    return_path = TRUE
  ))) |>
  unnest(path)

synthetic_pensions_post60 <- retirement_paths_post60 |>
  summarise(
    wealth_65 = dplyr::first(wealth_65),
    avg_pension = mean(pension[age > 65]),
    inheritance = wealth_after_pension[age == 100],
    .by = scenario
  )

synthetic_pensions_post60

lifecycle_paths_post60 <- bind_rows(
  work_paths_post60 |> select(scenario, age, wealth),
  retirement_paths_post60 |>
    filter(age > 65) |>
    transmute(scenario, age, wealth = wealth_after_pension)
)

mean_lifecycle_path_post60 <- lifecycle_paths_post60 |>
  summarise(wealth = mean(wealth), .by = age)

plot_lifecycle_post60 <- plot_wealth_paths(
  lifecycle_paths_post60, mean_lifecycle_path_post60,
  title = "Post-1960 investment and retirement paths",
  subtitle = plot_lifecycle$labels$subtitle,
  caption = plot_lifecycle$labels$caption
) +
  geom_vline(xintercept = 65, linetype = "dashed", color = "grey40") +
  scale_x_continuous(breaks = c(seq(25, 95, 10), 100))

plot_lifecycle_post60


# distributions across the post-1960 simulations -----------------------------

# One observation per scenario: do not pool all annual balances or pensions.
# Histograms use linear dollar/count axes; the lifecycle plots keep log wealth.
histogram_subtitle <- paste0(
  n_scenarios, " simulations calibrated to ",
  growth_stats_post60$first_year, "-", growth_stats_post60$last_year,
  " real S&P 500 total returns"
)

plot_wealth65_hist_post60 <- ggplot(synthetic_pensions_post60, aes(x = wealth_65)) +
  geom_histogram(bins = 20, fill = "grey70", color = "white") +
  geom_vline(xintercept = mean(synthetic_pensions_post60$wealth_65),
             color = "black", linewidth = 1) +
  annotate("label", linetype = "dotted",
    x = median(synthetic_pensions_post60$wealth_65) + 1e6,
    y = 26,
    label = paste("Median:", scales::dollar(median(synthetic_pensions_post60$wealth_65), accuracy = 1))
  ) +
  geom_vline(xintercept = median(synthetic_pensions_post60$wealth_65),
             color = "black", linewidth = 1, linetype = "dotted") +
  annotate("label",
    x = mean(synthetic_pensions_post60$wealth_65) + 9e5,
    y = 21,
    label = paste("Mean:", scales::dollar(mean(synthetic_pensions_post60$wealth_65), accuracy = 1))
  ) +
  scale_x_continuous(labels = scales::label_dollar(scale_cut = scales::cut_short_scale())) +
  scale_y_continuous(breaks = scales::breaks_pretty(),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Wealth at retirement: post-1960 calibration",
    subtitle = histogram_subtitle,
    x = "Wealth at age 65 (2024 dollars)",
    y = "Number of scenarios"
    # caption = paste0(
    #   "Dotted line: median = ",
    #   scales::dollar(
    #     median(synthetic_pensions_post60$wealth_65), accuracy = 1),
    #   "; Black line: mean = ",
    #   scales::dollar(
    #     mean(synthetic_pensions_post60$wealth_65), accuracy = 1)
    #   )
  ) +
  theme_minimal(base_size = 24) +
  theme(panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0))

plot_wealth65_hist_post60

plot_pension_hist_post60 <- ggplot(synthetic_pensions_post60, aes(x = avg_pension)) +
  geom_histogram(bins = 20, fill = "grey70", color = "white") +
  geom_vline(xintercept = mean(synthetic_pensions_post60$avg_pension),
             color = "black", linewidth = 1) +
  annotate("label",
    x = mean(synthetic_pensions_post60$avg_pension) + 2.8e4,
    y = 21,
    label = paste("Mean:", scales::dollar(mean(synthetic_pensions_post60$avg_pension), accuracy = 1))
  ) +
  geom_vline(xintercept = median(synthetic_pensions_post60$avg_pension),
             color = "black", linewidth = 1, linetype = "dotted") +
  annotate("label", linetype = "dotted",
    x = median(synthetic_pensions_post60$avg_pension) + 2.8e4,
    y = 26,
    label = paste("Median:", scales::dollar(median(synthetic_pensions_post60$avg_pension), accuracy = 1))
  ) +
  scale_x_continuous(labels = scales::label_dollar(scale_cut = scales::cut_short_scale())) +
  scale_y_continuous(breaks = scales::breaks_pretty(),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Average pension: post-1960 calibration",
    subtitle = histogram_subtitle,
    x = "Average annual pension, ages 66-100 (2024 dollars)",
    y = "Number of scenarios",
    caption = paste0(
      "Safe retirement strategy: ", scales::percent(retirement_interest, accuracy = 0.1),
      " real return; ", scales::percent(bequest_share, accuracy = 1), " bequest.\n"
    )
  ) +
  theme_minimal(base_size = 24) +
  theme(panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0))

plot_pension_hist_post60

# With a fixed retirement rate and percentage bequest, avg_pension is
# proportional to wealth_65, so these distributions have the same shape.


# cumulative pensions by average-pension group -------------------------------

# Classify each scenario using its AVERAGE annual pension, as in the histogram.
# Then sum ALL its pension payments at ages 66-100, including years on either
# side of the threshold. Totals are undiscounted constant-2024-dollar amounts;
# inheritances are not pension payments and are excluded.
cumulative_pensions_by_group <- function(retirement_paths, threshold = 25000,
                                         confidence = 0.95, n_boot = 5000L,
                                         seed = 456L) {
  stopifnot(length(threshold) == 1L, is.finite(threshold), threshold > 0,
            length(confidence) == 1L, is.finite(confidence),
            confidence > 0, confidence < 1,
            length(n_boot) == 1L, is.finite(n_boot),
            n_boot >= 2, n_boot == as.integer(n_boot))

  annual_pensions <- retirement_paths |> filter(age > 65)
  if (nrow(annual_pensions) == 0 || anyNA(annual_pensions$scenario) ||
      any(!is.finite(annual_pensions$pension)) ||
      anyDuplicated(select(annual_pensions, scenario, age))) {
    stop("Provide finite annual pension payments, with one row per scenario and age.")
  }

  group_names <- c(
    paste0("Average pension < ", scales::dollar(threshold, accuracy = 1)),
    paste0("Average pension >= ", scales::dollar(threshold, accuracy = 1))
  )

  per_scenario <- annual_pensions |>
    summarise(
      retirement_years = n(),
      avg_pension = mean(pension),
      cumulative_pension = sum(pension),
      .by = scenario
    ) |>
    arrange(scenario) |>
    mutate(group = if_else(avg_pension < threshold, group_names[1], group_names[2]))

  # Each group total estimates the expected aggregate payment for a cohort of
  # n simulated retirees. Resample n WHOLE scenarios in each bootstrap replicate;
  # group counts can vary. Resampling annual payments would incorrectly treat
  # the 35 payments within a retirement path as independent observations.
  n <- nrow(per_scenario)
  contributions_below <- if_else(per_scenario$avg_pension < threshold,
                                 per_scenario$cumulative_pension, 0)
  contributions_above <- if_else(per_scenario$avg_pension >= threshold,
                                 per_scenario$cumulative_pension, 0)

  set.seed(seed)
  indices <- matrix(sample.int(n, size = n * n_boot, replace = TRUE), nrow = n)
  bootstrap_totals <- tibble(
    below = colSums(matrix(contributions_below[indices], nrow = n)),
    above = colSums(matrix(contributions_above[indices], nrow = n))
  )
  alpha <- (1 - confidence) / 2

  totals <- per_scenario |>
    summarise(
      n_scenarios = n(),
      mean_lifetime_pension = mean(cumulative_pension),
      total_cumulative_pensions = sum(cumulative_pension),
      .by = group
    )

  summary <- tibble(group = group_names) |>
    left_join(totals, by = "group") |>
    mutate(
      n_scenarios = coalesce(n_scenarios, 0L),
      total_cumulative_pensions = coalesce(total_cumulative_pensions, 0),
      ci_lower = c(quantile(bootstrap_totals$below, alpha),
                   quantile(bootstrap_totals$above, alpha)),
      ci_upper = c(quantile(bootstrap_totals$below, 1 - alpha),
                   quantile(bootstrap_totals$above, 1 - alpha)),
      confidence_level = confidence
    ) |>
    select(group, n_scenarios, total_cumulative_pensions, ci_lower, ci_upper,
           mean_lifetime_pension, confidence_level)

  # These are percentile bootstrap confidence intervals for estimated group
  # totals, not a 95% range of individual retirees' outcomes or a predictive
  # interval for a future population. Historical calibration uncertainty is
  # not included. An unobserved group has zero empirical bootstrap totals;
  # this does not prove that its probability is zero in the underlying model.
  list(per_scenario = per_scenario, summary = summary,
       bootstrap_totals = bootstrap_totals)
}

pension_threshold <- 25000
cumulative_pension_results_post60 <- cumulative_pensions_by_group(
  retirement_paths_post60,
  threshold = pension_threshold,
  confidence = 0.95,
  n_boot = 5000L,
  seed = 456L
)

cumulative_pensions_post60 <- cumulative_pension_results_post60$per_scenario
cumulative_pension_summary_post60 <- cumulative_pension_results_post60$summary

cumulative_pension_summary_post60

# Formatted display; retain the numeric table above for subsequent analysis.
cumulative_pension_summary_post60 |>
  mutate(across(c(total_cumulative_pensions, ci_lower, ci_upper, mean_lifetime_pension),
                ~ scales::dollar(.x, accuracy = 1)))
