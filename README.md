# Private pension simulations

R scripts for studying retirement wealth, pension income, inheritances, and illustrative pension-floor redistribution. **Start with `investment-scenarios-pensions-daily-compound.r` for the corrected historical analysis.**

## Scripts

| File | Purpose |
|---|---|
| [`investment-scenarios-pensions-daily-compound.r`](investment-scenarios-pensions-daily-compound.r) | Main historical model. Combines dividend-reinvested S&P returns, correct compounding, CPI adjustment, and fund expenses. Runs 40-year accumulation windows and retirement/bequest scenarios; produces full-history, 1930–1959-start, and 1960–1985-start summary tables. |
| [`investment-synthetic.r`](investment-synthetic.r) | Generates 100 reproducible synthetic working-life paths using independent normal annual returns calibrated to the historical real total-return mean and SD. Includes full-history and post-1960 calibrations, lifecycle plots, wealth/pension histograms, and cumulative pension totals grouped around a $25,000 average annual threshold, with scenario-level bootstrap intervals. |
| [`calculate-pension-tax.R`](calculate-pension-tax.R) | Sources the synthetic analysis and calculates taxes on above-floor pension income to finance subsidies. Distinguishes a lifetime-average income floor from a floor in every retirement year; includes bootstrap intervals for the lifetime-floor calculation. |
| [`investment-scenarios-pensions.r`](investment-scenarios-pensions.r) | Earlier function-based scenario model, retained for comparison. Predates the total-return and compounding corrections. |
| [`investment-variability.r`](investment-variability.r) | Earlier rolling-window analysis, with individual and decade-average wealth plots and retirement summaries. |
| [`investment.r`](investment.r) | Original introductory illustration comparing fixed 7%/10% growth with S&P price returns, followed by a retirement example. |

**The three earlier scripts retain a superseded annual-return formula** that compounds the arithmetic mean daily return, rather than the actual daily returns. They also omit dividends; the two inflation-adjusted earlier versions use provisional recent CPI. Their results should not be substituted for the corrected model's estimates.

## Running the analysis

Set the R working directory to this `scripts/` folder. Use a fresh session when switching between historical versions, which define overlapping function and object names.

```r
# Historical scenarios and tables
source("investment-scenarios-pensions-daily-compound.r")
summary_B_post60

# Synthetic paths, plots, and retirement outcomes
source("investment-synthetic.r")
plot_lifecycle_post60
plot_pension_hist_post60
synthetic_pensions_post60

# Redistribution calculations; reruns investment-synthetic.r internally
source("calculate-pension-tax.R")
lifetime_tax_summary
tax_confidence_intervals
tax_by_age
annual_floor_summary
```

Bare plot/table expressions display interactively; after ordinary `source()`, inspect the named objects as above. The scripts create objects in the R session rather than automatically exporting figures or data.

Required packages: `tidyverse`, `tidyquant`, `readxl`, `rvest`, `jsonlite`, `ggrepel`, `scales`, and `tinytable` (plus dependencies). Earlier scripts also load `collapse`. Internet access is needed for live data downloads.

## Current assumptions and sources

- **Working life:** ages 25–65, $1,000 initial wealth, $6,500 saved at each year-end, and a 0.2% annual expense ratio. Corrected-model amounts are in constant **2024 purchasing-power dollars**.
- **Returns:** annual reinvested total returns from Slickcharts for 1928–1988; daily Yahoo `^SP500TR` total returns compounded into annual returns for 1989–2024. Yahoo `^GSPC` supplies the price-only comparison. CPI combines Shiller's historical data with BLS observations for 2022–2024.
- **Retirement:** ages 65–100 with constant real scenario rates of 1%, 4%, or 7%. A withdrawal factor is solved for dollar or percentage bequest targets. The 0% rows use a different life-expectancy schedule from positive-bequest rows. Synthetic retirement defaults to 1% and a 10% bequest.
- **Customization:** `summarize_bequest_scenarios(accum, scenarios, start_years = 1960:1985)` selects historical starting-work cohorts. Synthetic settings and seeds are near the top of `investment-synthetic.r`; the floor and bootstrap settings are near the top of `calculate-pension-tax.R`.

Historical windows overlap, synthetic paths omit common market shocks and volatility clustering, and retirement returns are deterministic. Bootstrap intervals are conditional on the fitted simulation model. Redistribution totals are illustrative accounting exercises, not national policy cost estimates.

## Supporting files

- [`technical-appendix.md`](technical-appendix.md): data-source links, formulas, assumptions, and replication details.
- [`pension-calculation-explained.md`](pension-calculation-explained.md): withdrawal and bequest mathematics; numerical examples predate the corrected return data.
- `investment-scenarios.xlsx`: companion workbook; not required by the R scripts.
