# Technical appendix: investment, pension, and bequest scenarios

**Implementation:** `investment-scenarios-pensions-daily-compound.r`. This appendix describes the dividend-inclusive version, not the earlier price-only scripts. The analysis is a historical accumulation simulation followed by a deterministic retirement simulation; it is not an estimate of an actual pension product.

## 1. Data and coverage

| Input | Source and series | Use |
|---|---|---|
| Daily total-return index | [Yahoo Finance, `^SP500TR`](https://finance.yahoo.com/quote/%5ESP500TR/history/), downloaded with `tidyquant::tq_get()` | Complete annual total returns for **1989–2024**. Includes reinvested dividends. |
| Earlier annual total returns | [Slickcharts, S&P 500 historical returns](https://www.slickcharts.com/sp500/returns) | **1928–1988**. The source explicitly includes reinvested dividends; returns are published to 0.01 percentage-point precision. |
| Daily price index | [Yahoo Finance, `^GSPC`](https://finance.yahoo.com/quote/%5EGSPC/history/) | Price-only comparison for **1928–2024**, not the investment return used in the main simulation. |
| Historical monthly CPI | [Robert Shiller's data](http://www.econ.yale.edu/~shiller/data.htm), [`ie_data.xls`](http://www.econ.yale.edu/~shiller/data/ie_data.xls), sheet `Data` | CPI observations before **2022**. |
| Recent monthly CPI | [BLS public API](https://www.bls.gov/developers/api_signature_v2.htm), [`CUUR0000SA0`, 2022–2024](https://api.bls.gov/publicAPI/v2/timeseries/data/CUUR0000SA0?startyear=2022&endyear=2024) | Actual published CPI-U, US city average, all items, not seasonally adjusted, 1982–84 = 100. Replaces the earlier provisional recent CPI values. |

Yahoo's total-return history begins January 4, 1988. Without the preceding December close, a complete 1988 annual return cannot be calculated from it; 1988 therefore uses the annual source. The script compounds annual returns across this source boundary, rather than attempting to splice index levels. `sp500_summary$return_source` identifies the source for every year.

Before 1957, the historical equity series represents the predecessor index. These early windows are counterfactual index-investment experiences, not returns from a fund that was available to investors at the time.

## 2. Returns, dividends, expenses, and inflation

For daily index levels \(I_d\), annual growth is

\[
1+R_y=\prod_{d\in y}\left(1+\frac{I_d-I_{d-1}}{I_{d-1}}\right)
=\frac{I_{\text{last trading day of }y}}{I_{\text{last trading day of }y-1}}.
\]

The first January return includes the change from the preceding December close. Compounding the **arithmetic mean** daily return is not used: that overstates realized growth when returns vary. Earlier published annual total returns are already compounded returns and enter directly.

Total-return data incorporate dividend reinvestment; dividends are **not added a second time**. The benchmark assumes reinvestment on ex-dividend dates. An actual fund's cash-distribution timing, tracking error, and trading costs can produce small differences. The simulation is before investor-level taxes, broadly corresponding to tax-deferred accumulation; it does not model taxes on eventual withdrawals.

Let \(C_y\) be the arithmetic average of the twelve monthly CPI observations. Annual inflation is \(\pi_y=C_y/C_{y-1}-1\). This is an **annual-average CPI approximation**, rather than December-to-December inflation aligned exactly with the market-return endpoints.

The annual net real growth factor is

\[
G_y=\frac{(1+R_y)e^{-f}}{1+\pi_y},
\qquad f=0.002.
\]

The default 0.2% annual expense ratio is adjustable through `annual_fee`. The factor \(e^{-f}\) approximates continuously accruing fund expenses. It is charged once against the gross benchmark, not subtracted from an already-deflated percentage return. If actual fund returns already net of expenses replace the benchmark, this additional fee must be zero.

## 3. Working-life simulation and dollar units

Each starting year \(s\) uses exactly **40 annual returns**, \(s,\ldots,s+39\), with 41 wealth observations corresponding to ages 25–65. The 97 available return years generate **58 windows**, starting in 1928–1985.

Starting wealth is $1,000 and annual saving is $6,500, contributed at year-end:

\[
A_0=1{,}000,\qquad A_{t+1}=A_tG_{s+t}+6{,}500,
\quad t=0,\ldots,39.
\]

All amounts are in **constant 2024 purchasing-power dollars**. This means the initial wealth and contributions have the same real value in every window; they are not fixed historical nominal dollar amounts. Applying real returns keeps the whole path in those units, so no additional terminal CPI conversion is applied. Contributions do not earn that year's return, and monthly contribution timing is not modeled.

`accum` stores wealth at 65. `dividend_comparison` contrasts total-return and price-only accumulation using identical contributions, fees, and CPI. For earlier years, this comparison also inherits any differences between the historical source series.

## 4. Retirement, payment timing, and targeted inheritance

Retirement rates are constant **real** scenario assumptions: safe **1%**, medium **4%**, and bold **7%**. They are not observed future returns or guaranteed bank yields. Retirement does not replay a subsequent historical market window or model return volatility. The work-period expense ratio is not charged again during retirement; the specified rate is the effective real growth assumption for that phase.

Index retirement observations by \(i=1,\ldots,36\), corresponding to ages 65–100. With \(W_1=A_{40}\), \(P_1=0\), and a life-expectancy divisor \(L_i\) declining linearly from 25 to 8, the implemented recursion is

\[
W_i=(1+r)W_{i-1}-P_{i-1},\qquad
P_i=\lambda W_i/L_i,\quad i=2,\ldots,36.
\]

Reported inheritance is \(B=W_{36}-P_{36}\). Thus the first transition has no withdrawal, pensions otherwise reduce the next balance, and the final pension is deducted immediately from the final balance, without another investment year. In particular, the last transition deducts \(P_{35}\), then the terminal settlement deducts \(P_{36}\). This lagged-payment convention should be retained when reproducing the results; it is not a conventional level-payment annuity.

With no target, \(\lambda=1\) and inheritance is residual wealth. A target can be specified in real dollars or as a share of wealth at 65; if both are supplied, the share takes precedence. Base R's `uniroot()` solves

\[
B=W_1(1+r)
\prod_{i=2}^{35}\left(1+r-\frac{\lambda}{L_i}\right)
\left(1-\frac{\lambda}{L_{36}}\right).
\]

The feasible target range is \(0\leq B\leq W_1(1+r)^{35}\). The numerical tolerance is \(10^{-10}\) **in the withdrawal factor**, not a guaranteed dollar-error tolerance. Life-expectancy divisors below one are rejected.

The **0% bequest rows** use a different divisor schedule, 25 to **1**, with \(\lambda=1\); the final withdrawal then exhausts the balance. Positive percentage targets use the default 25-to-8 schedule and a solved \(\lambda\). These rows therefore compare both bequest targets **and withdrawal schedules**. The divisors are stylized policy parameters, not actuarial survival probabilities; death at 100 is imposed and no mortality weighting is used.

## 5. Summaries and interpretation

For each window, `avg_pension` is the unweighted arithmetic mean of the 35 pensions at ages 66–100. Tables then summarize those window-level averages, and terminal inheritances, across selected starting years. They report minimum, median, mean, maximum, and sample standard deviation; the underlying summary function also computes quartiles using R's default `quantile()` convention. Display rounding does not change the underlying calculations.

Section B reports all starts (1928–1985), starts in **1930–1959**, and starts in **1960–1985**. These are starting-work cohorts, not disjoint calendar-return periods. The first two years, 1928–1929, appear only in the all-years table. Adjacent windows share 39 of 40 return years, so these are dependent historical outcomes, not independent samples.

For fixed retirement assumptions and percentage bequests, pensions and inheritances scale with wealth at 65. Their mean–median ordering consequently repeats the underlying wealth distribution. Moreover, the undiscounted average pension is sensitive to withdrawal timing: delaying spending can raise it through additional compounding, even with a larger bequest. It is neither a present-value measure nor a welfare measure.

## 6. Reproduction and checks

Run from the script directory:

```r
source("investment-scenarios-pensions-daily-compound.r")
# Example: return the full scenario table for selected starting years
summarize_bequest_scenarios(accum, scenarios, start_years = 1960:1985)
```

Dependencies are `tidyverse`, `tidyquant`, `readxl`, `rvest`, `jsonlite`, and `tinytable`, plus their dependencies. Downloads require internet access. The script checks positive index levels, duplicate dates/years, consecutive annual return coverage, twelve valid monthly CPI observations per year, and finite real returns. Overlapping daily and published annual total returns must agree within **2 basis points**; the validation run's maximum difference was **0.53 basis points**.

Additional implementation checks confirmed daily compounding against year-end index ratios, the fee/inflation identity, all 58 windows, and targeted inheritances within one cent. Sources are downloaded live rather than pinned to an archived release; for exact replication, retain the raw downloads, retrieval date, script version, and `sessionInfo()`. The historical data do not establish future return distributions, and retirement-rate scenarios omit longevity and market-return uncertainty.
