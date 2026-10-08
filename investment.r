library(tidyverse)
library(tidyquant)
library(collapse)


# S&P 500 index -------------------------------------------------

# Ticker symbol for the S&P 500. Gujarat State Petroleum Corporation.
sp500 <- tq_get("^GSPC", 
                get  = "stock.prices", 
                from = "1984-01-01", 
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
  )

sp500_summary$growth |> mean()
sp500_summary$growth |> sd()

# pak::pak("misrori/getrich")
# download_sp500_hist_data()


# work period ----------------------------------------------------

df1 <- tibble(
  age         = 25:65,
  interest_7  = 0.07,
  interest_10 = 0.10,
  interest_sp = sp500_summary$growth - 0.002,
  save        = 6500,
  wealth_7    = 1000,       # initial inheritance
  wealth_10   = 1000,       #
  wealth_sp   = 1000        #
) 

df1 <- within(df1,{
  
  for (t in 2:length(age)) {
    
    wealth_7[t] = wealth_7[t-1] + wealth_7[t-1] * interest_7[t-1] + save[t-1]
    
    wealth_10[t] = wealth_10[t-1] + wealth_10[t-1] * interest_10[t-1] + save[t-1]
    
    wealth_sp[t] = wealth_sp[t-1] + wealth_sp[t-1] * interest_sp[t-1] + save[t-1]
    
  }
  
})

# alternative to for loop:
  # mutate(
  #   wealth_sp = accumulate2(
  #     .x    = interest_sp[-1],      # -1 to remove first element
  #     .y    = save[-1],             # same here
  #     .init = wealth_sp[1],
  #     
  #     .f = function(w_lag, .x, .y) {
  #       w_lag + w_lag*.x + .y
  #     }
  #   )
  # )


df1 |> 
  ggplot(aes(x = age)) +
  geom_line(aes(y = wealth_7,  color = "7%")) +
  geom_line(aes(y = wealth_10, color = "10%")) +
  geom_line(aes(y = wealth_sp, color = "S&P500")) +
  scale_y_continuous(labels = scales::dollar) +
  labs(
    x = "Age",
    y = "Wealth",
    color = "Annual\ninterest:"
  ) +
  theme_minimal(base_size = 16)


# retirement ----------------------------------------------

df2 <- tibble(
  age      = 65:100,
  wealth   = df1$wealth_10 |> tail(n = 1),
  interest = 0.03,
  pension  = 0,
  life_exp = seq(from = 25, to = 8, length.out = length(age))
) 

df2 <- within(df2, {
  for (i in 2:length(age)) {
    
    wealth[i] = wealth[i-1] + wealth[i-1]*interest[i-1] - pension[i-1]
    
    pension[i] = wealth[i] / life_exp[i]
    
  }  
  }) |> 
  mutate(wealth_sp = NA)

# work + retirement -------------------------------------------

# combine the two periods
df <- bind_rows(
  select(df1, age, wealth = wealth_10, wealth_sp),
  select(df2, age, wealth, wealth_sp)
  )

# plot evolution of wealth
df |> 
  ggplot(aes(x = age)) +
  geom_line(aes(y = wealth), color = "forestgreen") +
  geom_line(aes(y = wealth_sp), color = "royalblue") +
  scale_y_continuous(labels = scales::dollar) +
  labs(
    x = "Age",
    y = "Wealth"
  ) +
  theme_minimal(base_size = 16)

# average pension

df2 |>
  filter(age > 65) |> 
  summary()
