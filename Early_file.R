rm(list=ls())

require(quantmod)
require(zoo)
require(ggplot2)
require(tseries)
require(rugarch)
require(PerformanceAnalytics)
require(knitr)
require(MASS)
require(gridExtra)
require(dplyr)
require(knitr)
require(kableExtra)
require(aTSA)
require(FinTS)

set.seed(417628)

symbols <- c("TSLA", "^IXIC", "JPY=X", "HG=F", "ETH-USD")
symbolnames <- c("TSLA", "IXIC", "JPY=X", "HG=F", "ETH-USD")
Symbolnm <- c("Tesla","NASDAQ","JPY/USD","Copper","Ethereum")


start_date <- as.Date("2019-01-01")
end_date <- as.Date("2023-12-31")

getSymbols(symbols, src = "yahoo", from = start_date, to = end_date)
portfolio_data <- do.call(merge, lapply(symbolnames, function(x) Ad(get(x))))
colnames(portfolio_data) <- Symbolnm
portfolio_data <- na.locf(na.locf(portfolio_data, fromLast = TRUE), fromLast = FALSE)
returns <- na.omit(Return.calculate(portfolio_data, method = "log"))

portfolio_returns <- rowMeans(returns)
returns_combined <- cbind(returns, Portfolio = portfolio_returns)

par(mfrow = c(1, 1))
chart.TimeSeries(returns_combined$Portfolio, main = "Portfolio Log Returns", ylab = "Log Returns")

cumulative_returns <- cumprod(1 + returns_combined)

##### Returns combined #####

volatility_data <- returns_combined^2

# Convert the volatility data to a data frame for ggplot
volatility_df <- fortify.zoo(volatility_data, melt = TRUE)
colnames(volatility_df) <- c("Date", "Asset", "Volatility")

# Plot the volatility (squared log returns) for all assets
ggplot(volatility_df, aes(x = Date, y = Volatility, color = Asset)) +
  geom_line(size = 0.5, alpha = 0.75) +
  labs(title = "Volatility (Squared Log Returns) of Portfolio and Components",
       x = "Date",
       y = "Volatility (Squared Log Returns)") +
  scale_color_manual(values = scales::hue_pal(h = c(150, 300), l = 70)(ncol(volatility_data))) +
  theme_minimal(base_size = 15) +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.075, 1),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "gray95", color = "gray80", size = 0.5),
    legend.key = element_rect(fill = "gray95", color = NA),  
    plot.title = element_text(hjust = 0.5),
    panel.grid = element_blank()
  )


##### Descriptive statistics #####
returns_df <- as.data.frame(returns_combined)
summary_stats <- data.frame(
  Mean = round(colMeans(returns_df), 3),
  Median = round(apply(returns_df, 2, median), 3),
  SD = round(apply(returns_df, 2, sd), 3),
  Min = round(apply(returns_df, 2, min), 3),
  `1st_Quartile` = round(apply(returns_df, 2, quantile, probs = 0.25), 3),
  `3rd_Quartile` = round(apply(returns_df, 2, quantile, probs = 0.75), 3),
  Max = round(apply(returns_df, 2, max), 3),
  Skewness = round(apply(returns_df, 2, function(x) mean((x - mean(x))^3 / sd(x)^3)), 3),
  Kurtosis = round(apply(returns_df, 2, function(x) mean((x - mean(x))^4 / sd(x)^4) - 3), 3)
)

# Transpose the summary statistics so that metrics are row names
summary_stats_t <- t(summary_stats)

# Print the table using kableExtra for nice formatting
summary_stats_t %>%
  kable("html", caption = "Summary Statistics of Log Returns for Individual Assets and Portfolio") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = F, 
                font_size = 16) %>%
  row_spec(0, bold = TRUE, color = "white", background = "gray80") %>%
  column_spec(1:ncol(summary_stats_t), width = "2.5cm") %>%  # Adjusts the width of columns
  kable_paper("hover", full_width = F)


# Convert to long format for ggplot
cumulative_returns_df <- fortify.zoo(cumulative_returns, melt = TRUE)
colnames(cumulative_returns_df) <- c("Date", "Asset", "Cumulative_Return")

ggplot(cumulative_returns_df, aes(x = Date, y = Cumulative_Return, color = Asset)) +
  geom_line(size = 1.2, alpha = 0.7) +
  labs(title = "Cumulative Returns of Portfolio and Components",
       x = "Date",
       y = "Cumulative Returns") +
  scale_color_manual(values = scales::hue_pal(h = c(150, 300), l = 70)(ncol(cumulative_returns))) +
  theme_minimal(base_size = 15) +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.075, 1),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "gray95", color = "gray80", size = 0.5),
    legend.key = element_rect(fill = "gray95", color = NA),  
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_line(color = "gray90")
  )

portfolio_returns_df <- data.frame(Returns = portfolio_returns)

# Fit a normal distribution to the data
mu <- mean(portfolio_returns_df$Returns)
sigma <- sd(portfolio_returns_df$Returns)
fit_t <- fitdistr(portfolio_returns_df$Returns, "t")
df_t <- fit_t$estimate["df"]


x_values <- seq(min(portfolio_returns_df$Returns), max(portfolio_returns_df$Returns), length.out = 100)


normal_density <- dnorm(x_values, mean = mu, sd = sigma)
t_density <- dt((x_values - mu) / sigma, df = df_t) / sigma

density_df <- data.frame(
  x_values = x_values,
  normal_density = normal_density,
  t_density = t_density
)

distr <- ggplot(portfolio_returns_df, aes(x = Returns)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50, fill = "lightblue", color = "black", alpha = 0.7) +
  geom_line(data = density_df, aes(x = x_values, y = normal_density, color = "Normal Distribution"), size = 1.2) +
  geom_line(data = density_df, aes(x = x_values, y = t_density, color = "T-Distribution"), size = 1.2, linetype = "dashed") +
  labs(title = "Histogram of Portfolio Log Returns with Normal and T-Distribution",
       x = "Log Returns",
       y = "Density") +
  scale_color_manual(values = c("Normal Distribution" = "lightcoral", "T-Distribution" = "darkblue")) +
  theme_minimal(base_size = 15) +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.05, 0.95),             # Position legend in the top left corner inside the plot
    legend.justification = c(0, 1),              # Align legend to the top left
    legend.background = element_rect(fill = "gray95", color = "gray80", size = 0.5), # Subtle background and border for legend
    legend.key = element_rect(fill = "gray95", color = NA),  # Match legend key background with legend box
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_line(color = "gray90")
  ) 
  #+ annotate("text", x = Inf, y = -Inf, label = "Source: own calculations", 
  #         hjust = 1.1, vjust = -1.5, color = "black", size = 3.5, angle = 0)

##### QQ - Plots #####

# Normal distribution
qq_normal <- ggplot(portfolio_returns_df, aes(sample = Returns)) +
  stat_qq(distribution = qnorm, dparams = list(mean = mu, sd = sigma), size = 1.2, alpha = 0.7) +
  stat_qq_line(distribution = qnorm, dparams = list(mean = mu, sd = sigma), color = "lightcoral", size = 1) +
  labs(title = "QQ Plot: Portfolio Returns vs. Normal Distribution",
       x = "Theoretical Quantiles (Normal)",
       y = "Sample Quantiles") +
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5))

# T-student distribution
qq_t <- ggplot(portfolio_returns_df, aes(sample = Returns)) +
  stat_qq(distribution = qt, dparams = list(df = df_t), size = 1.2, alpha = 0.7) +
  stat_qq_line(distribution = qt, dparams = list(df = df_t), color = "lightblue", size = 1) +
  labs(title = "QQ Plot: Portfolio Returns vs. T-Distribution",
       x = "Theoretical Quantiles (T-Distribution)",
       y = "Sample Quantiles") +
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5))

# Arrange plots in grid
layout_matrix <- rbind(c(1),
                       c(2, 3))

# Arrange the plots using grid.arrange with the layout matrix
grid.arrange(distr, qq_normal, qq_t, layout_matrix = layout_matrix)

##### ACF & PACF #####
arima_model <- arima(portfolio_returns_df, order = c(0, 0, 0))
residuals_arima <- residuals(arima_model)
arch_test_result <- arch.test(arima_model)
print(arch_test_result)

squared_returns <- portfolio_returns_df^2

par(mfrow = c(1, 2))
acf(squared_returns, main = "ACF of Squared Portfolio Log Returns")
pacf(squared_returns, main = "PACF of Squared Portfolio Log Returns")

acf(portfolio_returns_df, main = "ACF of Squared Portfolio Log Returns")
pacf(portfolio_returns_df, main = "PACF of Squared Portfolio Log Returns")

par(mfrow = c(1, 1))


##### Estimating ARMAs #####

run_arma_model <- function(p, q) {
  arma_fit <- arima(portfolio_returns_df, order = c(p, 0, q))
  residuals_arma <- residuals(arma_fit)
  lb_res <- Box.test(residuals_arma, lag = 12, type = "Ljung-Box")$p.value
  acf_res <- sum(abs(acf(residuals_arma, plot = FALSE)$acf[2:12]))
  aic_value <- arma_fit$aic
  
  return(list(p = p, q = q,
              Ljung_Box_Res = lb_res,
              ACF_Res = acf_res,
              AIC = aic_value))
}


# Fit ARMA models and store results
results_list <- list()
for (p in 1:7) {
  for (q in 1:7) {
    result <- run_arma_model(p, q)
    results_list[[length(results_list) + 1]] <- result
  }
}

# Convert the results list to a data frame
results_df <- do.call(rbind, lapply(results_list, as.data.frame))

# Filter results to only include Ljung-Box p-values greater than 0.05
filtered_results_df <- results_df %>%
  filter(Ljung_Box_Res > 0.05) %>%
  arrange(AIC)  # Sort by AIC in ascending order

# Display the filtered and sorted summary table
library(knitr)
library(kableExtra)
filtered_results_df %>%
  kable("html", caption = "Filtered ARMA Models (Ljung-Box p-value > 0.05) Sorted by AIC") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F)



# Convert the results list to a data frame for easy viewing
results_df <- do.call(rbind, lapply(results_list, as.data.frame))

##### Estimating GARCHs #####

run_garch_model <- function(arma_p, arma_q, garch_p, garch_q, solver ='hybrid') {
  garch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(garch_p, garch_q)),
                           mean.model = list(armaOrder = c(arma_p, arma_q), include.mean = TRUE),
                           distribution.model = "std")
  garch_fit <- tryCatch({
    ugarchfit(spec = garch_spec, data = portfolio_returns_df, solver = solver)
  }, error = function(e) {
    message("Error in GARCH fitting: ", e$message)
    return(NULL)  # Return NULL if there's an error
  })
  
  if (is.null(garch_fit)) {
    return(NULL)
  }
  
  residuals_garch <- residuals(garch_fit, standardize = TRUE)
  sigma_garch <- sigma(garch_fit)
  lb_res <- Box.test(residuals_garch, lag = 5, type = "Ljung-Box")$p.value
  lb_res_sq <- Box.test(residuals_garch^2, lag = 5, type = "Ljung-Box")$p.value
  arch_test5 <- ArchTest(residuals_garch, lags = 5)$p.value
  arch_test10 <- ArchTest(residuals_garch, lags = 10)$p.value
  acf_res <- sum(abs(acf(residuals_garch, plot = FALSE)$acf[2:12]))
  acf_res_sq <- sum(abs(acf(residuals_garch^2, plot = FALSE)$acf[2:12]))
  aic_value <- infocriteria(garch_fit)[1]
  
  return(list(arma_p = arma_p, arma_q = arma_q, garch_p = garch_p, garch_q = garch_q,
              Ljung_Box_Res = lb_res,
              Ljung_Box_SqRes = lb_res_sq,
              LM_ARCH_Test5 = arch_test5,
              LM_ARCH_Test10 = arch_test10,
              Conditional_SD = mean(sigma_garch),
              ACF_Res = acf_res,
              ACF_SqRes = acf_res_sq,
              AIC = aic_value))
}


results_list <- list()
for (arma_p in 1:14) {
  for (arma_q in 1:14) {
    for (garch_p in 1:1) {
      for (garch_q in 1:1) {
        result <- tryCatch({
          run_garch_model(arma_p, arma_q, garch_p, garch_q)
        }, error = function(e) {
          message("Error in iteration (", arma_p, ", ", arma_q, ", ", garch_p, ", ", garch_q, "): ", e$message)
          return(NULL)
        })
        if (!is.null(result)) {
          results_list[[length(results_list) + 1]] <- result
        }
        print(list(arma_p, arma_q, garch_p, garch_q))
      }
    }
  }
}

results_df <- do.call(rbind, lapply(results_list, as.data.frame))

print(results_df)



results_df %>%
  kable("html", caption = "Summary of GARCH Models with ARMA(5,5) Mean Model and Various GARCH(p, q) Specifications") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F)

filtered_results_df <- results_df %>%
  select(-ACF_Res, -ACF_SqRes) %>% 
  filter(Ljung_Box_Res > 0.04, Ljung_Box_SqRes > 0.04) %>% 
  arrange(desc(LM_ARCH_Test5)) %>% 
  slice_head(n = 10) 

filtered_results_df %>%
  kable("html", caption = "Filtered GARCH Models (Ljung-Box and LM ARCH Test p-values > 0.05) Sorted by AIC") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F)


##### ACF & PACF of results #####
garch_spec_1 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                           mean.model = list(armaOrder = c(11, 12), include.mean = TRUE),
                           distribution.model = "std")
garch_fit_1 <- ugarchfit(spec = garch_spec_1, data = portfolio_returns_df)
residuals_1 <- residuals(garch_fit_1, standardize = TRUE)
squared_residuals_1 <- residuals_1^2

par(mfrow = c(1, 2)) 

acf(residuals_1, main = "ARMA(4,3) GARCH(3,4) - ACF Residuals", lag.max = 20)
acf(squared_residuals_1, main = "ARMA(4,3) GARCH(3,4) - ACF Squared Residuals", lag.max = 20)




##### Comparing GARCH and E-GARCH #####

garch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                        mean.model = list(armaOrder = c(11, 12), include.mean = TRUE),
                        distribution.model = "std")

garch_fit <- ugarchfit(spec = garch_spec, data = portfolio_returns_df)

egarch_spec <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(2, 4)),
                          mean.model = list(armaOrder = c(11, 12), include.mean = TRUE),
                          distribution.model = "std")

egarch_fit <- ugarchfit(spec = egarch_spec, data = portfolio_returns_df)

news_impact_garch <- newsimpact(garch_fit)
garch_impact_df <- data.frame(Shock = news_impact_garch$zx, Impact = news_impact_garch$zy, Model = "GARCH(2,4)")
news_impact_egarch <- newsimpact(egarch_fit)
egarch_impact_df <- data.frame(Shock = news_impact_egarch$zx, Impact = news_impact_egarch$zy, Model = "EGARCH(2,4)")

news_impact_df <- rbind(garch_impact_df, egarch_impact_df)
ggplot(news_impact_df, aes(x = Shock, y = Impact, color = Model)) +
  geom_line(size = 1.2) +
  labs(title = "News Impact Curve for GARCH(2,4) and EGARCH(2,4)",
       x = "Shocks",
       y = "Volatility Impact") +
  theme_minimal(base_size = 15) +
  scale_color_manual(values = c("GARCH(2,4)" = "blue", "EGARCH(2,4)" = "red")) +
  theme(legend.position = "top", 
        plot.title = element_text(hjust = 0.5))


### Comparison of models' results

comparison_df <- data.frame(
  Parameter = c('mu', 'ar1', 'ar2', 'ar3', 'ar4', 'ar5', 'ar6', 'ar7', 'ar8', 'ar9', 'ar10', 'ar11',
                'ma1', 'ma2', 'ma3', 'ma4', 'ma5', 'ma6', 'ma7', 'ma8', 'ma9', 'ma10', 'ma11', 'ma12',
                'omega', 'alpha1', 'beta1', 'gamma1', 'shape'),
  GARCH_Estimate = c(0.001070, -0.234099, -0.291947, -0.076605, -0.294852, 0.220055, -0.189896, 
                     0.243992, 0.028780, -0.025350, -0.749395, 0.111023, 0.221641, 0.335353, 
                     0.066063, 0.257082, -0.267727, 0.221864, -0.282354, 0.018578, 0.054568, 
                     0.826884, -0.077334, 0.108817, 0.000003, 0.045476, 0.941223, "-", 3.745358),
  GARCH_Pr = c(0.000045, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 
               0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 
               0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 
               0.000000, 0.000000, 0.000000, 0.353840, 0.004908, 0.000000, "-", 0.000000),
  EGARCH_Estimate = c(0.000986, 0.049637, -0.310202, 0.308416, -0.396631, 0.490990, -0.463930, 
                      0.263596, -0.059174, -0.106336, -0.599438, 0.145343, -0.082137, 0.397319, 
                      -0.323442, 0.401387, -0.545752, 0.538121, -0.339334, 0.112619, 0.093098, 
                      0.659524, -0.148167, 0.123726, -0.126006, -0.014848, 0.985210, 0.099959, 3.679854),
  EGARCH_Pr = c(0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 
                0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 
                0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 
                0.00000, 0.25322, 0.00000, 0.00023, 0.00000)
)

nyblom_df <- data.frame(
  Parameter = c('mu', 'ar1', 'ar2', 'ar3', 'ar4', 'ar5', 'ar6', 'ar7', 'ar8', 'ar9', 'ar10', 'ar11',
                'ma1', 'ma2', 'ma3', 'ma4', 'ma5', 'ma6', 'ma7', 'ma8', 'ma9', 'ma10', 'ma11', 'ma12',
                'omega', 'alpha1', 'beta1', 'shape'),
  GARCH = c(0.188729, 0.009644, 0.021881, 0.060705, 0.040124, 0.029027, 0.027414, 0.009470,
            0.015564, 0.026207, 0.042444, 0.030423, 0.035627, 0.016352, 0.069853, 0.018669,
            0.022773, 0.042197, 0.017513, 0.022123, 0.022254, 0.011563, 0.027000, 0.042983,
            1.083351, 0.459599, 0.466493, 0.660119),
  EGARCH = c(0.017881, 0.010208, 0.020270, 0.007628, 0.023122, 0.007936, 0.022901, 0.008508,
             0.022563, 0.007186, 0.023834, 0.007914, 0.024218, 0.007271, 0.023988, 0.007500,
             0.023525, 0.007414, 0.023551, 0.007212, 0.023941, 0.009411, 0.022959, 0.007939,
             0.252480, 0.360863, 0.257954, 0.469246)
)

# Sign Bias Test (GARCH and EGARCH)
sign_bias_df <- data.frame(
  Test = c('Sign Bias', 'Negative Sign Bias', 'Positive Sign Bias', 'Joint Effect'),
  GARCH_t_value = c(0.3242, 2.4584, 0.3976, 6.7986),
  GARCH_prob = c(0.74581, 0.01405, 0.69098, 0.07860),
  GARCH_sig = c('', '**', '', '*'),
  EGARCH_t_value = c(0.3958, 2.1980, 0.9368, 7.0246),
  EGARCH_prob = c(0.69231, 0.02807, 0.34900, 0.07112),
  EGARCH_sig = c('', '**', '', '*')
)

# Adjusted Pearson Goodness-of-Fit Test (GARCH and EGARCH)
pearson_df <- data.frame(
  Group = c(20, 30, 40, 50),
  GARCH_Statistic = c(30.09, 34.42, 51.89, 68.01),
  GARCH_p_value = c(0.05066, 0.22406, 0.08113, 0.03736),
  EGARCH_Statistic = c(35.04, 39.65, 49.70, 64.62),
  EGARCH_p_value = c(0.01380, 0.08979, 0.11717, 0.06664)
)


##### Forecast #####
start_in_sample <- as.Date("2019-01-01")
end_in_sample <- as.Date("2022-12-31")
start_out_sample <- as.Date("2023-01-01")
end_out_sample <- start_out_sample + 364  

in_sample_data <- returns_combined$Portfolio[index(returns_combined$Portfolio) >= start_in_sample & 
                                               index(returns_combined$Portfolio) <= end_in_sample]

out_of_sample_data <- returns_combined$Portfolio[index(returns_combined$Portfolio) >= start_out_sample & 
                                                   index(returns_combined$Portfolio) <= end_out_sample]



calculate_rolling_var <- function(model_spec, in_sample_data, out_of_sample_data, alpha = 0.05, solver = "hybrid") {
  n_out <- nrow(out_of_sample_data)  # Number of points in the out-of-sample period
  window_size <- nrow(in_sample_data)  # Fixed window size from the in-sample data
  VaR_values <- numeric(n_out)  # Store VaR values for each rolling step
  
  # Combine in-sample and out-of-sample data to allow rolling
  combined_data <- rbind(in_sample_data, out_of_sample_data)
  
  for (i in 1:n_out) {
    # Rolling window: select data for current window (in-sample data plus i-th out-of-sample point)
    current_window_data <- combined_data[i:(window_size + i - 1), ]
    
    # Fit the model on the current window
    fit <- tryCatch(
      ugarchfit(spec = model_spec, data = current_window_data, solver = solver),
      error = function(e) {
        message("Error in GARCH fitting at iteration ", i, ": ", e$message)
        return(NA)
      }
    )
    
    if (!is.na(fit)) {
      # Predict the next return's volatility
      forecast <- ugarchforecast(fit, n.ahead = 1)
      
      # Calculate VaR for the forecasted volatility
      sigma_forecast <- sigma(forecast)[1]  # Forecasted volatility
      mu_forecast <- fitted(forecast)[1]    # Forecasted mean
      VaR_values[i] <- mu_forecast + sigma_forecast * qnorm(alpha)  # VaR formula
    } else {
      VaR_values[i] <- NA
    }
  }
  
  return(VaR_values)
}

# 2. Define model specifications for GARCH(2,4) and EGARCH(2,4)


# 3. Calculate VaR for GARCH(2,4) and EGARCH(2,4) using a rolling window approach
VaR_garch <- calculate_rolling_var(garch_spec_1, in_sample_data, out_of_sample_data)
VaR_egarch <- calculate_rolling_var(egarch_spec_1, in_sample_data, out_of_sample_data)

# 4. Extract actual returns from the out-of-sample data
actual_returns <- coredata(out_of_sample_data)

# 5. Calculate VaR violations for GARCH and EGARCH
garch_violations <- sum(actual_returns < VaR_garch) / length(VaR_garch)
egarch_violations <- sum(actual_returns < VaR_egarch) / length(VaR_egarch)

# 6. Create a summary dataframe for comparison
var_comparison <- data.frame(
  Model = c("GARCH(2,4)", "EGARCH(2,4)"),
  VaR_Violations = c(garch_violations, egarch_violations)
)

##### Tests #####
p <- 0.05 # Confidence level (1% VaR)
backN <- length(out_of_sample_data)  # Sample size

# Get VaR exceedances (eta)
eta_garch <- actual_returns < VaR_garch
eta_egarch <- actual_returns < VaR_egarch

# Calculate VaR Violations as percentage
var_violations_garch <- sum(eta_garch) / length(eta_garch) * 100
var_violations_egarch <- sum(eta_egarch) / length(eta_egarch) * 100

# Traffic Light Test (simplified, based on Basel's green/yellow/red criteria)
lights_test <- function(violations, n, alpha) {
  # Basel uses a fixed set of thresholds for green, yellow, and red zones based on expected violations
  expected_violations <- n * alpha
  if (violations < expected_violations + 1.96 * sqrt(expected_violations)) {
    return("Green")
  } else if (violations < expected_violations + 2.58 * sqrt(expected_violations)) {
    return("Yellow")
  } else {
    return("Red")
  }
}

# Apply Traffic Light Test
traffic_light_garch <- lights_test(sum(eta_garch), length(eta_garch), p)
traffic_light_egarch <- lights_test(sum(eta_egarch), length(eta_egarch), p)

### Kupiec's Test (Proportion of Failures Test)
kupiec_test <- function(eta, p) {
  n <- length(eta)
  n1 <- sum(eta)  # Number of VaR exceedances
  n0 <- n - n1    # Number of non-exceedances
  pi <- n1 / n    # Observed exceedance rate
  
  LR_uc <- (p/pi)^n1 * ((1 - p)/(1 - pi))^n0
  stat_uc <- -2 * log(LR_uc)
  prob_uc <- 1 - pchisq(stat_uc, df=1)
  
  return(list(stat_uc = stat_uc, prob_uc = prob_uc))
}

# Apply Kupiec's test
kupiec_garch <- kupiec_test(eta_garch, p)
kupiec_egarch <- kupiec_test(eta_egarch, p)

### Christoffersen's Independence Test
christoffersen_independence_test <- function(eta) {
  eta1 <- eta[-length(eta)]  # eta[t-1]
  eta0 <- eta[-1]            # eta[t]
  
  n00 <- sum(!eta1 & !eta0)  # No exceedance after no exceedance
  n01 <- sum(!eta1 & eta0)   # Exceedance after no exceedance
  n10 <- sum(eta1 & !eta0)   # No exceedance after exceedance
  n11 <- sum(eta1 & eta0)    # Exceedance after exceedance
  
  pi0 <- n01 / (n00 + n01)   # Probability of exceedance after no exceedance
  pi1 <- n11 / (n10 + n11)   # Probability of exceedance after exceedance
  pi  <- (n01 + n11) / (n00 + n01 + n10 + n11) # Overall exceedance probability
  
  LR_ind <- (pi / pi0)^n01 * (pi / pi1)^n11 * ((1 - pi) / (1 - pi0))^n00 * ((1 - pi) / (1 - pi1))^n10
  stat_ind <- -2 * log(LR_ind)
  prob_ind <- 1 - pchisq(stat_ind, df=1)
  
  return(list(stat_ind = stat_ind, prob_ind = prob_ind))
}

# Apply Christoffersen's independence test
christoffersen_ind_garch <- christoffersen_independence_test(eta_garch)
christoffersen_ind_egarch <- christoffersen_independence_test(eta_egarch)

### Christoffersen's Conditional Coverage Test
christoffersen_conditional_coverage_test <- function(stat_uc, stat_ind) {
  stat_cc <- stat_uc + stat_ind
  prob_cc <- 1 - pchisq(stat_cc, df=2)  # Degrees of freedom = 2 for combined test
  
  return(list(stat_cc = stat_cc, prob_cc = prob_cc))
}

# Apply Christoffersen's conditional coverage test
christoffersen_cc_garch <- christoffersen_conditional_coverage_test(kupiec_garch$stat_uc, christoffersen_ind_garch$stat_ind)
christoffersen_cc_egarch <- christoffersen_conditional_coverage_test(kupiec_egarch$stat_uc, christoffersen_ind_egarch$stat_ind)

### Summary table with all results, including Traffic Light Test and VaR Violations (%)
var_comparison_with_tests <- data.frame(
  Model = c("GARCH(2,4)", "EGARCH(2,4)"),
  VaR_Violations_Percent = c(var_violations_garch, var_violations_egarch),
  Traffic_Light = c(traffic_light_garch, traffic_light_egarch),
  Kupiec_Test_Stat = c(kupiec_garch$stat_uc, kupiec_egarch$stat_uc),
  Kupiec_Test_pValue = c(kupiec_garch$prob_uc, kupiec_egarch$prob_uc),
  Christoffersen_I_Stat = c(christoffersen_ind_garch$stat_ind, christoffersen_ind_egarch$stat_ind),
  Christoffersen_I_pValue = c(christoffersen_ind_garch$prob_ind, christoffersen_ind_egarch$prob_ind),
  Christoffersen_II_Stat = c(christoffersen_cc_garch$stat_cc, christoffersen_cc_egarch$stat_cc),
  Christoffersen_II_pValue = c(christoffersen_cc_garch$prob_cc, christoffersen_cc_egarch$prob_cc)
)

# Print the summary table
print(var_comparison_with_tests)

# Optionally, format it as an HTML table using kable (if needed for reporting)
library(knitr)
library(kableExtra)
var_comparison_with_tests %>%
  kable("html", caption = "VaR Violations and Statistical Test Results: GARCH(2,4) vs. EGARCH(2,4)", align = 'c') %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = FALSE)



##### Tests2 #####
alpha <- 0.05

# 1. Kupiec’s Proportion of Failures (POF) Test
kupiec_test <- function(actual_returns, VaR_values, alpha) {
  n <- length(VaR_values)
  failures <- sum(actual_returns < VaR_values)
  pof_stat <- -2 * log(((1 - failures / n)^(n - failures)) * (failures / n)^failures) +
    2 * log(((1 - alpha)^(n - failures)) * alpha^failures)
  p_value <- 1 - pchisq(pof_stat, 1)
  return(p_value)
}

christoffersen_independence_test <- function(actual_returns, VaR_values) {
  hit <- ifelse(actual_returns < VaR_values, 1, 0)
  T00 <- sum(hit[-length(hit)] == 0 & hit[-1] == 0)
  T01 <- sum(hit[-length(hit)] == 0 & hit[-1] == 1)
  T10 <- sum(hit[-length(hit)] == 1 & hit[-1] == 0)
  T11 <- sum(hit[-length(hit)] == 1 & hit[-1] == 1)
  
  p01 <- T01 / (T00 + T01)
  p11 <- T11 / (T10 + T11)
  
  LR_indep <- -2 * log(((1 - p01)^(T00) * p01^T01) * ((1 - p11)^(T10) * p11^T11))
  p_value <- 1 - pchisq(LR_indep, 1)
  return(p_value)
}

# 3. Christoffersen Conditional Coverage Test
christoffersen_conditional_coverage_test <- function(actual_returns, VaR_values, alpha) {
  pof_test <- kupiec_test(actual_returns, VaR_values, alpha)
  indep_test <- christoffersen_independence_test(actual_returns, VaR_values)
  LR_cc <- pof_test + indep_test
  p_value <- 1 - pchisq(LR_cc, 2)
  return(p_value)
}

garch_kupiec <- kupiec_test(actual_returns, VaR_garch, alpha)
egarch_kupiec <- kupiec_test(actual_returns, VaR_egarch, alpha)
garch_independence <- christoffersen_independence_test(actual_returns, VaR_garch)
egarch_independence <- christoffersen_independence_test(actual_returns, VaR_egarch)
garch_conditional_coverage <- christoffersen_conditional_coverage_test(actual_returns, VaR_garch, alpha)
egarch_conditional_coverage <- christoffersen_conditional_coverage_test(actual_returns, VaR_egarch, alpha)
garch_lights <- lights_test(sum(actual_returns < VaR_garch), length(VaR_garch), alpha)
egarch_lights <- lights_test(sum(actual_returns < VaR_egarch), length(VaR_egarch), alpha)


lights_test <- function(violations, n, alpha) {
  # Basel uses a fixed set of thresholds for green, yellow, and red zones based on expected violations
  expected_violations <- n * alpha
  if (violations < expected_violations + 1.96 * sqrt(expected_violations)) {
    return("Green")
  } else if (violations < expected_violations + 2.58 * sqrt(expected_violations)) {
    return("Yellow")
  } else {
    return("Red")
  }
}



# Create the summary dataframe
var_comparison_with_tests <- data.frame(
  Model = c("GARCH(2,4)", "EGARCH(2,4)"),
  VaR_Violations = c(garch_violations, egarch_violations),
  Lights_Test = c(garch_lights, egarch_lights),
  Kupiec_Test_PValue = c(garch_kupiec, egarch_kupiec),
  Christoffersen_I_PValue = c(garch_independence, egarch_independence),
  Christoffersen_II_PValue = c(garch_conditional_coverage, egarch_conditional_coverage)
)
var_comparison_with_tests %>%
  kable("html", caption = "VaR Violations and Statistical Test Results: GARCH(1,1) vs. EGARCH(1,1)", align = 'c') %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = FALSE)

print(var_comparison)


plot_var_with_returns <- function(actual_returns, VaR_values, title, color_line = "red") {
  
  # Extract core data from the xts object for plotting
  data_plot <- data.frame(
    Date = index(actual_returns),  # Dates from the xts index
    Returns = as.numeric(coredata(actual_returns)),  # Ensure numeric Returns
    VaR = VaR_values  # VaR values
  )
  
  # Identify violation points where returns are below VaR
  violation_points <- data_plot$Returns < data_plot$VaR
  
  # Plot the returns, VaR line, and violation points
  ggplot(data_plot, aes(x = Date)) +
    geom_line(aes(y = Returns), color = "black") +  # Plot actual returns in black
    geom_line(aes(y = VaR), color = color_line, linetype = "solid") +  # Plot VaR line in specified color
    geom_point(data = data_plot[violation_points, ], aes(y = Returns), color = "red", shape = 4) +  # Mark violations with red points
    labs(title = title, x = "Date", y = "Log returns") +
    theme_minimal()
}


# Plot 
garch_out_sample_plot <- plot_var_with_returns(out_of_sample_data, VaR_garch, "GARCH(1,1) Out-of-Sample", color_line = 'blue')
egarch_out_sample_plot <- plot_var_with_returns(out_of_sample_data, VaR_egarch, "EGARCH(1,1) Out-of-Sample", color_line = "blue")

# Combine the two in-sample plots side by side
grid.arrange(garch_out_sample_plot, garch_out_sample_plot, ncol = 2)

plot_var_with_two_models <- function(actual_returns, VaR_garch, VaR_egarch, title) {
  
  # Extract core data from the xts object for plotting
  data_plot <- data.frame(
    Date = index(actual_returns),  # Dates from the xts index
    Returns = as.numeric(coredata(actual_returns)),  # Ensure numeric Returns
    VaR_garch = VaR_garch,  # GARCH VaR values
    VaR_egarch = VaR_egarch  # EGARCH VaR values
  )
  
  # Identify violation points for both GARCH and EGARCH where returns are below VaR
  violation_points_garch <- data_plot$Returns < data_plot$VaR_garch
  violation_points_egarch <- data_plot$Returns < data_plot$VaR_egarch
  
  # Plot the returns, VaR lines for both models, and violation points
  ggplot(data_plot, aes(x = Date)) +
    geom_line(aes(y = Returns, color = "Returns"), size = 1) +  # Plot actual returns in black
    geom_line(aes(y = VaR_garch, color = "VaR (GARCH)"), linetype = "solid", size = 1) +  # Plot GARCH VaR line
    geom_line(aes(y = VaR_egarch, color = "VaR (EGARCH)"), linetype = "solid", size = 1) +  # Plot EGARCH VaR line
    geom_point(data = data_plot[violation_points_garch, ], aes(y = Returns, color = "Violations (GARCH)"), shape = 19, size = 4, alpha = 0.5) +  # Bright GARCH violations (big dots)
    geom_point(data = data_plot[violation_points_egarch, ], aes(y = Returns, color = "Violations (EGARCH)"), shape = 19, size = 4, alpha = 0.5) +  # Bright EGARCH violations (big dots)
    labs(title = title, x = "Date", y = "Log returns") +
    theme_minimal() +
    
    # Set custom pastel colors for lines and bright colors for violations
    scale_color_manual(values = c("Returns" = "black", 
                                  "VaR (GARCH)" = "#FF9999",  # Pastel red for GARCH
                                  "VaR (EGARCH)" = "#99CCFF",  # Pastel blue for EGARCH
                                  "Violations (GARCH)" = "#FF0000",  # Bright red for GARCH violations
                                  "Violations (EGARCH)" = "#0000FF"  # Bright blue for EGARCH violations
    )) +
    
    # Customize legend position and background
    theme(legend.position = c(0.85, 0.15),  # Bottom-right placement
          legend.background = element_rect(fill = "gray90", color = "gray70", size = 0.5),  # Subtle gray background
          legend.title = element_blank())  # Remove legend title
}

# Call the function to plot both GARCH and EGARCH on the same graph
combined_var_plot <- plot_var_with_two_models(out_of_sample_data, VaR_garch, VaR_egarch, "GARCH(2,4) and EGARCH(2,4) Out-of-Sample")

# Display the plot
print(combined_var_plot)