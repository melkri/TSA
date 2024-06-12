rm(list=ls())

library(quantmod)
require(zoo)
require(ggplot2)
require(rugarch)
require(PerformanceAnalytics)
require(knitr)

start_date <- as.Date("2019-01-01")
end_date <- as.Date("2023-12-31")

# Retrieve the Data
getSymbols("TSLA", from = start_date, to = end_date)
getSymbols("^IXIC", from = start_date, to = end_date)
getSymbols("JPY=X", from = start_date, to = end_date)
getSymbols("HG=F", from = start_date, to = end_date)
getSymbols("ETH-USD", from = start_date, to = end_date)

# Get specific values
TSLA <- Ad(TSLA)
NASDAQ <- Ad(`IXIC`)
USDJPY <- Ad(`JPY=X`)
Copper <- Ad(`HG=F`)
ETH <- Ad(`ETH-USD`)

log_returns <- function(prices) {
  returns <- diff(log(prices))
  returns <- returns[-1, ] 
  return(returns)
}

returns_TSLA <- log_returns(TSLA)
returns_NASDAQ <- log_returns(NASDAQ)
returns_USDJPY <- log_returns(USDJPY)
returns_Copper <- log_returns(Copper)
returns_ETH <- log_returns(ETH)

returns <- merge(returns_TSLA, returns_NASDAQ, returns_USDJPY, returns_Copper, returns_ETH)
returns <- na.fill(returns, fill = 0)
colnames(returns) <- c("TSLA", "NASDAQ", "USDJPY", "Copper", "ETH")

weights <- rep(0.2, 5)
portfolio_returns <- rowSums(returns * weights)

acf(portfolio_returns, main = "ACF of Portfolio Returns")
pacf(portfolio_returns, main = "PACF of Portfolio Returns")


rolling_garch_egarch <- function(returns, window_size) {
  n <- length(returns)
  garch_results <- zoo(, order.by = index(returns)[(window_size+1):n])
  egarch_results <- zoo(, order.by = index(returns)[(window_size+1):n])
  
  for (i in seq(window_size, n)) {
    window_data <- returns[(i - window_size + 1):i]
    
    # Fit GARCH(1,1)
    spec_garch <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                             mean.model = list(armaOrder = c(1, 1), include.mean = TRUE))
    fit_garch <- ugarchfit(spec = spec_garch, data = window_data, solver = 'hybrid')
    garch_results[i - window_size + 1] <- sigma(fit_garch)[window_size]
    
    # Fit EGARCH(1,1)
    spec_egarch <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
                              mean.model = list(armaOrder = c(1, 1), include.mean = TRUE))
    fit_egarch <- ugarchfit(spec = spec_egarch, data = window_data, solver = 'hybrid')
    egarch_results[i - window_size + 1] <- sigma(fit_egarch)[window_size]
  }
  
  return(list(garch = garch_results, egarch = egarch_results))
}

window_size <- 365
garch_egarch_results <- rolling_garch_egarch(portfolio_returns, window_size)
