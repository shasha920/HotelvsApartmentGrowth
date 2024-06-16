install.packages("fpp3")
install.packages("urca")
install.packages("tseries")
install.packages("ggplot2")
install.packages("TTR")
install.packages("ggfortify")
library(readxl)
library(fpp3)
library(urca)
library(tseries)
library(quantmod)
library(forecast)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggfortify)
library(forecast)
library(zoo)
options(scipen=999)


# Load the data
#add tenyear to predict
Inland <- read_excel("InlandEmpirePredict1.xlsx")
Inland$Period <- as.yearmon(Inland$Period, "%b %Y")
Inland$RevPAR <- as.numeric(gsub("\\$", "", Inland$RevPAR))
Inland <- Inland[order(Inland$Period),]

all_periods <- seq(from = min(Inland$Period), to = max(Inland$Period), by = 1/12)

complete_data <- merge(data.frame(Period = all_periods), Inland, by = "Period", all.x = TRUE)

complete_data$RevPAR <- na.approx(complete_data$RevPAR, na.rm = FALSE)
complete_data$TenYear <- na.approx(complete_data$TenYear, na.rm = FALSE)

RevPAR_ts <- ts(complete_data$RevPAR, start = c(1987, 1), frequency = 12)
TenYear_ts <- ts(complete_data$TenYear, start = c(1987, 1), frequency = 12)

train_size <- round(length(RevPAR_ts) * 0.67)
train_RevPAR <- window(RevPAR_ts, end = c(1987 + (train_size - 1) %/% 12, (train_size - 1) %% 12 + 1))
train_TenYear <- window(TenYear_ts, end = c(1987 + (train_size - 1) %/% 12, (train_size - 1) %% 12 + 1))
test_RevPAR <- window(RevPAR_ts, start = c(1987 + train_size %/% 12, train_size %% 12 + 1))
test_TenYear <- window(TenYear_ts, start = c(1987 + train_size %/% 12, train_size %% 12 + 1))

fit <- auto.arima(train_RevPAR, xreg = train_TenYear, seasonal = TRUE)

test_predict <- forecast(fit, xreg = test_TenYear, h = length(test_TenYear))

test_mse <- mean((test_RevPAR - test_predict$mean)^2)
test_rmse <- sqrt(test_mse)
test_mae <- mean(abs(test_RevPAR - test_predict$mean))

cat("Test MSE: ", test_mse, "\n")
cat("Test RMSE: ", test_rmse, "\n")
cat("Test MAE: ", test_mae, "\n")

future_TenYear <- rep(mean(complete_data$TenYear, na.rm = TRUE), 3)
future_TenYear <- as.matrix(future_TenYear)
forecast_result <- forecast(fit, xreg = future_TenYear, h = 3)

options(repr.plot.width = 16, repr.plot.height = 8)
plot(forecast_result, main = "RevPAR Forecast with TenYear as Regressor", xlab = "Period", ylab = "RevPAR ($)", cex.main = 1.5, cex.lab = 1.2, cex.axis = 1.2, cex = 1.2)
grid()

positions <- c(3, 3, 3)  
text(x = time(forecast_result$mean), y = forecast_result$mean, labels = round(forecast_result$mean, 2), pos = positions, cex = 1.2, col = "green")

options(repr.plot.width = 16, repr.plot.height = 8)
plot(test_RevPAR, main = "ARIMA Model Predictions vs Actuals", xlab = "Period", ylab = "RevPAR ($)", col = "blue", type = "l")
lines(test_predict$mean, col = "green")
legend("topleft", legend = c("Actual", "Predicted"), col = c("green", "red"), lty = 1, cex = 1.2)
grid()


#without tenyear to predict
Inland <- read_excel("InlandEmpirePredict1.xlsx")
Inland$Period <- as.yearmon(Inland$Period, "%b %Y")
Inland$RevPAR <- as.numeric(gsub("\\$", "", Inland$RevPAR))
Inland <- Inland[order(Inland$Period),]

all_periods <- seq(from = min(Inland$Period), to = max(Inland$Period), by = 1/12)

complete_data <- merge(data.frame(Period = all_periods), Inland, by = "Period", all.x = TRUE)

complete_data$RevPAR <- na.approx(complete_data$RevPAR, na.rm = FALSE)

RevPAR_ts <- ts(complete_data$RevPAR, start = c(1987, 1), frequency = 12)

train_size <- round(length(RevPAR_ts) * 0.67)
train_RevPAR <- window(RevPAR_ts, end = c(1987 + (train_size - 1) %/% 12, (train_size - 1) %% 12 + 1))
test_RevPAR <- window(RevPAR_ts, start = c(1987 + train_size %/% 12, train_size %% 12 + 1))

fit <- auto.arima(train_RevPAR, seasonal = TRUE)

test_predict <- forecast(fit, h = length(test_RevPAR))

test_mse <- mean((test_RevPAR - test_predict$mean)^2)
test_rmse <- sqrt(test_mse)
test_mae <- mean(abs(test_RevPAR - test_predict$mean))

cat("Test MSE: ", test_mse, "\n")
cat("Test RMSE: ", test_rmse, "\n")
cat("Test MAE: ", test_mae, "\n")

forecast_result <- forecast(fit, h = 3)

options(repr.plot.width = 16, repr.plot.height = 8)
plot(forecast_result, main = "RevPAR Forecast", xlab = "Period", ylab = "RevPAR ($)", cex.main = 1.5, cex.lab = 1.2, cex.axis = 1.2, cex = 1.2)
grid()

positions <- c(3, 3, 3)  
text(x = time(forecast_result$mean), y = forecast_result$mean, labels = round(forecast_result$mean, 2), pos = positions, cex = 1.2, col = "green")

options(repr.plot.width = 16, repr.plot.height = 8)
plot(test_RevPAR, main = "ARIMA Model Predictions vs Actuals", xlab = "Period", ylab = "RevPAR ($)", col = "blue", type = "l")
lines(test_predict$mean, col = "green")
legend("topleft", legend = c("Actual", "Predicted"), col = c("green", "red"), lty = 1, cex = 1.2)
grid()
