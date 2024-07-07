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

#Inland Empire RevPAR
# Load the dataset
Inland <- read_excel("InlandEmpirePredict1.xlsx")

#Convert year-month
Inland$Period <- as.yearmon(Inland$Period, "%b %Y")
Inland$RevPAR <- as.numeric(gsub("\\$", "", Inland$RevPAR))
Inland <- Inland[order(Inland$Period),]
all_periods <- seq(from = as.yearmon("1987-01"), to = as.yearmon("2024-04"), by = 1/12)
complete_data <- merge(data.frame(Period = all_periods), Inland, by = "Period", all.x = TRUE)
#Based on Covid influence, after test. Decide delet 2020-2021 data
complete_data$RevPAR <- na.spline(complete_data$RevPAR)

RevPAR_ts <- ts(complete_data$RevPAR, start = c(1987, 1), frequency = 12)

#ARIMA model
fit <- auto.arima(RevPAR_ts, seasonal = TRUE)
future_forecast <- forecast(fit, h = 12)

# Plot
plot(future_forecast, main = "12-Month Future Forecast for RevPAR", xlab = "Time", ylab = "RevPAR ($)")
points(future_forecast$mean, col="green", pch=19)
text(future_forecast$mean, labels=round(future_forecast$mean, 2), pos=3)

# Seasonality 
seasonal_decomp <- stl(RevPAR_ts, s.window = "periodic")
plot(seasonal_decomp, main="STL Decomposition of RevPAR")

# test accuracy
train_RevPAR <- window(RevPAR_ts, end=c(2023, 10))
test_RevPAR <- window(RevPAR_ts, start=c(2023, 11), end=c(2024, 04))

#model
fit_train <- auto.arima(train_RevPAR, seasonal = TRUE)
test_forecast <- forecast(fit_train, h = 6)

# Plot
plot(test_forecast, main="Forecast vs Actual", ylab="RevPAR ($)")
lines(test_RevPAR, col='green', type='o') 
points(test_forecast$mean, col="purple", pch=19) 
legend("topleft", legend=c("Forecast", "Actual"), col=c("purple", "green"), lty=1, pch=1)

#accuracy
accuracy_metrics <- accuracy(test_forecast, test_RevPAR)
accuracy_metrics

#Inland Empire RevPAU
Inland2 <- read_excel("InlandEmpireRevPAU.xlsx")
revpau_ts <- ts(Inland2$RevPAU, frequency = 4, start = c(2000, 1))  

# ARIMA model
fit <- auto.arima(revpau_ts)
summary(fit)

# Forecast
forecast <- forecast(fit, h = 4)
print(forecast)

# Plot forecast
plot(forecast, main = "Forecasted RevPAU", xlab = "Time", ylab = "RevPAU", col = "blue")
for(i in 1:length(forecast$mean)) {
  text(x = length(revpau_ts) + i, y = forecast$mean[i], labels = round(forecast$mean[i], 2), pos = 3, cex = 1.5) 
}
