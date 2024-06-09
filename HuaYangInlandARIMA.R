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

Inland<-read_excel("InlandEmpirePredict.xlsx")
#build Simple Linear Regression
fit<-lm(Inland$RevPAR~Inland$TenYear)
summary(fit)

par(mfrow=c(2,2))
plot(fit)


Inland$Period <- as.yearmon(Inland$Period, "%b %Y")
tseries.Inland <- read.zoo(Inland, index.column = 1)
df <- fortify.zoo(tseries.Inland, melt = TRUE)
ggplot(df, aes(x = Index, y = Value)) +
  geom_line() +
  facet_wrap(~ Series, scales = "free_y") +
  xlab("Month of Year") +
  ylab("") +
  ggtitle("Monthly Change in TenYear and RevPAR")

tseries.Inland[,"TenYear"]%>%
  ur.kpss()%>%
  summary()
tseries.Inland[,"RevPAR"]%>%
  ur.kpss()%>%
  summary()

diff_tenyear <- diff(tseries.Inland[, "TenYear"])
diff_revpar <- diff(tseries.Inland[, "RevPAR"])

tenyear_kpss_test_diff <- ur.kpss(diff_tenyear)
summary(tenyear_kpss_test_diff)
revpar_kpss_test_diff <- ur.kpss(diff_revpar)
summary(revpar_kpss_test_diff)

fit <- auto.arima(diff_revpar, xreg = diff_tenyear)
summary(fit)
checkresiduals(fit)

future_tenyear <- diff_tenyear[(length(diff_tenyear) - 11):length(diff_tenyear)]

forecast_revpar <- forecast(fit, xreg = future_tenyear, h = 3)
forecast_revpar

autoplot(forecast_revpar) +
  ggtitle("Forecast of RevPAR with TenYear as external regressor") +
  xlab("Month") + ylab("RevPAR")

