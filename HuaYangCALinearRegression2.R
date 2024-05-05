install.packages("corrr")
install.packages("ggcorrplot")
install.packages("FactoMineR")
install.packages("factoextra")
install.packages("pls")
install.packages("tidyverse")
install.packages("sandwich")
install.packages("lmtest")
library(lmtest)
library("sandwich")
library("tidyverse")
library("corrr")
library("ggcorrplot")
library("FactoMineR")
library("factoextra")
library("pls")
library(readxl)
library(car)

#cleaning caHostip data
caHostip<-read_excel("CAHospitalityDataGrid.xlsx")
attach(caHostip)
caHostip$Year <- as.integer(substring(caHostip$Period, 5))
caHostip <- caHostip %>%
  filter(Year != 2024)
avgRevPAR <- caHostip %>%
  group_by(Year) %>%
  summarise(avg_RevPAR = mean(RevPAR, na.rm = TRUE))

#cleaning caApartment data
caApartment<-read_excel("CAMultifamilyDataGrid.xlsx")
attach(caApartment)
caApartment$Year <- as.integer(substring(caApartment$Period, 1, 4))
caApartment <- caApartment %>%
  filter(Year != 2024)
avgRevPAU <- caApartment %>%
  group_by(Year) %>%
  summarise(avg_RevPAU = mean(RevPAU, na.rm = TRUE))

#build RevPAR linear regression model
ecomonic<-read_excel("Final_ecomonic2.xlsx")
ecomonic$AvgRevPAR<-avgRevPAR$avg_RevPAR
ecomonic$AvgRevPAU<-avgRevPAU$avg_RevPAU
eco<-lm(AvgRevPAR~.,data=ecomonic)
summary(eco)
# Residual Analysis shows result model good
eco.dv.est <- eco$fitted.values
eco.res.std <- rstudent(eco)
lb <- min(-3, -max(abs(eco.res.std)))
ub <- max(3, max(abs(eco.res.std)))
plot(eco.res.std ~ eco.dv.est, pch = 19, xlab = "Fitted Value of DV",
     ylab = "Standardized Residual", main = "Residual Analysis", ylim = c(lb, ub))
abline(h = -1.5, lty = 3)
abline(h = 1.5, lty = 3)
# bptest shows homoscedasticity fit for regression model
bptest(eco)

#build AvgRevPAU linear regression model
eco1<-lm(AvgRevPAU~.,data=ecomonic)
summary(eco1)
# Residual Analysis shows result model good
eco1.dv.est <- eco$fitted.values
eco1.res.std <- rstudent(eco1)
lb <- min(-3, -max(abs(eco1.res.std)))
ub <- max(3, max(abs(eco1.res.std)))
plot(eco1.res.std ~ eco1.dv.est, pch = 19, xlab = "Fitted Value of DV",
     ylab = "Standardized Residual", main = "Residual Analysis", ylim = c(lb, ub))
abline(h = -1.5, lty = 3)
abline(h = 1.5, lty = 3)
# bptest shows homoscedasticity fit for regression model
bptest(eco1)