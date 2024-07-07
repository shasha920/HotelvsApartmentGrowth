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

#Inland Empire Linear Regression
ecomonic<-read_excel("Final_ecomonic3.xlsx")
eco<-lm(RevPAR~.,data=ecomonic)
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
eco1<-lm(RevPAU~.,data=ecomonic)
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