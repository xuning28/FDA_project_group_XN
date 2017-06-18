## Testing ARCH Effects
library(TSA)
library(xts)
library(xtable)
library(lubridate)
library(readxl)
AMZN <- load("F:/CUEB/Junior/SPRING/FDA/FDA_project_group_XN/data/AMZN_all.RData",
                   col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric"))

at = lgreturn-mean(lgreturn)
Box.test(at^2,lag=5,type='Ljung') # perform a Ljung Box test to test ARCH effects

## GARCH Modelling
library(fGarch)

# Normal distributed innovation
m1 = garchFit(~ garch(1,0), data = lgreturn, cond.dist = "norm") 
# Fit an ARCH(1) model
summary(m1)

m2 = garchFit(~ garch(1,1), data = lgreturn, cond.dist = "norm") 
# Fit a GARCH(1,1) model
summary(m2)

# student-t distributed innovation
m11 = garchFit(~ garch(1,0), data = lgreturn, cond.dist = "std") 
# Fit an ARCH(1) model
summary(m11)
m22 = garchFit(~ garch(1,1), data = lgreturn, cond.dist = "std") 
# Fit a GARCH(1,1) model
summary(m22)

## Time plots of the stock price and trading volume
plot(col='blue',y=AMZN$close,x=AMZN$date,xlab='year', ylab='price',type='l',main='Stock Price of AMZN 2010-2016')
plot(col='brown',y=AMZN$volume,x=AMZN$date,xlab='year', ylab='volume',type='l',main='Trading Volume of AMZN 2010-2016')

## Plot the estimated volatility and standardized residuals based on the selected model
vol_1 <- fBasics::volatility(m2)
plot(col='purple',y=vol_1,x=AMZN$date,xlab='year', ylab='volatility of AMZN',type='l',main='Volatility of AMZN 2010-2016')

sres_1 <- residuals(m2, standardize=TRUE)
plot(col='blue',y=sres_1,x=AMZN$date,xlab='year', ylab='Standardized Residuals of AMZN',type='l',main='standardized residuals of AMZN 2010-2016')

## Prediction
load("F:/CUEB/Junior/SPRING/FDA/FDA_project_group_XN/data/AMZN2017.RData")

logreturn2017 = diff(log(AMZN2017$close))*100
print(logreturn2017)
sigma2=predict(m2,114)
print(sigma2)

library(sciplot)
plot(c(1:114),sigma2$meanForecast,ylim=c(-3.0,3.0),type = "l")
lines(c(1:114),logreturn2017,col='red')
lines(c(1:114),sigma2$meanForecast+1.96*sigma2$meanError,col='blue')
lines(c(1:114),sigma2$meanForecast-1.96*sigma2$meanError,col='blue')

