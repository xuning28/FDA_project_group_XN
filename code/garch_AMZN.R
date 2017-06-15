## Testing ARCH Effects
library(TSA)
library(xts)
library(xtable)
library(lubridate)
library(readxl)
load("/FDA_project_group_XN/data/AMZN_all.RData")

at = lgreturn-mean(lgreturn)
Box.test(at^2,lag=5,type='Ljung') # perform a Ljung Box test to test ARCH effects

## GARCH Modelling
library(fGarch)

m1 = garchFit(~ garch(1,0), data = lgreturn, cond.dist = "norm") # Fit an ARCH(1) model
summary(m1)
m2 = garchFit(~ garch(1,1), data = lgreturn, cond.dist = "norm") # Fit a GARCH(1,1) model
summary(m2)

## Prediction
sigma2=predict(m2,50)
print(sigma2)
library(sciplot)
plot(sigma2$meanForecast, ylim=c(-0.05,0.05))
lines(c(1:50),sigma2$meanForecast+1.96*sigma2$meanError,col='blue')
lines(c(1:50),sigma2$meanForecast-1.96*sigma2$meanError,col='blue')