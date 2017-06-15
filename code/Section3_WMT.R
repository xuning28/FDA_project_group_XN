## Testing ARCH Effects
library(TSA)
library(xts)
library(xtable)
library(lubridate)
library(readxl)
WMT <- read_excel("F:/CUEB/Junior/SPRING/FDA/FDA_project_group_XN/data/WMT.xlsx",col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric"))
WMT = WMT[-1763:-1764,]
lgreturn = diff(log(WMT$close))*100

at = lgreturn-mean(lgreturn)
Box.test(at^2,lag=5,type='Ljung') # perform a Ljung Box test to test ARCH effects

## GARCH Modelling
library(fGarch)

m1 = garchFit(~ garch(1,0), data = lgreturn, cond.dist = "norm") # Fit an ARCH(1) model
summary(m1)

m2 = garchFit(~ garch(1,1), data = lgreturn, cond.dist = "norm") # Fit a GARCH(1,1) model
summary(m2)