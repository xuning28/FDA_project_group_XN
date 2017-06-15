## Testing ARCH Effects
library(TSA)
library(xts)
library(xtable)
library(lubridate)
library(readxl)
AMZN <- read_excel("F:/CUEB/Junior/SPRING/FDA/FDA_project_group_XN/data/AMZN.xlsx",col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric"))
AMZN = AMZN[-1763:-1764,]
lgreturn = diff(log(AMZN$close))*100

at = lgreturn-mean(lgreturn)
Box.test(at^2,lag=5,type='Ljung') # perform a Ljung Box test to test ARCH effects

## GARCH Modelling
library(fGarch)

m1 = garchFit(~ garch(1,0), data = lgreturn, cond.dist = "norm") # Fit an ARCH(1) model
summary(m1)

m2 = garchFit(~ garch(1,1), data = lgreturn, cond.dist = "norm") # Fit a GARCH(1,1) model
summary(m2)