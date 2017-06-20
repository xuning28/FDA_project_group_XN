library(xts)
library(xtable)
library(lubridate)
library(readxl)
WMT <- read_excel("../data/WMT.xlsx",
                   col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric"))
# use the relative path

# data statistics
WMT = WMT[-1763:-1764,]
lgreturn = diff(log(WMT$close))*100
WMT = WMT[-1,]
year = year(WMT$date)
WMT = data.frame(WMT,lgreturn,year)
yearmean = round(aggregate(cbind(WMT$lgreturn) ~ year, data = WMT, FUN = "mean"),4)
yearsd = round(aggregate(cbind(WMT$lgreturn) ~ year, data = WMT, FUN = "sd"),4)
yearsample = as.numeric(table(year(WMT$date)))
stat = data.frame(yearmean$year, yearsample, yearmean$V1, yearsd$V1)
stat[8,] = c("2010-2016",sum(yearsample),round(mean(WMT$lgreturn),4),round(sd(WMT$lgreturn),4))
names(stat) = c("Year","Sample","Mean(%)","Sd(%)")
xtable(stat,caption = "Descriptive Statistics of WMT",align = "ccccc",label = NULL)

# time plot
tslgreturn = xts(WMT$lgreturn,order.by = WMT$date)
plot((WMT$lgreturn)/100~WMT$date, xlab="TIME", ylab="WMT Log Return", col="brown", type="l")

# acf & pacf
layout(matrix(c(1,2,3,4),nr=2,byrow=T))
acf(lgreturn, main = "(a) Log Return", ylim = c(-0.07,0.07))
pacf(lgreturn, main = "(b) Log Return")
acf(lgreturn^2, main = "(c) Squared Log Return", ylim = c(-0.08,0.08))
pacf(lgreturn^2, main = "(d) Squared Log Return")