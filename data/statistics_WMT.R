library(xts)
library(xtable)
library(lubridate)
library(readxl)
WMT <- read_excel("~/Desktop/ThirdYear2/Financial_Data_Analysis/TimeSeries2/FDA_project_group_XN/data/WMT.xlsx",
                   col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric"))

# data statistics
WMT = WMT[-1763:-1764,]
lgreturn = diff(log(WMT$close))*100
WMT = WMT[-1,]
year = year(WMT$date)
WMT = data.frame(WMT,lgreturn,year)
mean = round(aggregate(cbind(WMT$lgreturn) ~ year, data = WMT, FUN = "mean"),4)
sd = round(aggregate(cbind(WMT$lgreturn) ~ year, data = WMT, FUN = "sd"),4)
sample = as.numeric(table(year(WMT$date)))
stat = data.frame(mean$year, sample, mean$V1, sd$V1)
stat[8,] = c("2010-2016",sum(sample),round(mean(WMT$lgreturn),4),round(sd(WMT$lgreturn),4))
names(stat) = c("Year","Sample","Mean(%)","Sd(%)")
xtable(stat,caption = "Descriptive Statistics of WMT",align = "ccccc",label = NULL)

# time plot
tslgreturn = xts(WMT$lgreturn,order.by = WMT$date)
plot((WMT$lgreturn)/100~WMT$date, xlab="TIME", ylab="Log Return", col="blue", type="l")

# acf & pacf
layout(matrix(c(1,2,3,4),nr=2,byrow=T))
acf(lgreturn, main = "Log Return")
pacf(lgreturn, main = "Log Return")
acf(lgreturn^2, main = "Squared Log Return")
pacf(lgreturn^2, main = "Squared Log Return")