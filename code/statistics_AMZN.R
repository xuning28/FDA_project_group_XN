library(xts)
library(xtable)
library(lubridate)
library(readxl)
AMZN <- read_excel("~/Desktop/ThirdYear2/Financial_Data_Analysis/TimeSeries2/FDA_project_group_XN/data/AMZN.xlsx",
col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric"))

# data statistics
AMZN = AMZN[-1763:-1764,]
lgreturn = diff(log(AMZN$close))*100
AMZN = AMZN[-1,]
year = year(AMZN$date)
AMZN = data.frame(AMZN,lgreturn,year)
yearmean = round(aggregate(cbind(AMZN$lgreturn) ~ year, data = AMZN, FUN = "mean"),4)
yearsd = round(aggregate(cbind(AMZN$lgreturn) ~ year, data = AMZN, FUN = "sd"),4)
yearsample = as.numeric(table(year(AMZN$date)))
stat = data.frame(yearmean$year, yearsample, yearmean$V1, yearsd$V1)
stat[8,] = c("2010-2016",sum(yearsample),round(mean(AMZN$lgreturn),4),round(sd(AMZN$lgreturn),4))
names(stat) = c("Year","Sample","Mean(%)","Sd(%)")
xtable(stat,caption = "Descriptive Statistics of AMZN",align = "ccccc",label = NULL)

# time plot
tslgreturn = xts(AMZN$lgreturn,order.by = AMZN$date)
plot((AMZN$lgreturn)/100~AMZN$date, xlab="TIME", ylab="AMZN Log Return", col="blue", type="l")

# acf & pacf
layout(matrix(c(1,2,3,4),nr=2,byrow=T))
acf(lgreturn, main = "(a) Log Return")
pacf(lgreturn, main = "(b) Log Return")
acf(lgreturn^2, main = "(c) Squared Log Return")
pacf(lgreturn^2, main = "(d) Squared Log Return")