source('framework/data.R')
library(ggplot2)
library(grid)
library(gridExtra)
library(tseries)
library(xts)
library(PairTrading)
######################################################################################################################################
# data
######################################################################################################################################
dataList <- getData("PART1")
dataList2 <- getData("PART2")
dataList3 <- getData("PART3")
series <- lapply(dataList[1:10], function(x) x$Open)
series2 <- lapply(dataList2[1:10], function(x) x$Open)
series3 <- lapply(dataList3[1:10], function(x) x$Open)
modeldata <- cbind(series[[9]], series[[1]])
modeldata2 <- cbind(series2[[9]], series2[[1]])
modeldata3 <- cbind(series3[[9]], series3[[1]])

reg <- EstimateParameters(modeldata, method=lm)
plot(reg$spread)
adf.test(reg$spread)

reg2 <- EstimateParameters(modeldata2, method=lm)
plot(reg2$spread)
adf.test(reg2$spread)

reg3 <- EstimateParameters(modeldata3, method=lm)
plot(reg3$spread)
adf.test(reg3$spread)

allspread <- c(reg$spread, reg2$spread, reg3$spread)
plot(allspread)
adf.test(allspread) 
