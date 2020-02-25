source('framework/data.R')
library(ggplot2)
library(grid)
library(gridExtra)
library(tseries)
library(xts)
######################################################################################################################################
# data
######################################################################################################################################
dataList <- getData("PART1")
dataList2 <- getData("PART2")
series <- lapply(dataList[1:10], function(x) x$Open)
series2 <- lapply(dataList2[1:10], function(x) x$Open)

chartSeries(series[[1]])
            addEMA(n=26, col = "blue")
            addEMA(n=14, col = "red")
            
MACD(series[[1]])