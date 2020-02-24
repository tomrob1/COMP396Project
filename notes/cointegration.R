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

#chart_Series(series[[1]])

ggplot() + 
  geom_line(data = fortify.zoo(series[[1]], melt = TRUE), aes(x=Index, y=Value, group=Series)) +
  geom_line(data = fortify.zoo(series[[9]], melt = TRUE), aes(x=Index, y=Value, group=Series)) 

pdtOne <- diff(series[[1]])[-1] 
pdtNine <- diff(series[[9]])[-1]

model <- lm(pdtOne ~ pdtNine -1)
hr <- as.numeric(model$coefficients[1])

spreadT <- pdtOne - hr * pdtNine
meanT <- as.numeric(mean(spreadT, na.rm = TRUE))
sdT <- as.numeric(sd(spreadT, na.rm = TRUE))

upperThr <- meanT + 1 * sdT
lowerThr <- meanT -1 * sdT

plot(spreadT)
abline(h=meanT, col="red", lwd=2)
abline(h=meanT + 1 * sdT, col="blue", lwd=2)
abline(h=meanT -1 * sdT, col="blue", lwd=2)