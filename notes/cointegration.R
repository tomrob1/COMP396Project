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

#Part 1
ggplot() + 
  geom_line(data = fortify.zoo(series[[1]], melt = TRUE), aes(x=Index, y=Value, group=Series)) +
  geom_line(data = fortify.zoo(series[[9]], melt = TRUE), aes(x=Index, y=Value, group=Series)) + 
  ggtitle("Part 1 Series 1 + 9")
#Part 2
ggplot() + 
  geom_line(data = fortify.zoo(series2[[1]], melt = TRUE), aes(x=Index, y=Value, group=Series)) +
  geom_line(data = fortify.zoo(series2[[9]], melt = TRUE), aes(x=Index, y=Value, group=Series)) + 
  ggtitle("Part 2 Series 1 + 9")




##
## TEST
##

#Difference in prices month to month
pdtOne <- diff(series[[1]])[-1] 
pdtNine <- diff(series[[9]])[-1]
model <- lm(pdtOne ~ pdtNine -1)
hr <- as.numeric(model$coefficients[1])

#Spread
spreadT <- pdtOne - hr * pdtNine
meanT <- as.numeric(mean(spreadT, na.rm = TRUE))
sdT <- as.numeric(sd(spreadT, na.rm = TRUE))

#Once spread exceeds upper threshold, sell 1 buy 9
#Once spread drops below lower threshol, buy 1 sell 9
upperThr <- meanT + 1 * sdT
lowerThr <- meanT -1 * sdT

plot(spreadT)
abline(h=meanT, col="red", lwd=2)
abline(h=meanT + 1 * sdT, col="blue", lwd=2)
abline(h=meanT -1 * sdT, col="blue", lwd=2)

hist(spreadT, col="blue", breaks=100, main="Spread Histogram (1 vs 9)")
abline(v=meanT, col="red", lwd=2)

#Stationarity test for spread
adf.test(spreadT)



#Model 2 test
model2 <- lm(series[[1]] ~ series[[2]])
hedge <- model2$coefficients[2]
spread <- series[[1]] - hedge*series[[2]]
plot(spread)




