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

#MODEL 1 - Difference in prices day to day
  #Part 1
  diffOnep1 <- diff(series[[1]])[-1] 
  diffNinep1 <- diff(series[[9]])[-1]
  model <- lm(diffOnep1 ~ diffNinep1 -1)
  hr <- as.numeric(model$coefficients[1])
  #Spread
  spreadOnep1 <- diffOnep1 - hr * diffNinep1
  meanT <- as.numeric(mean(spreadOnep1, na.rm = TRUE))
  sdT <- as.numeric(sd(spreadOnep1, na.rm = TRUE))
  z <- (spreadOnep1 - meanT) / sdT
  plot(spreadOnep1, main="Part 1 price difference spread p<0.01")
  #Stationarity test for spread
  adf.test(spreadOnep1)
  
  #PART 2 
  diffOnep2 <- diff(series2[[1]])[-1]
  diffNinep2 <- diff(series2[[9]])[-1]
  modelp2 <- lm (diffOnep2 ~ diffNinep2 -1)
  hr2 <- as.numeric(modelp2$coefficients[1])
  spreadOnep2 <- diffOnep2 - hr2 * diffNinep2
  sdT2 <- as.numeric(sd(spreadOnep2, na.rm = TRUE))
  plot(spreadOnep2, main="Part 2 price difference spread p<0.01")
  #Stationarity test for spread
  adf.test(spreadOnep2)

hist(spreadOnep1, col="blue", breaks=100, main="Spread Histogram Price Difference (1 vs 9) Part1")
hist(spreadOnep2, col="blue", breaks=100, main="Spread Histogram Price Difference(1 vs 9) Part2")
#Once spread exceeds upper threshold, sell 1 buy 9
#Once spread drops below lower threshol, buy 1 sell 9
upperThr <- meanT + 1 * sdT
lowerThr <- meanT -1 * sdT

  
#Model 2 test - PRICES
  #Part 1
  model2 <- lm(series[[1]] ~ series[[9]])
  hedge <- model2$coefficients[2]
  spreadTwop1 <- series[[1]] - hedge*series[[9]]
  meanTest <- as.numeric(mean(spreadTwop1, na.rm = TRUE))
  sdTest <- as.numeric(sd(spreadTwop1, na.rm = TRUE))
  z2 <- (spreadTwop1 - meanTest) / sdTest
  plot(spreadTwop1, main="Part 1 price spread p=0.3258")
  adf.test(spreadTwop1)
  #Part 2
  model2p2 <- lm(series2[[1]] ~ series2[[9]])
  hedge2 <- model2p2$coefficients[2]
  spreadTwop2 <- series2[[1]] - hedge2*series2[[9]]
  plot(spreadTwop2, main="Part 2 price spread p=0.03543")
  adf.test(spreadTwop2)

hist(spreadTwop1, col="blue", breaks=100, main="Spread Histogram PRICES (1 vs 9) Part1")
hist(spreadTwop2, col="blue", breaks=100, main="Spread Histogram PRICES (1 vs 9) Part1")



half_life <- function(series) {
  
  delta_P <- diff(series)
  mu <- mean(series)
  lag_P <- Lag(series) - mu
  model <- lm(delta_P ~ lag_P)
  lambda <- model$coefficients[2]
  H <- -log(2)/lambda
  
  return(H)
}
 
H <- half_life(spreadTwop2)
