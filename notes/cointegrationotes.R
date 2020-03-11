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

s1 <- merge(dataList[1], dataList2[1], all=TRUE)
s1<- s1$Open

s9 <- merge(dataList[9], dataList2[9], all=TRUE)
s9 <- s9$Open

modelOverall <- lm(s1 ~ s9)
hedgeOverall <- modelOverall$coefficients[2]
spreadOverall <- s1 - hedgeOverall*s9
adf.test(spreadOverall)
plot(spreadOverall)


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

  plot(spreadTwop1, main="Part 1 price spread p=0.3258")
  adf.test(spreadTwop1)
  
  z <- (spreadTwop1 - meanTest) / sdTest
  plot(z, main = "part 1 Z score")
  adf.test(z)
  
  #Part 2
  model2p2 <- lm(series2[[1]] ~ series2[[9]])
  hedge2 <- model2p2$coefficients[2]
  spreadTwop2 <- series2[[1]] - hedge2*series2[[9]]
  meanTest2 <- as.numeric(mean(spreadTwop2, na.rm = TRUE))
  sdTest2 <- as.numeric(sd(spreadTwop2, na.rm = TRUE))
  
  z2 <- (spreadTwop2 - meanTest2) / sdTest2
  plot(z2, main = "part 2 z score")
  adf.test(z2)
  
  plot(spreadTwop2, main="Part 2 price spread p=0.03543")
  adf.test(spreadTwop2)
  
  #Feature engineering
  bigma <- rollmean(spreadTwop1,60) # 60 day ma
  smallma <- rollmean(spreadTwop1, 5) # 5 day ma
  sd <- rollapply(spreadTwop1, width = 60, FUN = sd, na.rm=TRUE) # 60 day sd
  zscore <- (smallma - bigma)/sd 
  plot (zscore, main="z Score Prices")





      #Set up rolling linear regression model with lookback set by strategy params
      x <- series[[1]]
      y <- series[[9]]
      model <- roll::roll_lm(x,y,width = 60)
      hedge <- model$coefficients[,2]
      spread <- series[[1]] - hedge * series[[9]]
      mean <- as.numeric(mean(spread, na.rm=TRUE))
      sd <- as.numeric(sd(spread, na.rm=TRUE))
      z <- (spread-mean)/sd
      print(z)

## HALF LIFE OF MEAN REVERSION ##
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
