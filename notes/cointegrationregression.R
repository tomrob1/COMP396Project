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
dataList3 <- getData("PART3")
  series <- lapply(dataList[1:10], function(x) x$Open)
  series2 <- lapply(dataList2[1:10], function(x) x$Open)
  series3 <- lapply(dataList3[1:10], function(x) x$Open)
 
#Model test - PRICES
  #Part 1
  model <- lm(series[[1]] ~ series[[9]])
  hedge <- model$coefficients[2]
  spreadp1 <- series[[1]] - hedge*series[[9]]
  meanTest <- as.numeric(mean(spreadp1, na.rm = TRUE))
  sdTest <- as.numeric(sd(spreadp1, na.rm = TRUE))
    plot(spreadp1, main="Part 1 price spread p=0.3258")
    adf.test(spreadp1)
    z <- (spreadp1 - meanTest) / sdTest
    plot(z, main = "part 1 Z score")
    adf.test(z)

        
  #Part 2
  model2 <- lm(series2[[1]] ~ series2[[9]])
  hedge2 <- model2$coefficients[2]
  spreadp2 <- series2[[1]] - hedge2*series2[[9]]
  meanTest2 <- as.numeric(mean(spreadp2, na.rm = TRUE))
  sdTest2 <- as.numeric(sd(spreadp2, na.rm = TRUE))
    z2 <- (spreadp2 - meanTest2) / sdTest2
    plot(z2, main = "part 2 z score")
    adf.test(z2)
    plot(spreadp2, main="Part 2 price spread p=0.03543")
    adf.test(spreadp2)
    
  #Part 3
  model3 <- lm(series3[[1]] ~ series3[[9]])
  hedge3 <- model3$coefficients[2]
  spreadp3 <- series3[[1]] - hedge3*series3[[9]]
  meanTest3 <- as.numeric(mean(spreadp3, na.rm = TRUE))
  sdTest3 <- as.numeric(sd(spreadp3, na.rm = TRUE))
    z3 <- (spreadp3 - meanTest3) / sdTest3
    plot(z3, main = "part 3 z score p = 0.03661")
    adf.test(z3)
    plot(spreadp3)
    adf.test(spreadp3)
  
  #Feature engineering
  bigma <- rollmean(spreadp1,60) # 60 day ma
  smallma <- rollmean(spreadp1, 5) # 5 day ma
  sd <- rollapply(spreadp1, width = 60, FUN = sd, na.rm=TRUE) # 60 day sd
  zscore <- (smallma - bigma)/sd

  plot (zscore, main="z Score Prices")

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
 