source('framework/data.R')
library(ggplot2)
library(grid)
library(gridExtra)
library(tseries)
library(xts)


######################################################################################################################################
# functions
######################################################################################################################################
zscore <- function(series)
  return (series - mean(series))/sd(series)

rollz <- function(x,window){
  mean = rollapply(x, window, mean)
  std = rollapply(x, window, sd)
  z = (x - mean) / std
  return (z)
}

######################################################################################################################################
# data
######################################################################################################################################
dataList <- getData("PART1")
dataList2 <- getData("PART2")
series <- lapply(dataList[1:10], function(x) x$Close)
series2 <- lapply(dataList2[1:10], function(x) x$Close)

x <- series[[1]]
y <- series[[9]]
x <- rollz(x, 60)
y <- rollz(y, 60)

#Rolling zscore regression?
  model <- lm(x~y)
  hedge <- model$coefficients[2]
  spread <- x - hedge * y
  plot(spread, main="Spread regression with rolling z")
  spread <- na.remove(spread)
  adf.test(spread, main="p < 0.01")

  x <- na.remove(x)
  y <- na.remove(y)
  adf.test(x)
  adf.test(y)

  
#Price ratio
ratios <- series[[9]]/series[[1]]
plot(ratios, main="Part 1 Ratio")
adf.test(ratios)

ratios2 <- series2[[9]]/series2[[1]]
plot(ratios2, main="Part 2 Ratio")
adf.test(ratios2)

zScore <- zscore(ratios)
zScore2 <- zscore(ratios2)
plot(zScore, main="part 1 data")
plot(zScore2, main="part 2 data")

#Feature engineering
  #part 1 data
  bigma <- rollmean(ratios,60) # 60 day ma
  smallma <- rollmean(ratios, 5) # 5 day ma
  sd <- rollapply(ratios, width = 60, FUN = sd, na.rm=TRUE) # 60 day sd
  zed <- (smallma - bigma)/sd 
  plot(zed, main = "Part 1 FEATURE ENG p <0.01")
  zed <- na.remove(zed)
  adf.test(zed)
  
  #part 2 data
  bigma2 <- rollmean(ratios2,60) # 60 day ma
  smallma2 <- rollmean(ratios2, 5) # 5 day ma
  sd2 <- rollapply(ratios2, width = 60, FUN = sd, na.rm=TRUE) # 60 day sd
  zed2 <- (smallma2 - bigma2)/sd2 
  plot(zed2, main = "PArt 2 FEATURE ENG p <0.01", type= 'l')
  zed2 <- na.remove(zed)
  adf.test(zed2)




