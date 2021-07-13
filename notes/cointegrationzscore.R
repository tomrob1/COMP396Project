source('framework/data.R')
library(ggplot2)
library(grid)
library(gridExtra)
library(tseries)
library(xts)
##For Diss, split data to show cointegration?
######################################################################################################################################
#functions
######################################################################################################################################
zscore <- function(series)
  return (series - mean(series))/sd(series)

rollz <- function(x,window){
  mean = rollapply(x, window, mean)
  std = rollapply(x, window, sd)
  z = (x - mean) / std
  return (z)
}
# Used in strategy
rollma<- function(x, bigwindow, smallwindow) {
    bigma <- rollmean(x,bigwindow)
    smallma <- rollmean(x,smallwindow)
    sd <- rollapply(x, width=bigwindow, FUN=sd, na.rm=TRUE)
    z <- ((smallma - bigma)/ sd)
    return (z)
}

######################################################################################################################################
# data
######################################################################################################################################
dataList <- getData("PART1")
dataList2 <- getData("PART2")
dataList3 <- getData("PART3")
  series <- lapply(dataList[1:10], function(x) x$Open)
  series2 <- lapply(dataList2[1:10], function(x) x$Open)
  series3 <- lapply(dataList3[1:10], function(x) x$Open)
    drawdown <- 900
    series3drawdown <- lapply(series3, function(x) x[1:drawdown])

######################################################################################################################################
# Plot Pair
######################################################################################################################################
#Part 1
ggplot() + 
  geom_line(data = fortify.zoo(series[[1]], melt = TRUE), aes(x=Index, y=Value, group=Series), color="blue") +
  geom_line(data = fortify.zoo(series[[9]], melt = TRUE), aes(x=Index, y=Value, group=Series), color="red") + 
  ggtitle("Part 1 Series 1 + 9")
#Part 2
ggplot() + 
  geom_line(data = fortify.zoo(series2[[1]], melt = TRUE), aes(x=Index, y=Value, group=Series), color="blue") +
  geom_line(data = fortify.zoo(series2[[9]], melt = TRUE), aes(x=Index, y=Value, group=Series), color="red") + 
  ggtitle("Part 2 Series 1 + 9")
#Part 3
ggplot() + 
  geom_line(data = fortify.zoo(series3[[1]], melt = TRUE), aes(x=Index, y=Value, group=Series), color="blue") +
  geom_line(data = fortify.zoo(series3[[9]], melt = TRUE), aes(x=Index, y=Value, group=Series), color="red") + 
  ggtitle("Part 3 Series 1 + 9")
 #Part 3 Big Drawdown
ggplot() + 
  geom_line(data = fortify.zoo(series3drawdown[[1]], melt = TRUE), aes(x=Index, y=Value, group=Series), color="blue") +
  geom_line(data = fortify.zoo(series3drawdown[[9]], melt = TRUE), aes(x=Index, y=Value, group=Series), color="red") + 
  ggtitle("Part 3 Big Drawdown")
######################################################################################################################################
# Whole Series
######################################################################################################################################
# Both
ggplot() + 
  geom_line(data = fortify.zoo(series[[1]], melt = TRUE), aes(x=Index, y=Value, group=Series), color="blue") +
  geom_line(data = fortify.zoo(series[[9]], melt = TRUE), aes(x=Index, y=Value, group=Series), color="red") + 
  geom_line(data = fortify.zoo(series2[[1]], melt = TRUE), aes(x=Index, y=Value, group=Series), color="blue") +
  geom_line(data = fortify.zoo(series2[[9]], melt = TRUE), aes(x=Index, y=Value, group=Series), color="red") + 
  geom_line(data = fortify.zoo(series3[[1]], melt = TRUE), aes(x=Index, y=Value, group=Series), color="blue") +
  geom_line(data = fortify.zoo(series3[[9]], melt = TRUE), aes(x=Index, y=Value, group=Series), color="red") +
  geom_vline(xintercept = 1100, color = 'black') +
  geom_vline(xintercept = 2200, color = 'black') +
  geom_vline(xintercept = 3100, color = 'black', linetype="dotted") +
  ggtitle("Whole Series 1 + 9")
# Whole Series 1
  ggplot() +
    geom_line(data = fortify.zoo(series[[1]], melt = TRUE), aes(x=Index, y=Value, group=Series), color="blue") +
    geom_line(data = fortify.zoo(series2[[1]], melt = TRUE), aes(x=Index, y=Value, group=Series), color="blue") +
    geom_line(data = fortify.zoo(series3[[1]], melt = TRUE), aes(x=Index, y=Value, group=Series), color="blue") +
    geom_vline(xintercept = 1100, color = 'black') +
    geom_vline(xintercept = 2200, color = 'black') +
    ggtitle("Whole Series 1")
# Whole Series 9
  ggplot() +
    geom_line(data = fortify.zoo(series[[9]], melt = TRUE), aes(x=Index, y=Value, group=Series), color="red") + 
    geom_line(data = fortify.zoo(series2[[9]], melt = TRUE), aes(x=Index, y=Value, group=Series), color="red") + 
    geom_line(data = fortify.zoo(series3[[9]], melt = TRUE), aes(x=Index, y=Value, group=Series), color="red") +
    geom_vline(xintercept = 1100, color = 'black') +
    geom_vline(xintercept = 2200, color = 'black') +
    ggtitle("Whole Series 9")

#Price ratio
ratios <- series[[9]]/series[[1]]
plot(ratios, main="Part 1 Ratio - Series9/Series 1 p-value = 0.6313")
adf.test(ratios) #0.6313
pp.test(ratios) # 0.7829

  #Price ratio
  ratiosTEST <- series[[1]]/series[[9]]
  adf.test(ratiosTEST) #0.6313
  pp.test(ratiosTEST) # 0.7829

ratios2 <- series2[[9]]/series2[[1]]
plot(ratios2, main="Part 2 Ratio - Series9/Series1 p-value = 0.394")
adf.test(ratios2) #0.3494
pp.test(ratios2) #0.3901

ratios3 <- series3[[9]]/series3[[1]]
plot(ratios3, main="Part 3 Ratio")
plot(rollma(ratios3,60,5))
adf.test(ratios3) #0.543
pp.test(ratios3) #0.661 


ratiosALL <- c(ratios,ratios2,ratios3)
adf.test(ratiosALL) #0.4958
pp.test(ratiosALL) #0.5903
plot(ratiosALL)

# Whole Price ratio
ggplot() +
  geom_line(data = ratiosALL, aes(x=Index, y=Open))+
  geom_vline(xintercept = 1100, color = 'blue') +
  geom_vline(xintercept = 2200, color = 'blue') + 
  geom_vline(xintercept = 3100, color = 'black', linetype="dotted") +
  ggtitle("Price Ratio ALL")

#Feature engineering - Roll MA
#part 1 data
zed <- rollma(ratios,60,5)
plot(zed)
ggplot() +
  geom_line(data = fortify.zoo(zed, melt = TRUE), aes(x=Index, y=Value, group=Series), size = 1) + 
  geom_hline(yintercept = 1, color = 'red') +
  geom_hline(yintercept = -1, color = 'red') +
  ggtitle("Part 1 Z-Score") 
#adf.test(na.remove(zed)) # doesnt prove anything

#part 2 data
zed2 <- rollma(ratios2,60,5)
plot(zed2)
#adf.test(na.remove(zed2))# doesnt prove anything

#part 3 data
zed3 <- rollma(ratios3,60,5)
plot(zed3)
#adf.test(na.remove(zed3)) # doesnt prove anything

# Whole Series Zed
ggplot() +
  geom_line(data = fortify.zoo(zed, melt = TRUE), aes(x=Index, y=Value, group=Series)) + 
  geom_line(data = fortify.zoo(zed2, melt = TRUE), aes(x=Index, y=Value, group=Series)) + 
  geom_line(data = fortify.zoo(zed3, melt = TRUE), aes(x=Index, y=Value, group=Series)) +
  geom_hline(yintercept = 1.2, color = 'red', linetype = 'dotted') +
  geom_hline(yintercept = -1.2, color = 'red', linetype = 'dotted') +
  geom_vline(xintercept = 1100, color = 'blue') +
  geom_vline(xintercept = 2200, color = 'blue') +
  geom_vline(xintercept = 3100, color = 'black', linetype="dotted") +
  ggtitle("Whole Series Zed Score")

ggplot() +
  geom_line(data = fortify.zoo(zed3[1:900], melt = TRUE), aes(x=Index, y=Value, group=Series), size =1) +
  geom_hline(yintercept = 1.2, color = 'red', size=1) +
  geom_hline(yintercept = -1.2, color = 'red', size=1) +
  ggtitle("Part 3 Drawdown Z-Score")
    
#####
#Extra  
#####
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

x <- series[[1]]
y <- series[[9]]
x <- rollz(x, 60)
y <- rollz(y, 60)

