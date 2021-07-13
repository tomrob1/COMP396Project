source('framework/data.R')
#source('notes/cointegrationzscore.R')
source('framework/backtester.R')
source('framework/processResults.R'); 
source('framework/utilities.R'); # for backtestAndPlot function
source('strategies/cointegration2.R') 

library(ggplot2)
library(grid)
library(gridExtra)
library(tseries)
library(xts)
library(egcm)
library(urca)
#check 1100 +60 = both series go down together
######################################################################################################################################
# data
######################################################################################################################################
dataList <- getData("PART1")
dataList2 <- getData("PART2")
dataList3 <- getData("PART3")
dataList4 <- getData("PART4") # Part 1 and 2 Merged 2200 records
dataList5 <- getData("PART5") #123 merged 3500 records
  series <- lapply(dataList[1:10], function(x) x$Open)
  series2 <- lapply(dataList2[1:10], function(x) x$Open)
  series3 <- lapply(dataList3[1:10], function(x) x$Open)
  series4 <- lapply(dataList4[1:10], function(x) x$Open)
  seriesALL <- lapply(dataList5[1:10], function(x) x$Open)

xALL <- seriesALL[[1]]
yALL <- seriesALL[[9]]

#Pre Part 3

x <- series[[1]]
y <- series[[9]]

x <-log(x)
y <- log(y)

test <- lm(x ~ y + 0)
b <- test$coefficients[1]
sprd <- x - b * y
adf.test(sprd)

test <- lm(y ~ x + 0)
b <- test$coefficients[1]
sprd <- y - b * x
adf.test(sprd)


whole_sample <- cbind(x,y)
plot(whole_sample)
#In/Out split
split <- 1540
train_data <- lapply(series4, function(x) x[1:split])

model_data <- cbind(train_data[[1]],train_data[[9]])
plot(model_data)

a <- series[[1]] #s1
b <- series [[9]] #s9

model <- lm (b ~ a)
beta <- model$coefficients[1]
spread <- b - beta * a
plot(spread)
adf.test(spread) #0.4772

model <- lm (a ~ b)
beta <- model$coefficients[1]
spread <- a - beta * b
plot(spread)
adf.test(spread) #0.6054

  a2 <- train_data[[1]]
  b2 <- train_data[[9]]
  
  model <- lm (b2 ~ a2)
  beta <- model$coefficients[1]
  spread <- b2 - beta * a2
  plot(spread)
  adf.test(spread)$p.value 
  
  model <- lm (a2 ~ b2)
  beta <- model$coefficients[1]
  spread <- a2 - beta * b2
  plot(spread)
  adf.test(spread)$p.value

######################################################################################################################################
# backtest
######################################################################################################################################
strategyFile <-'strategies/cointegration2.R'
cat("Sourcing",strategyFile,"\n")
source(strategyFile) # load in getOrders

sMult <- 0.20 # slippage multiplier
path <- paste0("images/")
#dataList <- dataList2
numDays <- nrow(dataList5[[1]])
#out of sample
dataList <- lapply(dataList5, function(x) x[2140:numDays])
#params
params<- list(series=c(1,9), big=60, small=5, upperThreshold=1.2, lowerThreshold=-1.2)
backtestAndPlot(path=path,
                filename="final",
                main="Part 3 Results")


######################################################################################################################################
# plots
######################################################################################################################################
#Show in/out sample split
ggplot() + 
  geom_line(data = fortify.zoo(series[[1]], melt = TRUE), aes(x=Index, y=Value, group=Series, color="blue")) +
  geom_line(data = fortify.zoo(series[[9]], melt = TRUE), aes(x=Index, y=Value, group=Series, color="red")) + 
  #geom_vline(xintercept = 1100, color = 'black') +
  scale_color_discrete(name="Series", labels =c("Series 1", "Series 9") ) +
  ggtitle("Series 1 and 9")

#Show P1,P2,P3
ggplot() + 
  geom_line(data = fortify.zoo(seriesALL[[1]], melt = TRUE), aes(x=Index, y=Value, group=Series, color="blue")) +
  geom_line(data = fortify.zoo(seriesALL[[9]], melt = TRUE), aes(x=Index, y=Value, group=Series, color="red")) + 
  #geom_vline(xintercept = 1100, color = 'black') +
  scale_color_discrete(name="Series", labels =c("Series 1", "Series 9") ) +
  ggtitle("Series 1 and 9 (Part 1,2 & 3)")

#Show P3 With drawdown
ggplot() + 
  geom_line(data = fortify.zoo(series3[[1]], melt = TRUE), aes(x=Index, y=Value, group=Series, color="blue")) +
  geom_line(data = fortify.zoo(series3[[9]], melt = TRUE), aes(x=Index, y=Value, group=Series, color="red")) + 
  scale_color_discrete(name="Series", labels =c("Series 1", "Series 9") ) +
  geom_vline(xintercept = 900, color = 'black') +
  ggtitle("Series 1 and 9 Part 3")
