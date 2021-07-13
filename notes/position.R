source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('framework/utilities.R'); # for backtestAndPlot function
source('strategies/cointegration.R') 

#################################################################
# DATA
#################################################################
dataList <- getData(directory="PART1")
series1 <- lapply(dataList[1], function(x)x$Open)
series9 <- lapply(dataList[9], function(x)x$Open)
dataList <- c(series1,series9)
#################################################################
# STRATEGY
#################################################################
strategyFile <-'strategies/cointegration.R'
cat("Sourcing",strategyFile,"\n")
source(strategyFile) # load in getOrders

sMult <- 0.20 # slippage multiplier

################################################################
# DO BACKTEST
################################################################

path <- paste0("images/")

################################################################
# STRATEGY PARAMETERS

#NEED TO ADD SIZES PARAM TO STRAT#
params<- list(series=c(1,9), big=60, small=5, upperThreshold=1, lowerThreshold=-1, sizes=c(1,0,0,0,0,0,0,0,-1,0))

numOfDays <- 600 
#dataList  <- lapply(dataList, function(x) x[(numOfDays+1):nrow(dataList[[1]])])
dataList <- lapply(dataList, function(x) x[1:numOfDays])
#Equal position sizes chart
backtestAndPlot(path=path,
                filename="buy_and_hold_one_PART1",
                main="Equal position sizes")

#daily open diff
openDiffs <- lapply(dataList,function(x) diff(x$Open))
toPlot <- do.call(cbind,openDiffs)
colnames(toPlot) <- paste("Series",sprintf("%02d",1:10)) 

plot.zoo(toPlot,
         main="Open on open simple differences",
         cex.axis=1.2,
         cex.main=2,
         yax.flip=TRUE)
dev.copy(pdf,file.path(path,"opendiffs.pdf"))
dev.off()

#Absolute open Difference table
absOpenDiffs    <- lapply(openDiffs,abs)
avgAbsDiffs <- sapply(absOpenDiffs,mean,na.rm=TRUE)
print('Avg Abs Diffs', avgAbsDiffs)
opensOnFirstDay <- sapply(dataList,function(x) first(x)$Open)
print(opensOnFirstDay)

tab <- cbind(opensOnFirstDay,
             avgAbsDiffs,
             abs(avgAbsDiffs)/opensOnFirstDay)
colnames(tab) <- c("Open","Mean abs diff","Mean abs diff/Open")
print(tab)

#Pos sizes inverse to open chart
opens <- sapply(dataList,function(x) first(x)$Open)
print(opens)
largestOpen <- max(opens)
print(largestOpen)
positionSizes <- round(largestOpen/opens)
print(positionSizes) #1 1 ????

# pos sizes inverse prop to avg abs diffs chart
largestAvgAbsDiffs <- max(avgAbsDiffs)
positionSizes <- round(largestAvgAbsDiffs/avgAbsDiffs)
print(positionSizes)
params<- list(series=c(1,9), big=60, small=5, upperThreshold=1, lowerThreshold=-1, sizes=c(1,0,0,0,0,0,0,0,-1,0))

estCostToBuy <- sum(positionSizes * opens)
print("cost to buy")
print(estCostToBuy)
target <- 900000 # Try to spend this much
multiplier <- target / estCostToBuy
positionSizes <- round(multiplier * positionSizes)
print(positionSizes)
