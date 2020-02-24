source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('framework/utilities.R'); # for backtestAndPlot function
source('strategies/fixed.R') 

#################################################################
# DATA
#################################################################
dataList <- getData(directory="PART1")

#################################################################
# STRATEGY
#################################################################
strategyFile <-'strategies/fixed.R'
cat("Sourcing",strategyFile,"\n")
source(strategyFile) # load in getOrders

################################################################
# BACKTEST PARAMETERS
#numOfDays <- 200 # don't use all available days to start with!
#dataList  <- lapply(dataList, function(x) x[1:numOfDays])
#dataList  <- lapply(dataList, function(x) x[(numOfDays+1):nrow(dataList[[1]])])

sMult <- 0.20 # slippage multiplier

################################################################
# DO BACKTEST
################################################################

path <- paste0("images/")

################################################################
# STRATEGY PARAMETERS

params<- list(sizes=rep(1,10)) # buy and hold equal positions

numOfDays <- 500 
dataList  <- lapply(dataList, function(x) x[(numOfDays+1):nrow(dataList[[1]])])

#Equal position sizes chart
backtestAndPlot(path=path,
                filename="buy_and_hold_one_PART1",
                main="Equal position sizes")

# bbands
#params<- list(lookback=10,sdParam=1.25,series=1:5,positionSizes=positionSizes) # bbands


# daily open difference chart
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
print(positionSizes)
params<- list(sizes=positionSizes) # inversely proportional to starting open

backtestAndPlot(path=path,
                filename="inversely_prop_open",
                main="Position sizes inversely proportional to Open")

# pos sizes inverse prop to avg abs diffs chart
largestAvgAbsDiffs <- max(avgAbsDiffs)
positionSizes <- round(largestAvgAbsDiffs/avgAbsDiffs)
params<- list(sizes=positionSizes) # inversely proportional to average abs diff

backtestAndPlot(path=path,
                filename="inversely_prop_diffs",
                main="Position sizes inversely proportional to Average Abs Diffs")


estCostToBuy <- sum(positionSizes * opens)
print("cost to buy")
print(estCostToBuy)
target <- 900000 # Try to spend this much
multiplier <- target / estCostToBuy
positionSizes <- round(multiplier * positionSizes)
print(positionSizes)

params<- list(sizes=positionSizes) 

backtestAndPlot(path=path,
                filename="spend_target",
                main=paste0("Spending �900,000"))
