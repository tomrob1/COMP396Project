source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('framework/utilities.R'); # for backtestAndPlot function
source('strategies/simple_limit.R');

dataList <- getData(directory='PART1')
#################################################################
# STRATEGY
#################################################################
strategyFile <-'strategies/simple_limit.R'
cat("Sourcing",strategyFile,"\n")
source(strategyFile) # load in getOrders

################################################################
# BACKTEST PARAMETERS
numOfDays <- 500 # don't use all available days to start with!
dataList  <- lapply(dataList, function(x) x[1:numOfDays])
# What does this do
##dataList  <- lapply(dataList, function(x) x[(numOfDays+1):nrow(dataList[[1]])]) 
sMult <- 0.20 # slippage multiplier
params <- list(spreadPercentage=0.001, inventoryLimits=rep(10,10), sizes=positionSizes)

path <- paste0("images/")

#Open prices
opens <- sapply(dataList, function(x) first(x)$Open)
largestOpen <- max(opens)
print(largestOpen)
positionSizes <- round(largestOpen/opens)
print(positionSizes)

estCostToBuy <- sum(positionSizes* opens)
print(estCostToBuy)

target <- 900000
multiplier <- target / estCostToBuy
positionSizes <- round(multiplier * positionSizes)
print(positionSizes)

backtestAndPlot(path=path,
                filename="spend_target_test",
                main="Spending 900k")
