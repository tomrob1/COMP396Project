source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('framework/utilities.R'); # for backtestAndPlot function
source('strategies/cointegration2.R') 

#################################################################
# DATA
#################################################################
dataList <- getData(directory="PART1")

#################################################################
# STRATEGY
#################################################################
strategyFile <-'strategies/cointegration2.R'
cat("Sourcing",strategyFile,"\n")
source(strategyFile) # load in getOrders

################################################################
# BACKTEST PARAMETERS
################################################################
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

#NEED TO ADD SIZES PARAM TO STRAT#
params<- list(series=c(1,9), big=60, small=5, upperThreshold=1, lowerThreshold=-1, sizes=)

numOfDays <- 600 
dataList  <- lapply(dataList, function(x) x[(numOfDays+1):nrow(dataList[[1]])])

#Equal position sizes chart
backtestAndPlot(path=path,
                filename="buy_and_hold_one_PART1",
                main="Equal position sizes")
