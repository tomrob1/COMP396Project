source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('framework/utilities.R'); # for backtestAndPlot function
session <- Sys.getenv("SESSION")
source('lecture_03/analyse_cond_pr.R')

# DATA ##################################

full_dataList <- getData(directory="PART1")

#####################################
strategyFile <-'lecture_03/cond_pr_strategy.R'

thr <- 0.525
params <- get_params(thr=thr)

cat("Sourcing",strategyFile,"\n")
source(strategyFile) # load in getOrders

print("Parameters:")
print(params)

# BACKTEST PARAMETERS ##########################
# split data in two (e.g. for in/out test)
numDays <- nrow(full_dataList[[1]])
inSampDays <- 500

path <- ("/lecture_03")

# IN-SAMPLE; NO SLIPPAGE ############################

# in-sample period
dataList <- lapply(full_dataList, function(x) x[1:inSampDays])

sMult <- 0.0 # slippage multiplier

backtestAndPlot(path=path,
                filename="cond_pr_in_sample_no_slippage",
                main="Cond Pr Strategy (thr = 0.525): in-sample; no slippage")

# IN-SAMPLE; NORMAL SLIPPAGE ############################

sMult <- 0.2 # slippage multiplier

backtestAndPlot(path=path,
                filename="cond_pr_in_sample_with_slippage",
                main="Cond Pr Strategyy (thr = 0.525): in-sample; with slippage")

# out-of-sample period
dataList <- lapply(full_dataList, function(x) x[(inSampDays+1):numDays])

# OUT-OF-SAMPLE; NO SLIPPAGE ############################

sMult <- 0.0 # slippage multiplier

backtestAndPlot(path=path,
                filename="cond_pr_out_of_sample_no_slippage",
                main="Cond Pr Strategy (thr = 0.525): out-of-sample; no slippage")

# OUT-OF-SAMPLE; NORMAL SLIPPAGE ############################

sMult <- 0.2 # slippage multiplier

backtestAndPlot(path=path,
                filename="cond_pr_out_of_sample_with_slippage",
                main="Cond Pr Strategy  (thr = 0.525): out-of-sample; with slippage")
