example_strategies <- c("fixed", 
                        "big_spender",
                        "bankrupt", 
                        "copycat", 
                        "random", 
                        "rsi_contrarian",
                        "rsi_contrarian_limit", 
                        "bbands_trend_following",
                        "bbands_contrarian",
                        "bbands_holding_period",
                        "simple_limit",
                        "probmomentum",
                        "extreme_limit",
                        "cointegration",
                        "cointegration2"
                        )

example_params <- list(
                    "fixed"=list(sizes=rep(1,10)),
                    "big_spender"=list(sizes=rep(1,10)),
                    "bankrupt"=list(leverage=40000000),
                    "copycat"=NULL,
                    "random"=list(maxLots=100),
                    "probmomentum"=list(lookback=10,series=1:2,posSizes=rep(1,10)),
                    "rsi_contrarian"=list(lookback=10,threshold=30,series=1:10),
                    "rsi_contrarian_limit"=list(lookback=10, threshold=30, series=1:3),
                    "bbands_contrarian"=list(lookback=50,sdParam=2,series=1:10,posSizes=rep(1,10)),
                    "bbands_trend_following"=list(lookback=50,sdParam=2,series=1:10,posSizes=rep(1,10)),
                    "bbands_holding_period"=list(lookback=20,sdParam=1.5,series=1:10,posSizes=rep(1,10),holdPeriod=4),
                    "simple_limit"=list(spreadPercentage=0.001,inventoryLimits=rep(10,10)),
                    "extreme_limit"=list(spreadPercentage=0.001,inventoryLimits=rep(10,10)),
                    "cointegration"=list(series=c(1,9), lookback=60),
                    "cointegration2"=list(series=c(1,9), big=60, small=5, upperThreshold=1, lowerThreshold=-1)
                    )

load_strategy <- function(strategy) {

    strategyFile <- file.path('strategies', paste0(strategy,'.R'))

    # load strategy
    cat("Sourcing",strategyFile,"\n")
    source(strategyFile) # load in getOrders

    # set params
    params <<- example_params[[strategy]]
    print("Parameters:")
    print(params)
}
