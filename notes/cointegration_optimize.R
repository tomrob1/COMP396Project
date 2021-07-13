source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('strategies/cointegration2.R') 
# params
# series=c(1,9), big=60, small=5, upperThreshold=1, lowerThreshold=-1
#
dataList <- getData(directory="PART4")
## 2200 total records, 70% = 1540
split <- 1540
train_data <- lapply(dataList, function(x) x[1:split])

sMult <- 0.2 

upperThreshold <- seq(from=0.8, to=1.5, by= 0.1)
lowerThreshold <- seq(from=-0.8, to=-1.5, by= -0.1)
paramsList <- list(upperThreshold,lowerThreshold)

numberComb <- prod(sapply(paramsList,length))

resultsMatrix <- matrix(nrow=numberComb, ncol=3)
colnames(resultsMatrix) <- c("upper","lower","PD Ratio")
pfolioPnLList <- vector(mode="list", length=numberComb)

count <- 1
for (up in upperThreshold){
    for(low in lowerThreshold){
        params <- list(upperThreshold = up, lowerThreshold = low, series=c(1,9), big=60, small=5)
        results <- backtest(train_data, getOrders, params, sMult) #data goes here
        pfolioPnL<- plotResults(train_data, results) # data goes here
        resultsMatrix[count,] <- c(up,low,pfolioPnL$fitAgg)
        pfolioPnLList[[count]] <- pfolioPnL
        cat("completed ", count, " of ", numberComb,"\n")
        print(resultsMatrix[count,])
        count <- count + 1
    }
}
print(resultsMatrix[order(resultsMatrix[,"PD Ratio"]),])
