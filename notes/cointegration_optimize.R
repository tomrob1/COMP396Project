source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('strategies/cointegration2.R') 
# params
# series=c(1,9), big=60, small=5, upperThreshold=1, lowerThreshold=-1
#
dataList <- getData(directory="PART1")
sampleSize <- 365
numOfDays <- nrow(dataList[[1]]) #1100

#dataList <- lapply(dataList, function(x) x[1:numOfDays])
    year1 <- lapply(dataList, function(x) x[1:sampleSize]) # 1 -> 365
    year2 <- lapply(dataList, function(x) x[(sampleSize+1):(numDays-sampleSize)]) # 366 -> 731
    year3 <- lapply(dataList, function(x) x[((numDays-sampleSize + 1):numDays)]) # 732 -> 1100    

sMult <- 0.2 


upperThreshold <- seq(from=0.5, to=2, by= 0.5)
lowerThreshold <- seq(from=-0.5, to=-2, by= -0.5)
paramsList <- list(upperThreshold,lowerThreshold)

numberComb <- prod(sapply(paramsList,length))

resultsMatrix <- matrix(nrow=numberComb, ncol=3)
colnames(resultsMatrix) <- c("upper","lower","PD Ratio")
pfolioPnLList <- vector(mode="list", length=numberComb)

count <- 1
for (up in upperThreshold){
    for(low in lowerThreshold){
        params <- list(upperThreshold = up, lowerThreshold = low, series=c(1,9), big=60, small=5)
        results <- backtest(year2, getOrders, params, sMult) #data goes here
        pfolioPnL<- plotResults(year2, results) # data goes here
        resultsMatrix[count,] <- c(up,low,pfolioPnL$fitAgg)
        pfolioPnLList[[count]] <- pfolioPnL
        cat("completed ", count, " of ", numberComb,"\n")
        print(resultsMatrix[count,])
        count <- count + 1
    }
}
print(resultsMatrix[order(resultsMatrix[,"PD Ratio"]),])