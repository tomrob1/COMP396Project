source('framework/data.R')
library(ggplot2)
library(grid)
library(gridExtra)
library(tseries)
library(xts)

######################################################################################################################################
# data
######################################################################################################################################
dataList <- getData("PART1")
  dataList <- lapply(dataList[1:10], function(x) x$Open)
dataList2 <- getData("PART2")
dataList3 <- getData("PART3")
series <- lapply(dataList[1:10], function(x) x$Open)
series2 <- lapply(dataList2[1:10], function(x) x$Open)
series3 <- lapply(dataList3[1:10], function(x) x$Open)


# x - series
# y - series
# z - coint
result <- c()
n <- length(dataList)
scoreMatrix <- matrix (0:0, nrow=n, ncol=n)
pValueMatrix <- matrix (1:1, nrow=n, ncol=n)


for (i in 1:n){
  #or 1:n
  for (j in 1:n){
    s1 <- dataList[[i]]
    s2 <- dataList[[j]]
    ratio <- s1-s2
    #print(ratio)
    coint <- adf.test(ratio)
    #print(coint)
    pvalue <- coint$p.value
    pValueMatrix[i,j] <- pvalue

  }
}