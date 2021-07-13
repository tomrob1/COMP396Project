maxRows<- 10000
library(roll)
getOrders <- function(store, newRowList, currentPos, info, params){
    
    allzero <- rep(0, length(newRowList))

    if (is.null(store)) {
      store <- initStore(newRowList, params$series)
    }
    store <- updateStore(store, newRowList, params$series)
    marketOrders <- -currentPos; pos <- allzero # exit all positions
    
    x <- store$cl[1:store$iter,1] # Series 1    
    y <- store$cl[1:store$iter,2] # Series 9

    # Price ratio
    ratio <- y/x #Series 9/1
    #print(last(ratio))
    if (store$iter > params$big){
        z <- last(rollma(ratio,params$big,params$small)) # calculate zscore
        print(z)
        if (z >= params$upperThreshold){
            # Buy series 1, short 9 
            marketOrders <- c(1,0,0,0,0,0,0,0,-1,0)
        } else if (z <= params$lowerThreshold) {
            #Short series 1, buy 9
            marketOrders <- c(-1,0,0,0,0,0,0,0,1,0)
        }
        
    } else  
    marketOrders <- allzero
    #print(marketOrders)
    #print(store)
    
    return(list(store=store,marketOrders=marketOrders,
	                    limitOrders1=allzero,limitPrices1=allzero,
	                    limitOrders2=allzero,limitPrices2=allzero))
}

###############################################################################
# main strategy logic
###############################################################################
# Calculate 5 + 60 day moving average
# Calculate 60day standard deviation
# Z = (5ma - 60ma)/60sd
rollma<- function(x, bigwindow, smallwindow) {
    bigma <- rollmean(x,bigwindow)
    smallma <- rollmean(x,smallwindow)
    sd <- rollapply(x, width=60, FUN=sd, na.rm=TRUE)
    z <- ((smallma - bigma)/ sd)
    return (z)
}

###############################################################################
# All the subsequent functions were designed to simplify and 
# improve the readaility of getNewPos(); 
#
# While not required, this type of function-based approach is advisable 
# and these functions will likely have analogues in your strategies
###############################################################################

# functions for managing the store


initClStore  <- function(newRowList,series) {
  clStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(clStore)
}
updateClStore <- function(clStore, newRowList, series, iter) {
  for (i in 1:length(series))
    clStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Close)
  return(clStore)
}
initStore <- function(newRowList,series) {
  return(list(iter=0,cl=initClStore(newRowList,series)))
}
updateStore <- function(store, newRowList, series) {
  store$iter <- store$iter + 1
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter) 
  return(store)
}

###############################################################################
