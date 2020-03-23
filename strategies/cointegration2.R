maxRows<- 10000
library(roll)
getOrders <- function(store, newRowList, currentPos, info, params){
    allzero <- rep(0, length(newRowList))

    if (is.null(store)) {
      store <- initStore(newRowList, params$series)
    }
    store <- updateStore(store, newRowList, params$series)
    marketOrders <- -currentPos; pos <- allzero
    
    x <- store$cl[1:store$iter,1] # Series 1    
    y <- store$cl[1:store$iter,2] # Series 9

    # Price ratio
    ratio <- y/x

    if (store$iter > params$lookback){
      #startIndex <- store$iter - params$lookback
    
        z <- last(rollma(ratio))
        #z <- last(rollz(ratio,60))
            
        if (z >= 1){
            # Buy series 1, short 9 
            marketOrders <- c(1,0,0,0,0,0,0,0,-1,0)
        } else if (z <= -1 ) {
            #Short series 1, buy 9
            marketOrders <- c(-1,0,0,0,0,0,0,0,1,0)
        }

    } else  
    marketOrders <- allzero
    print(marketOrders)
    return(list(store=store,marketOrders=marketOrders,
	                    limitOrders1=allzero,limitPrices1=allzero,
	                    limitOrders2=allzero,limitPrices2=allzero))
}

###############################################################################
# main strategy logic
###############################################################################
rollz <- function(x,window){
    mean = rollapply(x, window, mean)
    std = rollapply(x, window, sd)
    z = (x - mean) / std
    return (z)
}

rollma<- function(x) {
    bigma <- rollmean(x,60)
    smallma <- rollmean(x,5)
    sd <- rollapply(x, width=60, FUN=sd, na.rm=TRUE)
    z <- ((smallma - bigma)/ sd)
    return (z)
}

rollLM <- function(x, y, width){
    model <- roll::roll_lm(x,y,width = width)
    hedge <- model$coefficients[,2]
    spread <- x - hedge * y
    return (spread)
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
