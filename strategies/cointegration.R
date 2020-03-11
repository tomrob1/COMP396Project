maxRows<- 10000
library(roll)
getOrders <- function(store, newRowList, currentPos, info, params){
    allzero <- rep(0, length(newRowList))

    if (is.null(store)) {
      store <- initStore(newRowList, params$series)
    }
    store <- updateStore(store, newRowList, params$series)
    marketOrders <- -currentPos; pos <- allzero
    

    
    if (store$iter > params$lookback){
      startIndex <- store$iter - params$lookback
      
      '
      marketOrders   <- sapply(1:length(newRowList),
                        function(x) ifelse(x %in% params$series, 
                                lgStFt(store$cl,which(x==params$series),store$iter), 0))
      '
      #Set up rolling linear regression model with lookback set by strategy params
      model <- roll::roll_lm(store$cl[1:store$iter,1],store$cl[1:store$iter,2],width = params$lookback)
      hedge <- model$coefficients[,2]
      spread <- store$cl[1:store$iter,1] - hedge * store$cl[1:store$iter,2]
      mean <- as.numeric(mean(spread, na.rm=TRUE))
      sd <- as.numeric(sd(spread, na.rm=TRUE))
      z <- (spread-mean)/sd
      print(hedge)

      'model <- lm(store$cl[1:store$iter,1] ~ store$cl[1:store$iter,2])
      hedge <- model$coefficients[2]
      spread <- store$cl[1:store$iter,1] - hedge * store$cl[1:store$iter,2]
      mean <- as.numeric(mean(spread, na.rm=TRUE))
      sd <- as.numeric(sd(spread, na.rm=TRUE))
      z <- (spread - mean)/ sd
      
      if (z <-1 ){
        marketOrders <- marketOrders + 1
      }'

      '
      bigma <- rollmean(spread,60)
      smallma <- rollmean(spread, 5)
      sd <- rollapply(spread, width=60, FUN=sd, na.rm=TRUE)
      zscore <- (smallma - bigma)/sd
      '  
    } else  
    marketOrders <- allzero
    return(list(store=store,marketOrders=marketOrders,
	                    limitOrders1=allzero,limitPrices1=allzero,
	                    limitOrders2=allzero,limitPrices2=allzero))
}

###############################################################################
# main strategy logic
###############################################################################
lgStFt <- function(clStore, column, iter){
    startIndex <- clStore$iter - params$lookback

    model <- lm(clStore[startIndex:iter,1] ~ clStore[startIndex:iter,2])
    hedge <- model$coefficients[2]
    spread <- clStore[startIndex:iter,1] - hedge * clStore[startIndex:iter,2]
    mean <- as.numeric(mean(spread, na.rm=TRUE))
    sd <- as.numeric(sd(spread, na.rm=TRUE))
    z <- last((spread - mean)/sd)
    print(z)

    
}

calculateSpreadZScore <- function(clStore, column, iter, params){

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
# cl store variable, calls update store
updateStore <- function(store, newRowList, series) {
  store$iter <- store$iter + 1
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter) 
  return(store)
}

###############################################################################
