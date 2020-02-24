maxRows <- 3100

getOrders <- function(store, newRowList, currentPos, info, params){

    allzero <- rep(0, length(newRowList))

    if (is.null(store)) {
        store <- initStore(newRowList, params$series)
    }
    store <- updateStore(store, newRowList, params$series)

    if (store$iter > params$lookback){
      # Call strategy loguc
      marketOrders <- sapply(1:length(newRowList),
                            function(x) ifelse(x %in% params$series,
                                    lgStFt(store$cl,which(x==params$series),store$iter), 0))
      
    
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
  startIndex <- iter - params$lookback
  momentum <- last(momentum(clStore[startIndex:iter,column], params$lookback))

  momentum1 <- last(momentum(clStore[startIndex:iter,1], params$lookback))
  momentum2 <- last(momentum(clStore[startIndex:iter,2], params$lookback))

  bool1 <- momentum1 > momentum2
  bool2 <- momentum1 <= momentum2
  #print(bool1)
  
  if(momentum1 > momentum2){
    return(1)
  }
  if(momentum1 <= momentum2){
    return(-1)
  } else    return(0)


  ## Returns
  #Calculate monthly returns, lag monthly returns
  # order.by must be either 'names()' or otherwise specified 
  #returns <- last(monthlyReturn(clStore[startIndex:iter,column]))

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
