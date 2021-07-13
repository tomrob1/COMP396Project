getOrders <- function(store,newRowList,currentPos,info,params) {

    allzero  <- rep(0,length(newRowList)) 

    marketOrders <- -currentPos # exit yesterday's positions 

    # position sizing - we return to this in lecture 04
    close_prices <- sapply(newRowList, Cl) 
    position_sizes <- max(close_prices)/close_prices

    los <- longOrShort(newRowList, params)
    marketOrders <- marketOrders + position_sizes*los

    return(list(store=store,marketOrders=marketOrders,
	                        limitOrders1=allzero,
	                        limitPrices1=allzero,
	                        limitOrders2=allzero,
	                        limitPrices2=allzero))
}
longOrShort <- function(newRowList, params) { 
    up_or_dn <- sapply(newRowList,function(x) x$Close>x$Open)
    return(ifelse(up_or_dn, params$response_to_up, 
	                        params$response_to_dn))
}
