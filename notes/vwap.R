source('framework/data.R')
dataList <- getData("PART1")

lookback <- 10
uLim <- 1.001 #If price/vwap > uLim short trade
lLim <- 0.999 #If price/vwap < lLim long trade

seriesOpen <- lapply(dataList[1:10], function(x) x$Open)
seriesVolume <- lapply(dataList[1:10], function(x) x$Volume)

#vwap <- VWAP(seriesOpen, seriesVolume, n=10)
# Take OHLC object, newRowList?

#calculate daily returns
#?

#signal = price/vwap
signal <- seriesOpen/vwap
  
  