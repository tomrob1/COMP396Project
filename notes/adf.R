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
dataList2 <- getData("PART2")
  series <- lapply(dataList[1:10], function(x) x$Open)
  series2 <- lapply(dataList2[1:10], function(x) x$Open)

  
#Get opens part 1
open <- dataList[[6]]$Open
#ADF test - stationaritys
adf.test(dataList[[8]]$Open)

######################################################################################################################################
# Plot series
######################################################################################################################################

#Part 1 plot
seriesSingleXts <- do.call(cbind, series)
p <- ggplot(fortify.zoo(seriesSingleXts, melt=TRUE), aes(x=Index,y=Value, group=Series))
p <- p + geom_line()
p <- p + facet_wrap(~ Series, scales="free")
p <- p + ggtitle("Part 1 Open")
plot(p)
dev.copy(pdf,file='images/seriesOpenP1.png')
dev.off()


#Part 2 plot
series2SingleXts <- do.call(cbind, series2)
q <- ggplot(fortify.zoo(series2SingleXts, melt=TRUE), aes(x=Index,y=Value, group=Series))
q <- q + geom_line()
q <- q + facet_wrap(~ Series, scales="free")
q <- q + ggtitle("Part 2 Open")
plot(q)
dev.copy(pdf,file='images/seriesOpenP2.png')
dev.off()

######################################################################################################################################
# Momentum
######################################################################################################################################
s1 <- dataList[[1]]$Close
s2 <- dataList[[2]]$Close

#Merge two series
result <- cbind(s1,s2)

momentumSeries <- momentum(result, n=5)



momentum1 <- momentum(s1, n=5)
momentum2 <- momentum(s2, n=5)
mData <-new.env()
mData$weight$s1 = momentum(s1) > momentum(s2)
mData$weight$s2 = momentum(s1) <= momentum(s2)


######################################################################################################################################
# Returns
######################################################################################################################################
returnS1Function<-monthlyReturn(s1)
returnS1FunctionLag <- lag(returnS1Function)

returnS1 <- ((s1/lag(s1))-1)
returnS1Lag <- lag(returnS1)

