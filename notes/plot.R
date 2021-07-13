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
dataList3 <- getData("PART3")
  series <- lapply(dataList[1:10], function(x) x$Open)
  series2 <- lapply(dataList2[1:10], function(x) x$Open)
  series3 <- lapply(dataList3[1:10], function(x) x$Open)

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

#Part 3 plot
series3SingleXts <- do.call(cbind, series3)
q <- ggplot(fortify.zoo(series3SingleXts, melt=TRUE), aes(x=Index,y=Value, group=Series))
q <- q + geom_line()
q <- q + facet_wrap(~ Series, scales="free")
q <- q + ggtitle("Part 3 Open")
plot(q)
dev.copy(pdf,file='images/seriesOpenP3.png')
dev.off()