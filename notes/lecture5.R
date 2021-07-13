source('framework/data.R')
library(ggplot2)
library(grid)
library(gridExtra)

#Load in data
dataList <- getData("PART1")

#autoplot.zoo(dataList[[1]])

#COMBINING SERIEs
opensList <- lapply(dataList[1:10], function(x) x$Open)
opensInSingleXts <- do.call(cbind, opensList)
head(opensInSingleXts, n=2)

q <- ggplot(fortify.zoo(opensInSingleXts, melt=TRUE), aes(x=Index, y=Value, group=Series))
q <- q + geom_line()
q <- q + facet_wrap(~ Series, scales="free")
plot(q)

p <- ggplot(fortify.zoo(diff(opensInSingleXts), melt=TRUE), aes(x=Index, y=Value, group=Series))
p <- p + geom_bar(stat='identity', width = 8)
p <- p + facet_wrap(~ Series, scales="free")

grid.arrange(q,p)


#Chart Series and TA's
d1 <- dataList[[1]]
chartSeries(d1$Open, theme='white', TA=NULL)
addTA(SMA(d1$Open,n=25),on=1)

ind1 <- ifelse((d1$Close-d1$Open) > 5,1,0)
ind2 <- ifelse((d1$Close-d1$Open) > -5,-1,0)
addTA(ind1, col='blue', type='h', on=NA, yrange = c(-1,1))
addTA(ind2, col='red', type='h',on=2)