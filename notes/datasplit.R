source('framework/data.R'); 

dataList <- getData(directory="PART1")
dataList2 <- getData(directory="PART2")

sampleSize <- 365
numDays <- nrow(dataList[[1]]) #1100

#split series 1 into years, adds 4th year for extra values
splitData <- split(dataList[[1]], f ="years")


test <- lapply(dataList, function(x) list(y1=x[1:sampleSize], y2=x[sampleSize+1:(numDays-sampleSize)], y3=x[((numDays-sampleSize + 1):numDays)]))
print (test[[1]])
test[[9]]

year1 <- lapply(dataList, function(x) x[1:sampleSize]) # 1 -> 365
year2 <- lapply(dataList, function(x) x[(sampleSize+1):(numDays-sampleSize)]) #366 ->731
year3 <- lapply(dataList, function(x) x[((numDays-sampleSize + 1):numDays)]) #732 ->1097

splitData <- function(data, window){
  
}