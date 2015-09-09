library(plyr)

setwd("C:/git/digital-platform/country-year")
districts <- read.csv(
  "../reference/uganda-district-entity.csv"
  ,na.strings=""
  ,as.is=TRUE)
pov <- read.csv("uganda-poverty-headcount.csv",as.is=TRUE)

#Vert hist?
library(ggplot2)
require(plyr)
set.seed(1234)
bins <- 20
probBin <- (range(pov$value)[2]-range(pov$value)[1])/bins
binList <- numeric(nrow(pov))
previousBin <- 0
for(i in 1:bins){
  binMax = probBin*i
  for(j in 1:nrow(pov)){
    if(pov$value[j]>previousBin & pov$value[j]<=binMax){
      binList[j] <- round(binMax*100)
    }
  }
  previousBin <- binMax
}
pov$count <- binList
dat <- ddply(pov,.(year,count),summarize,count=sum(!is.na(value)))
p <- ggplot(data = dat, aes(x=count)) 
p <- p + geom_histogram(aes(weights=count))
p
