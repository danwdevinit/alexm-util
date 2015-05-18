#install.packages("ggplot2")
# Visualizing Distributions

wd <- "C:/git/alexm-util/DevInit/R/GHA/GHA_Data_Check"
setwd(wd)

df <- read.csv("4.7.csv")

#Density
par(mfrow=c(4, 4))
years <- unique(df$Year)
for (i in 1:length(years)) {
  d <- density(df[which(df$Year==years[i]),]$X..Covered..C.B)
  plot(d, type="n", main=years[i])
  polygon(d, col="#ba0c2f", border="#ba0c2f")
}

#Box Plot
par(mfrow=c(1, 1))
boxplot(X..Covered..C.B~Year
        ,data=df
        ,ylab="% Covered"
        ,xlab="Year")

#Vert hist?
require(plyr)
set.seed(1234)
bins <- 20
coveredBin <- (range(df$X..Covered..C.B)[2]-range(df$X..Covered..C.B)[1])/bins
binList <- numeric(nrow(df))
previousBin <- 0
for(i in 1:bins){
  binMax = coveredBin*i
  for(j in 1:nrow(df)){
    if(df$X..Covered..C.B[j]>previousBin & df$X..Covered..C.B[j]<=binMax){
      binList[j] <- round(binMax*100)
    }
  }
  previousBin <- binMax
}
df$covered <- binList
dat <- ddply(df,.(Year,covered),summarize,count=sum(!is.na(X..Covered..C.B)))
dat$Year <- factor(dat$Year, ordered=T)
dat$covered <- factor(dat$covered)

require(ggplot2)    
p <- ggplot(data = dat, aes(x=covered)) 
p <- p + geom_histogram(aes(weights=count, fill=Year))
p <- p + facet_wrap( ~ Year, ncol=2)
p