#install.packages("ggplot2")
# Visualizing Distributions

wd <- "C:/git/alexm-util/Finance"
setwd(wd)

df <- read.csv("aapl.csv")
fit <- lm(value ~ downPayoff+neutralPayoff+upPayoff, data=df)
print(summary(fit))

downsub <- subset(df,state=="down")
fit <- lm(value ~ downPayoff+neutralPayoff+upPayoff, data=downsub)
print(summary(fit))

neutralsub <- subset(df,state=="neutral")
fit <- lm(value ~ downPayoff+neutralPayoff+upPayoff, data=neutralsub)
print(summary(fit))

upsub <- subset(df,state=="up")
fit <- lm(value ~ downPayoff+neutralPayoff+upPayoff, data=upsub)
print(summary(fit))

sub <- subset(sub,change==3.0)
#Density
par(mfrow=c(3, 1))
states <- unique(sub$state)
for (i in 1:length(states)) {
  d <- density(sub[which(sub$state==states[i]),]$value)
  plot(d, type="n", main=states[i])
  polygon(d, col="#ba0c2f", border="#ba0c2f")
}

#Box Plot
par(mfrow=c(1, 1))
boxplot(value~state
        ,data=sub
        ,ylab="probability"
        ,xlab="state")

#Vert hist?
require(plyr)
set.seed(1234)
bins <- 20
probBin <- (range(sub$value)[2]-range(sub$value)[1])/bins
binList <- numeric(nrow(sub))
previousBin <- 0
for(i in 1:bins){
  binMax = probBin*i
  for(j in 1:nrow(sub)){
    if(sub$value[j]>previousBin & sub$value[j]<=binMax){
      binList[j] <- round(binMax*100)
    }
  }
  previousBin <- binMax
}
sub$prob <- binList
dat <- ddply(sub,.(state,prob),summarize,count=sum(!is.na(value)))
dat$state <- factor(dat$state, ordered=T)
dat$prob <- factor(dat$prob)

require(ggplot2)    
p <- ggplot(data = dat, aes(x=prob)) 
p <- p + geom_histogram(aes(weights=count, fill=state))
p <- p + facet_wrap( ~ state, ncol=1)
p
