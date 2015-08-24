#install.packages("ggplot2")
library(ggplot2)
# Visualizing Distributions

windows <- TRUE
if(windows){pathpre<-"C:"}else{pathpre<-"~"}
wd <- paste0(pathpre,"/git/alexm-util/Finance")
setwd(wd)

df <- read.csv("aapl3.csv")

averages <- ddply(df,.(change,state),summarize,avg=mean(prob))
ggplot(data=averages,aes(x=change,y=avg,group=state,colour=state)) +
  geom_line() +
  geom_point()

fit <- lm(prob ~ call+strike+(call*strike), data=df)
print(summary(fit))

downsub <- subset(df,state=="down")
fit <- lm(prob ~ call+strike+(call*strike), data=downsub)
print(summary(fit))

neutralsub <- subset(df,state=="neutral")
fit <- lm(prob ~ call+strike+(call*strike), data=neutralsub)
print(summary(fit))

upsub <- subset(df,state=="up")
fit <- lm(prob ~ call+strike+(call*strike), data=upsub)
print(summary(fit))

# downsub <- subset(df,state=="down" & change==2.0 & call==0)
# plot(downsub$strike,downsub$prob)
# 
# downsub <- subset(df,state=="down" & change==2.0 & call==1)
# plot(downsub$strike,downsub$prob)

sub <- subset(df,change==5.0)
#Density
par(mfrow=c(3, 1))
states <- unique(sub$state)
for (i in 1:length(states)) {
  d <- density(sub[which(sub$state==states[i]),]$prob)
  plot(d, type="n", main=states[i])
  polygon(d, col="#ba0c2f", border="#ba0c2f")
}

#Box Plot
par(mfrow=c(1, 1))
boxplot(prob~state
        ,data=sub
        ,ylab="probability"
        ,xlab="state")

#Vert hist?
require(plyr)
set.seed(1234)
bins <- 20
probBin <- (range(sub$prob)[2]-range(sub$prob)[1])/bins
binList <- numeric(nrow(sub))
previousBin <- 0
for(i in 1:bins){
  binMax = probBin*i
  for(j in 1:nrow(sub)){
    if(sub$prob[j]>previousBin & sub$prob[j]<=binMax){
      binList[j] <- round(binMax*100)
    }
  }
  previousBin <- binMax
}
sub$probCount <- binList
dat <- ddply(sub,.(state,probCount),summarize,count=sum(!is.na(prob)))
dat$state <- factor(dat$state, ordered=T)
dat$probCount <- factor(dat$probCount)

p <- ggplot(data = dat, aes(x=probCount)) 
p <- p + geom_histogram(aes(weights=count, fill=state))
p <- p + facet_wrap( ~ state, ncol=1)
p

down2 <- subset(downsub,change==5.0)
neutral2 <- subset(neutralsub,change==5.0)
up2 <- subset(upsub,change==5.0)

ggplot(downsub,aes(x=strike,y=prob)) + geom_bin2d()
ggplot(neutralsub,aes(x=strike,y=prob)) + geom_bin2d()
ggplot(upsub,aes(x=strike,y=prob)) + geom_bin2d()

ggplot(down2,aes(x=strike,y=prob)) + geom_bin2d()
ggplot(neutral2,aes(x=strike,y=prob)) + geom_bin2d()
ggplot(up2,aes(x=strike,y=prob)) + geom_bin2d()

ggplot(downsub,aes(x=change,y=prob)) + geom_bin2d()
ggplot(neutralsub,aes(x=change,y=prob)) + geom_bin2d()
ggplot(upsub,aes(x=change,y=prob)) + geom_bin2d()
