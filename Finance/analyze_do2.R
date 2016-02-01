#install.packages("ggplot2")
#install.packages("dplyr")
library(ggplot2)
library(plyr)
# Visualizing Distributions

windows <- TRUE
if(windows){pathpre<-"C:"}else{pathpre<-"~"}
wd <- paste0(pathpre,"/git/alexm-util/Finance/data")
setwd(wd)

df <- read.csv("amzn_mar2.csv")

averages <- ddply(df,.(change,state),summarize,avg=mean(prob),sd=sd(prob),count=length(prob))
p1 <- ggplot(data=averages,aes(x=change,y=avg,ymax=avg+sd,ymin=avg-sd,group=state,colour=state)) +
  geom_line(size=1) +
  geom_point(size=3) +
  geom_errorbar(width=0.2)
p1

strike_averages <- ddply(df,.(strike,state),summarize,avg=mean(prob),sd=sd(prob),count=length(prob))
p2 <- ggplot(data=strike_averages,aes(x=strike,y=avg,ymax=avg+sd,ymin=avg-sd,group=state,colour=state)) +
  geom_point(size=3) +
  geom_errorbar(width=0.2)
p2

#Box Plot
par(mfrow=c(1, 1))
boxplot(prob~state
        ,data=df
        ,ylab="probability"
        ,xlab="state")

#Vert hist?
sub <- df
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
