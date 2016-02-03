#install.packages("ggplot2")
#install.packages("dplyr")
library(ggplot2)
library(plyr)

windows <- TRUE
if(windows){pathpre<-"C:"}else{pathpre<-"~"}
wd <- paste0(pathpre,"/git/alexm-util/Finance/data")
setwd(wd)

setClass("myDate")
setAs("character","myDate", function(from) as.Date(from, format="%m/%d/%y"))

df <- read.csv("spx.csv"
               ,header=TRUE
               ,colClasses=c("myDate","numeric","character","numeric","numeric","numeric"))

averages <- ddply(df,.(date,state),summarize,avg=mean(prob),sd=sd(prob),count=length(prob))
p1 <- ggplot(data=averages,aes(x=date,y=avg,ymax=avg+sd,ymin=avg-sd,group=state,colour=state)) +
  geom_line(size=1) +
  geom_point(size=3) +
  geom_errorbar(width=0.2)
p1

strike_averages <- ddply(df,.(strike,state),summarize,avg=mean(prob),sd=sd(prob),count=length(prob))
p2 <- ggplot(data=strike_averages,aes(x=strike,y=avg,ymax=avg+sd,ymin=avg-sd,group=state,colour=state)) +
  geom_point(size=3) +
  geom_errorbar(width=0.2)
p2