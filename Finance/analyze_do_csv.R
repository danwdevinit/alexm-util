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

df <- read.csv("apr1_tsla.csv"
               ,header=TRUE
               ,colClasses=c("myDate","numeric","character","numeric","numeric","numeric"))
timestamp <- paste0(
  "\nExpiry: "
  ,df[1,1]
  ,"\nCalculated: "
  ,Sys.Date()
)

change_averages <- ddply(df,.(change,state),summarize,avg=mean(prob))
title <- paste0(
  "Probability of up/neutral/down given percent change",
  timestamp
  )
p1 <- ggplot(data=change_averages,aes(x=change,y=avg,group=state,colour=state)) +
  geom_line() +
  geom_point() +
  ggtitle(title)
p1

strike_averages <- ddply(df,.(strike,state),summarize,avg=mean(prob),sd=sd(prob),count=length(prob))
title <- paste0(
  "Probability of up/neutral/down given strike",
  timestamp
)
p2 <- ggplot(data=strike_averages,aes(x=strike,y=avg,ymax=avg+sd,ymin=avg-sd,group=state,colour=state)) +
  geom_point(size=3) +
  geom_errorbar(width=0.2) +
  ggtitle(title)
p2