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

df <- read.csv("ibm_macd.csv"
               ,header=FALSE
               ,col.names=c("Date","Stock","MACD","Signal")
               ,colClasses=c("myDate","numeric","numeric","numeric"))

p1 <- ggplot(data=df,aes(x=Date,y=Stock)) + geom_line() + geom_point()
p2 <- ggplot(data=df,aes(x=Date)) +
  geom_line(aes(y=MACD,colour="MACD")) +
  geom_point(aes(y=MACD,colour="MACD")) +
  geom_line(aes(y=Signal,colour="Signal")) +
  geom_point(aes(y=Signal,colour="Signal"))

p1
p2
