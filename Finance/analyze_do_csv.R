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
               ,colClasses=c("myDate",rep("numeric")))

df[150,]
