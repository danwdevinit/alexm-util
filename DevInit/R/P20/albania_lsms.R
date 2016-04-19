library(Hmisc)
library(ggplot2)
library(plyr)
library(foreign)

wd <- "D:/Documents/P20/LSMS 2012_eng/Data_LSMS 2012"
setwd(wd)

df <- read.spss("Modul_1A_householdroster.sav",to.data.frame=TRUE)

hist(df$m1a_q05y,main="Histogram of household member age from Albania 2012 LSMS",xlab="Age",col="#BA0C2F",prob=TRUE)
