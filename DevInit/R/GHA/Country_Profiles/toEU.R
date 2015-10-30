library(plyr)

setwd("C:/git/alexm-util/DevInit/R/GHA/Country_Profiles")

dat <- read.csv("table 1 EU constant.csv",as.is=TRUE,na.strings="")

dat <- reshape(
  dat
  ,direction="long"
  ,times=c(1990:2014)
  ,idvar="DONOR"
  ,varying=names(dat)[2:26]
  ,sep=".")

names(dat)[which(names(dat)=="time")] <- "obsTime"
dat$RECIPIENT <- "EU Institutions"
write.csv(dat,"toEUmanual.csv",row.names=FALSE,na="")
