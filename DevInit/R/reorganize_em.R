library(plyr)
wd <- "C:/git/alexm-util/DevInit/R/"
setwd(wd)

data <- read.csv("lGovtReleases.csv",na.strings=c(" -   "," ",""))
names(data) <- c("district","item","value")

data <- subset(data,!is.na(district)&!is.na(item))

data <- reshape(
  data
  ,timevar=c("district")
  ,idvar=c("item")
  ,direction="wide"
  )

write.csv(data,"releases_reshaped.csv",na="",row.names=FALSE)
