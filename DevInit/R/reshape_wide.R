#install.packages('reshape')
library(reshape)

filepath <- "C:/git/digital-platform/country-year"

wd <- "C:/git/alexm-util/DevInit/R/tmp"
setwd(wd)
data <- read.csv(filenames[i], header = TRUE,sep=",",na.strings="",check.names=FALSE)
data <- reshape(data,idvar="id",timevar="year",direction="wide")
write.csv(data,paste("name","-wide",".csv",sep=""),row.names=FALSE,na="")