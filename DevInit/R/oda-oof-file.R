#install.packages('plyr')
library(plyr)

wd <- "C:/git/digital-platform/country-year"
odaPath <- paste(wd,"oda.csv",sep="/")
oofPath <- paste(wd,"oof.csv",sep="/")

#oda
setwd(wd)
data <- read.csv(odaPath, header = TRUE,sep=",",na.strings="",check.names=TRUE,stringsAsFactors=FALSE)
keep <- c("id.from","id.to","year","value")
data <- ddply(data[keep],.(id.from,id.to,year),summarize,value=sum(value))
data <- data[order(data$id.from,data$id.to,data$year),]
names(data)[names(data) == "id.to"] <- "id-to"
names(data)[names(data) == "id.from"] <- "id-from"
write.csv(data,paste(wd,"/oda-by-donor.csv",sep=""),row.names=FALSE,na="")

#oof
setwd(wd)
data <- read.csv(oofPath, header = TRUE,sep=",",na.strings="",check.names=TRUE,stringsAsFactors=FALSE)
keep <- c("id.from","id.to","year","value")
data <- ddply(data[keep],.(id.from,id.to,year),summarize,value=sum(value))
data <- data[order(data$id.from,data$id.to,data$year),]
names(data)[names(data) == "id.to"] <- "id-to"
names(data)[names(data) == "id.from"] <- "id-from"
write.csv(data,paste(wd,"/oof-by-donor.csv",sep=""),row.names=FALSE,na="")