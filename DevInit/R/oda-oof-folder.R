#install.packages('plyr')
library(plyr)

wd <- "C:/git/digital-platform/country-year"
odaPath <- paste(wd,"oda.csv",sep="/")
oofPath <- paste(wd,"oof.csv",sep="/")

#oda
setwd(wd)
odawd <- paste(wd,"oda-donor",sep="/")
unlink(odawd,recursive=TRUE)
dir.create(odawd)
setwd(odawd)
data <- read.csv(odaPath, header = TRUE,sep=",",na.strings="",check.names=TRUE,stringsAsFactors=FALSE)
keep <- c("id.from","id.to","year","value")
data <- ddply(data[keep],.(id.from,id.to,year),summarize,value=sum(value))
data <- data[order(data$id.from,data$id.to,data$year),]
donors <- unique(data$id.from)
donorKeep  <- c("id.to","year","value")
for(i in 1:length(donors)){
  donor = donors[i]
  donorData = data[which(data$id.from==donor),][donorKeep]
  names(donorData)[names(donorData) == "id.to"] <- "id"
  write.csv(donorData,paste(odawd,"/oda-",donor,".csv",sep=""),row.names=FALSE,na="")
}

#oof
setwd(wd)
oofwd <- paste(wd,"oof-donor",sep="/")
unlink(oofwd,recursive=TRUE)
dir.create(oofwd)
setwd(oofwd)
data <- read.csv(oofPath, header = TRUE,sep=",",na.strings="",check.names=TRUE,stringsAsFactors=FALSE)
keep <- c("id.from","id.to","year","value")
data <- ddply(data[keep],.(id.from,id.to,year),summarize,value=sum(value))
data <- data[order(data$id.from,data$id.to,data$year),]
donors <- unique(data$id.from)
donorKeep  <- c("id.to","year","value")
for(i in 1:length(donors)){
  donor = donors[i]
  donorData = data[which(data$id.from==donor),][donorKeep]
  names(donorData)[names(donorData) == "id.to"] <- "id"
  write.csv(donorData,paste(oofwd,"/oof-",donor,".csv",sep=""),row.names=FALSE,na="")
}