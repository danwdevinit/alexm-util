#install.packages('reshape')
library(reshape)

wd <- "C:/git/digital-platform/country-year"
donorPath <- paste(wd,"intl-flows-donors.csv",sep="/")
recipPath <- paste(wd,"intl-flows-recipients.csv",sep="/")
setwd(wd)

entities <- read.csv("../reference/entity.csv",na.strings="",check.names=FALSE)
keep <- c("id","dac-id")
entities <- entities[keep]
names(entities) <- c("new-id","dac-id")
#Donor
data <- read.csv(donorPath, header = TRUE,sep=",",na.strings="",check.names=FALSE)
keep <- c("id","year","flow-name","value")
data <- merge(
  data
  ,entities
  ,by.x="id"
  ,by.y="dac-id"
  ,all.x=TRUE)
data <- data[c(7,2:6)]
names(data)[1] <- "id"
rdata <- reshape(data[keep],idvar=c("id","year"),timevar="flow-name",direction="wide")
rnames <- names(rdata)
for(i in 1:length(rnames)){
  name = rnames[i]
  if(substr(name,1,5)=="value"){
    names(rdata)[names(rdata) == name] <- substr(name,7,nchar(name))
  }
}
write.csv(rdata,paste(wd,"intl-flows-donors-wide.csv",sep="/"),row.names=FALSE,na="")

#Recipient
data <- read.csv(recipPath, header = TRUE,sep=",",na.strings="",check.names=FALSE)
keep <- c("id","year","flow-name","value")
data <- merge(
  data
  ,entities
  ,by.x="id"
  ,by.y="dac-id"
  ,all.x=TRUE)
data <- data[c(7,2:6)]
names(data)[1] <- "id"
rdata <- reshape(data[keep],idvar=c("id","year"),timevar="flow-name",direction="wide")
rnames <- names(rdata)
for(i in 1:length(rnames)){
  name = rnames[i]
  if(substr(name,1,5)=="value"){
    names(rdata)[names(rdata) == name] <- substr(name,7,nchar(name))
  }
}
write.csv(rdata,paste(wd,"intl-flows-recipients-wide.csv",sep="/"),row.names=FALSE,na="")
