#install.packages('reshape')
library(reshape)

wd <- "C:/git/digital-platform/country-year"
donorPath <- paste(wd,"intl-flows-donors.csv",sep="/")
recipPath <- paste(wd,"intl-flows-recipients.csv",sep="/")
setwd(wd)

#Donor
data <- read.csv(donorPath, header = TRUE,sep=",",na.strings="",check.names=FALSE)
keep <- c("id","year","flow-name","value")
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
rdata <- reshape(data[keep],idvar=c("id","year"),timevar="flow-name",direction="wide")
rnames <- names(rdata)
for(i in 1:length(rnames)){
  name = rnames[i]
  if(substr(name,1,5)=="value"){
    names(rdata)[names(rdata) == name] <- substr(name,7,nchar(name))
  }
}
write.csv(rdata,paste(wd,"intl-flows-recipients-wide.csv",sep="/"),row.names=FALSE,na="")
