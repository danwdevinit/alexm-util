library(plyr)
library(treemap)
library(ggplot2)
setwd("C:/git/alexm-util/DevInit/R/IDC/")

####Define DI colors####
diColors <- c("#ba0c2f" #Red
              ,"#1b365d" #blue
              ,"#ea7600" #Orange
              ,"#93328e" #purple
              ,"#0095c8" #lightblue
              ,"#b7bf10" #Yellow
)
diColorsLong <- c('#00688c'
                  ,'#be84bb'
                  ,'#662363'
                  ,'#b7bf10'
                  ,'#0c192d'
                  ,'#1b365d'
                  ,'#cceaf4'
                  ,'#f2ad66'
                  ,'#ea7600'
                  ,'#0095c8'
                  ,'#595d07'
                  ,'#66bfde'
                  ,'#471845'
                  ,'#e9d6e8'
                  ,'#820820'
                  ,'#93328e'
                  ,'#004862'
                  ,'#f7f2cf'
                  ,'#80850b'
                  ,'#f7ced5'
                  ,'#d4d970'
                  ,'#d1d7df'
                  ,'#122541'
                  ,'#76869e'
                  ,'#5b0516'
                  ,'#fbe4cc'
                  ,'#ba0c2f'
                  ,'#723900'
                  ,'#d66d82'
                  ,'#a35200'
)

flownames <- read.csv("C:/git/digital-platform/reference/flow-name.csv",na.strings="",stringsAsFactors=FALSE)
keep <- c("id","flow.category","type","name","flow.type")
flownames <- flownames[keep]
data <- read.csv("C:/git/digital-platform/country-year/intl-flows-recipients.csv",na.strings="",stringsAsFactors=FALSE)
data <- merge(
  data
  ,flownames
  ,by.x="flow.name"
  ,by.y="id")
eth <- subset(data,id=="ET")
eth <- ddply(eth,.(flow.type.x,year),summarize,total=sum(value,na.rm=TRUE))
setwd("C:/git/alexm-util/DevInit/R/IDC")
write.csv(eth,"eth.csv",na="",row.names=FALSE)
pak <- subset(data,id=="PK")
pak <- ddply(pak,.(flow.type.x,year),summarize,total=sum(value,na.rm=TRUE))
setwd("C:/git/alexm-util/DevInit/R/IDC")
write.csv(pak,"pak.csv",na="",row.names=FALSE)

oda <- read.csv("C:/git/digital-platform/country-year/oda.csv",na.strings="",as.is=TRUE)
dacPath <- "C:/git/alexm-util/DevInit/R/GHA/dac.csv"
dac <- read.csv(dacPath,header=T,as.is=T)[,"id"]
oecd <- subset(oda,id.from %in% dac & year==2013)
isGB <- function(xVector){
  results <- character(length(xVector))
  for(i in 1:length(xVector)){
    x <- xVector[i]
    if(x=="GB"){
      results[i] <- "GB"
    }else{
      results[i] <- "Other"
    }
  }
  return(results)
}
oecd <- transform(oecd,id.from=isGB(id.from))
oecd <- ddply(oecd,.(id.from,sector),summarize,sum=sum(value,na.rm=TRUE))
sectors <- read.csv("C:/git/digital-platform/reference/sector.csv",as.is=TRUE)
oecd <- merge(
  oecd
  ,sectors
  ,by.x="sector"
  ,by.y="id"
  )
write.csv(oecd,"wheredoesoecdodago.csv",na="",row.names=FALSE)
