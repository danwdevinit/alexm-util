library(XML)
library(Hmisc)
library(plyr)
library(data.table)

wd <- "D:/Documents/Data/AidData-WB/"

setwd(wd)

iati <- xmlToList("iati_1.04_WorldBank_GeocodedResearchRelease.xml")

dataList <- list()
dataIndex <- 1

for(i in 1:length(iati)){
  element.name <- names(iati[i])
  if(element.name=="iati-activity"){
    activity <- iati[[i]]
    total.value <- 0
    recipient.countries <- ""
    sectors <- ""
    start.date <- ""
    for(j in 1:length(activity)){
      activity.name <- names(activity[j])
      attribute <- activity[[j]]
      if(activity.name=="transaction"){
        if("value" %in% names(attribute)){
          if("text" %in% names(attribute$value)){
            value <- as.double(trimws(attribute$value$text))
            total.value <- total.value + value
          }
        }
      }else if(activity.name=="recipient-country"){
        recip.code <- attribute["code"]
        recip.perc <- attribute["percentage"]
        recipStr <- paste(recip.code,recip.perc,sep=",")
        recipient.countries <- paste(recipient.countries,recipStr,sep=";")
      }else if(activity.name=="sector"){
        sector.code <- attribute["code"]
        sector.perc <- attribute["percentage"]
        sectorStr <- paste(sector.code,sector.perc,sep=",")
        sectors <- paste(sectors,sectorStr,sep=";")
      }else if(activity.name=="activity-date"){
        date <- attribute["iso-date"]
        type <- attribute["type"]
        if(type=="start-actual"){
          start.date <- date
        }
      }
    }
    data <- data.frame(recipient.countries,sectors,start.date,total.value)
    dataList[[dataIndex]] <- data
    dataIndex <- dataIndex + 1
  }
}

metaData <- rbindlist(dataList)
write.csv(metaData,"WB_iati.csv",row.names=FALSE,na="")
