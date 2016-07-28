library(XML)
library(Hmisc)
library(plyr)
library(data.table)

wd <- "D:/Documents/Data/Uganda AMP/"

setwd(wd)

amp <- xmlToList("iati_1.04_UgandaAIMS_GeocodedResearchRelease.xml")

dataList <- list()
dataIndex <- 1

for(i in 1:length(amp)){
  element.name <- names(amp[i])
  if(element.name=="iati-activity"){
    activity <- amp[[i]]
    total.value <- 0
    location.name <- ""
    longitude <- NA
    latitude <- NA
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
      }else if(activity.name=="location"){
        if("name" %in% names(attribute)){
          location.name <- trimws(attribute$name)
        }
        if("coordinates" %in% names(attribute)){
          latitude <- as.double(attribute$coordinates["latitude"])
          longitude <- as.double(attribute$coordinates["longitude"])
        }
      }
    }
    data <- data.frame(location.name,longitude,latitude,total.value)
    dataList[[dataIndex]] <- data
    dataIndex <- dataIndex + 1
  }
}

metaData <- rbindlist(dataList)
metaData <- metaData[complete.cases(metaData),]
write.csv(metaData,"Uganda_AMP_locations.csv",row.names=FALSE,na="")
