####Function and setup####
library(Hmisc)
library(plyr)
library(foreign)
library(data.table)

setwd("D:/Documents/Data/MICSmeta")
varNames <- read.csv("toiletsVars.csv",as.is=TRUE,na.strings="")

cwi.class <- function(hrwd){
  hrBase <- basename(hrwd)
  
  toiletVar <- subset(varNames,filename==hrBase)$toiletsVar
    
  hh <- read.csv(paste0(hrwd,"/hh.csv"),as.is=TRUE,na.strings="",check.names=FALSE)
  
  #Exit function if really low MICS phase
  if(typeof(hh$hh1)=="NULL"){
    return(NA)
  }
  
  #Rename survey year var
  names(hh)[which(names(hh)=="hh5y")] <- "year"
  
  #Rename wall var
  names(hh)[which(names(hh)=="hc5")] <- "wall"
  if(typeof(hh$wall)=="NULL"){message("No wall!");hh$wall<-NA}
  
  #Rename floor var
  names(hh)[which(names(hh)=="hc3")] <- "floor"
  if(typeof(hh$floor)=="NULL"){message("No floor!");hh$floor<-NA}
  
  #Rename drinking water var
  names(hh)[which(names(hh)=="ws1")] <- "water"
  if(typeof(hh$water)=="NULL"){message("No water!");hh$water<-NA}
  
  #Rename toilets var
  names(hh)[which(names(hh)==toiletVar)] <- "toilets"
  if(typeof(hh$toilets)=="NULL"){message("No toilets!");hh$toilets<-NA}
  
  unique.wall <- data.frame(tolower(unique(hh$wall)))
  names(unique.wall) <- "value"
  unique.wall$type <- "wall"
  unique.floor <- data.frame(tolower(unique(hh$floor)))
  names(unique.floor) <- "value"
  unique.floor$type <- "floor"
  unique.water <- data.frame(tolower(unique(hh$water)))
  names(unique.water) <- "value"
  unique.water$type <- "water"
  unique.toilets <- data.frame(tolower(unique(hh$toilets)))
  names(unique.toilets) <- "value"
  unique.toilets$type <- "toilets"
  uniques <- rbindlist(list(unique.wall,unique.floor,unique.water,unique.toilets),fill=TRUE)
  uniques$filename <- basename(hrwd)
  uniques$numerical <- ""
  uniques$inadequate <- ""
  uniques$urban.inadequate <- ""
  uniques$rural.inadequate <- ""
  return(uniques)
  
}
####Run function####
# set our working directory, change this if using on another machine
wd <- "D:/Documents/Data/MICSauto/"
setwd(wd)

# List out all the directories in our wd, this is where our data is contained
dirs <- list.dirs(wd,full.names=TRUE)

dataList <- list()
dataIndex <- 1

# Loop through every dir
for(i in 2:length(dirs)){
  dir <- dirs[i]
  message(basename(dir))
  data <- cwi.class(dir)
  if(!is.na(data)){
    dataList[[dataIndex]] <- data
    dataIndex <- dataIndex + 1 
  }
}

wd <- "D:/Documents/Data/MICSmeta"
setwd(wd)
metaData <- rbindlist(dataList,fill=TRUE)
write.csv(metaData,"global_mics_classes.csv",row.names=FALSE,na="")