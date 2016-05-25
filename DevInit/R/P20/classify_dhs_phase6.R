####Function and setup####
library(Hmisc)
library(plyr)
library(foreign)
library(data.table)

cwi.class <- function(hrwd){
  hrBase <- basename(hrwd)
  iso2 <- toupper(substr(hrBase,1,2))
  phase <- substr(hrBase,5,6)
  phase.short <- substr(phase,1,1)
  
  setwd(hrwd)
  
  hr <- read.dta(paste0(iso2,"HR",phase,"FL.dta"))
  
  #Rename survey year var
  names(hr)[which(names(hr)=="hv007")] <- "year"
  
  #Rename wall var
  names(hr)[which(names(hr)=="hv214")] <- "wall"
  if(typeof(hr$wall)=="NULL"){message("No wall!");hr$wall<-NA}
  
  #Rename floor var
  names(hr)[which(names(hr)=="hv213")] <- "floor"
  if(typeof(hr$floor)=="NULL"){message("No floor!");hr$floor<-NA}
  
  #Rename drinking water var
  names(hr)[which(names(hr)=="hv201")] <- "water"
  if(typeof(hr$water)=="NULL"){message("No water!");hr$water<-NA}
  
  #Rename toilets var
  names(hr)[which(names(hr)=="hv205")] <- "toilets"
  if(typeof(hr$toilets)=="NULL"){message("No toilets!");hr$toilets<-NA}
  
  unique.wall <- data.frame(tolower(unique(hr$wall)))
  names(unique.wall) <- "value"
  unique.wall$type <- "wall"
  unique.floor <- data.frame(tolower(unique(hr$floor)))
  names(unique.floor) <- "value"
  unique.floor$type <- "floor"
  unique.water <- data.frame(tolower(unique(hr$water)))
  names(unique.water) <- "value"
  unique.water$type <- "water"
  unique.toilets <- data.frame(tolower(unique(hr$toilets)))
  names(unique.toilets) <- "value"
  unique.toilets$type <- "toilets"
  uniques <- rbindlist(list(unique.wall,unique.floor,unique.water,unique.toilets),fill=TRUE)
  uniques$iso2 <- iso2
  uniques$filename <- basename(hrwd)
  uniques$numerical <- ""
  uniques$inadequate <- ""
  uniques$urban.inadequate <- ""
  uniques$rural.inadequate <- ""
  uniques$phase <- phase.short
  return(uniques)
  
}
####Run function####
# set our working directory, change this if using on another machine
wd <- "D:/Documents/Data/DHSauto/"
setwd(wd)

# List out all the directories in our wd, this is where our data is contained
dirs <- list.dirs(wd,full.names=TRUE)

dataList <- list()
dataIndex <- 1

# Loop through every dir
for(i in 2:length(dirs)){
  dir <- dirs[i]
  # Pull some coded info out of the dir name
  country <- tolower(substr(basename(dir),1,2))
  recode <- tolower(substr(basename(dir),3,4))
  phase <- as.integer(substr(basename(dir),5,5))
  # For this analysis, we're only interested in individual member recodes, or "hr"
#   if(recode=="hr" & phase==6){
    if(recode=="hr" & phase==5){
    message(basename(dir))
    data <- cwi.class(dir)
    if(!is.na(data)){
      dataList[[dataIndex]] <- data
      dataIndex <- dataIndex + 1 
    }
  }
}

wd <- "D:/Documents/Data/DHSmeta"
setwd(wd)
metaData <- rbindlist(dataList,fill=TRUE)
write.csv(metaData,"global_cwi_classes_5.csv",row.names=FALSE,na="")


###For missing files
# missing <- c(
#   "sthr50dt"
#   ,"eghr5adt"
#   ,"gnhr52dt"
#   ,"mlhr53dt"
#   ,"mlhr60dt"
#   ,"nihr51dt"
#   ,"pkhr52dt"
#   ,"rwhr53dt"
#   ,"rwhr5adt"
# )
# missing <- c("snhr50dt","snhr5hdt", "snhr61dt", "snhr6ddt", "snhr70dt","snhr6rdt")
missing <- "rwhr70dt"
dataList <- list()
dataIndex <- 1
for(i in 1:length(missing)){
  dir <- paste("D:/Documents/Data/DHSauto",missing[i],sep="/")
  data <- cwi.class(dir)
  dataList[[dataIndex]] <- data
  dataIndex <- dataIndex + 1
}
wd <- "D:/Documents/Data/DHSmeta"
setwd(wd)
metaData <- rbindlist(dataList,fill=TRUE)
write.csv(metaData,"global_cwi_classes_extra2.csv",row.names=FALSE,na="")