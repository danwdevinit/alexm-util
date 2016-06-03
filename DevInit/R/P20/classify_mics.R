####Function and setup####
library(Hmisc)
library(plyr)
library(foreign)
library(data.table)

setwd("D:/Documents/Data/MICSmeta")
varNames <- read.csv("mics_meta_vars_complete.csv",as.is=TRUE,na.strings="")

cwi.class <- function(hh,hl){
  hrBase <- hh$filename
  
  file.varName <- subset(varNames,filename==hrBase)
  
  attendedVar <- subset(file.varName,match=="attended")$varName
#   gradeVar <- subset(file.varName,match=="grade")$varName
  schoolVar <- subset(file.varName,match=="school")$varName
  
  share.toiletsVar <- subset(file.varName,match=="share.toilets")$varName
  toiletsVar <- subset(file.varName,match=="toilets")$varName
  
  carVar <- subset(file.varName,match=="car")$varName
  fridgeVar <- subset(file.varName,match=="fridge")$varName
  phoneVar <- subset(file.varName,match=="phone")$varName
  tvVar <- subset(file.varName,match=="tv")$varName
  
  #Exit function if really low MICS phase
  if(typeof(hh$hh1)=="NULL"){
    return(NA)
  }
  
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
  names(hh)[which(names(hh)==toiletsVar)] <- "toilets"
  if(typeof(hh$toilets)=="NULL"){message("No toilets!");hh$toilets<-NA}
  
  #Rename share.toilets var
  names(hh)[which(names(hh)==share.toiletsVar)] <- "share.toilets"
  if(typeof(hh$share.toilets)=="NULL"){message("No share.toilets!");hh$share.toilets<-NA}

  #Rename urban var
  names(hh)[which(names(hh)=="hh6")] <- "urban.rural"
  if(typeof(hh$urban.rural)=="NULL"){message("No urban.rural!");hh$urban.rural<-NA}
  
  #Rename educ vars
  names(hl)[which(names(hl)==attendedVar)] <- "attended"
  if(typeof(hl$attended)=="NULL"){message("No attended!");hl$attended<-NA}
#   names(hl)[which(names(hl)==gradeVar)] <- "grade"
#   if(typeof(hl$grade)=="NULL"){message("No grade!");hl$grade<-NA}
  names(hl)[which(names(hl)==schoolVar)] <- "school"
  if(typeof(hl$school)=="NULL"){message("No school!");hl$school<-NA}
  
  #potentially double/triple assets
  unique.car <- c()
  if(length(carVar)>0){
    for(i in 1:length(carVar)){
      this.car <- carVar[i]
      unique.car <- unique(tolower(c(unique.car,unique(hh[[this.car]]))))
    } 
  }else{
    unique.car <- list("value"=NA)
  }
  unique.car <- data.frame(unique.car)
  names(unique.car) <- "value"
  unique.car$type <- "ynm"
  unique.fridge <- c()
  if(length(fridgeVar)>0){
    for(i in 1:length(fridgeVar)){
      this.fridge <- fridgeVar[i]
      unique.fridge <- unique(tolower(c(unique.fridge,unique(hh[[this.fridge]]))))
    }
  }else{
    unique.fridge <- list("value"=NA)
  }
  unique.fridge <- data.frame(unique.fridge)
  names(unique.fridge) <- "value"
  unique.fridge$type <- "ynm"
  unique.phone <- c()
  if(length(phoneVar)>0){
    for(i in 1:length(phoneVar)){
      this.phone <- phoneVar[i]
      unique.phone <- unique(tolower(c(unique.phone,unique(hh[[this.phone]]))))
    }
  }else{
    unique.phone <- list("value"=NA)
  }
  unique.phone <- data.frame(unique.phone)
  names(unique.phone) <- "value"
  unique.phone$type <- "ynm"
  unique.tv <- c()
  if(length(tvVar)>0){
    for(i in 1:length(tvVar)){
      this.tv <- tvVar[i]
      unique.tv <- unique(tolower(c(unique.tv,unique(hh[[this.tv]]))))
    }
  }else{
    unique.tv <- list("value"=NA)
  }
  unique.tv <- data.frame(unique.tv)
  names(unique.tv) <- "value"
  unique.tv$type <- "ynm"
  
  unique.share.toilets <- data.frame(tolower(unique(hh$share.toilets)))
  names(unique.share.toilets) <- "value"
  unique.share.toilets$type <- "ynm"

  unique.urban.rural <- data.frame(tolower(unique(hh$urban.rural)))
  names(unique.urban.rural) <- "value"
  unique.urban.rural$type <- "urban.rural"

  unique.attended <- data.frame(tolower(unique(hl$attended)))
  names(unique.attended) <- "value"
  unique.attended$type <- "attended"

  unique.school <- data.frame(tolower(unique(hl$school)))
  names(unique.school) <- "value"
  unique.school$type <- "school"
  
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

  uniques <- rbindlist(
    list(
      unique.wall
      ,unique.floor
      ,unique.water
      ,unique.toilets
      ,unique.attended
      ,unique.urban.rural
      ,unique.school
      ,unique.share.toilets
      ,unique.tv
      ,unique.phone
      ,unique.fridge
      ,unique.car
      ),fill=TRUE
    )
  uniques$filename <- hrBase
  uniques$numerical <- ""
  uniques$inadequate <- ""
  uniques$urban.inadequate <- ""
  uniques$rural.inadequate <- ""
  uniques$urban <- ""
  uniques$ynm <- ""
  uniques$attended <- ""
  uniques$level <- ""
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

for(i in 2:length(dirs)){
  dir <- dirs[i]
  message(basename(dir))
  if(exists("hh")){rm(hh)}
  if(exists("hl")){rm(hl)}
  load(paste0(dir,"/","hh.RData"))
  load(paste0(dir,"/","hl.RData"))
  names(hh) <- tolower(names(hh))
  names(hl) <- tolower(names(hl))
  if(typeof(hh$hh1)!="NULL"){
    vars <- cwi.class(hh,hl)
    dataList[[dataIndex]] <- vars
    dataIndex <- dataIndex + 1 
  }
}

wd <- "D:/Documents/Data/MICSmeta"
setwd(wd)
metaData <- rbindlist(dataList,fill=TRUE)
write.csv(metaData,"global_mics_classes.csv",row.names=FALSE,na="NAN")

###Merge old and rewrite
old_classes <- read.csv("global_mics_classes_old.csv",na.strings="NAN",as.is=TRUE)
keep <- c("value","type","filename","urban","ynm","attended","level")
metaData <- data.frame(metaData,as.is=TRUE)[keep]
metaData <- join(
  metaData
  ,old_classes
  ,by=c("value","type","filename")
  )
write.csv(metaData,"global_mics_classes.csv",row.names=FALSE,na="NAN")
