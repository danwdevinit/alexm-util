library(data.table)
library(plyr)
library(foreign)
library(varhandle)

wd <- "D:/Documents/Data/DHS shapefiles"

setwd(wd)

# zips <- list.files(pattern="*.zip",ignore.case=TRUE)
# 
# for(i in 1:length(zips)){
#   zip <- zips[i]
#   base <- substr(zip,1,nchar(zip)-4)
#   baseSplit <- strsplit(base," ")[[1]]
#   cc <- baseSplit[1]
#   survey <- baseSplit[length(baseSplit)]
#   year <- baseSplit[length(baseSplit)-1]
#   folderName <- paste(cc,year,survey)
#   dir.create(folderName)
#   setwd(folderName)
#   unzip(paste0("../",zip))
#   setwd(wd)
# }

folders <- list.dirs(wd,recursive=FALSE,full.names=FALSE)

dataList <- list()
dataIndex <- 1

for(i in 1:length(folders)){
  folder <- folders[i]
  baseSplit <- strsplit(folder," ")[[1]]
  cc <- baseSplit[1]
  year <- baseSplit[2]
  survey <- baseSplit[3]
  df <- data.frame(cc,year,survey)
  dataList[[dataIndex]] <- df
  dataIndex <- 1 + dataIndex
}

metaData <- rbindlist(dataList)
metaData$ccyear <- paste0(metaData$cc,metaData$year)
ccyears <- unique(subset(metaData,survey %in% c("DHS","DHS_2"))$ccyear)

wd <- "D:/Documents/Data/DHS gps data"
setwd(wd)

dataList <- list()
dataIndex <- 1

dbfs <- list.files(pattern="*.dbf")
for(i in 1:length(dbfs)){
  dbf <- dbfs[i]
  name <- substr(dbf,1,nchar(dbf)-4)
  dat <- read.dbf(dbf)
  cc <- unfactor(dat$DHSCC[1])
  year <- dat$DHSYEAR[1]
  ccyear <- paste0(cc,year)
  if(ccyear %in% ccyears){
    df <- data.frame(name,ccyear)
    dataList[[dataIndex]] <- df
    dataIndex <- 1 + dataIndex
  }
}

data <- rbindlist(dataList)
write.csv(data,"D:/Documents/Data/DHS map/matches.csv",row.names=FALSE)
# data <- read.csv("D:/Documents/Data/DHS map/matches.csv",as.is=TRUE)

data$cc <- sapply(data$ccyear,substr,start=1,stop=2)
data$year <- sapply(data$ccyear,substr,start=3,stop=6)

dat <- ddply(data,.(cc),function(x)
{
  for(i in 1:length(x$ccyear))
  {
      latestName <- x$name[i]
      latestCCyear = x$ccyear[i]
      latestYear = x$year[i]
  }
  y <- c(latestName,latestCCyear,latestYear)
  return(y)
}
)

names(dat) <- c("cc","name","ccyear","year")
write.csv(dat,"D:/Documents/Data/DHS map/matches_latest.csv",row.names=FALSE)
