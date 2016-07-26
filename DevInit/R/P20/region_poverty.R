library(foreign)
library(plyr)
library(data.table)
library(varhandle)
library(Hmisc)

wd <- "D:/Documents/Data/DHS map"
setwd(wd)

all.years <- read.csv("D:/Documents/Data/MICSmeta/all.years.csv",as.is=TRUE)
all.years <- subset(all.years,substr(filename,7,8)=="dt")
all.years$DHSCC <- toupper(substr(all.years$filename,1,2))
names(all.years)[which(names(all.years)=="year")] <- "DHSYEAR"
all.years <- ddply(all.years,.(DHSCC,DHSYEAR),function(x)
{
  for(i in 1:length(x$DHSYEAR))
  {
    latestName <- x$filename[i]
  }
  y <- latestName
  return(y)
}
)

names(all.years) <- c("DHSCC","DHSYEAR","filename")

all.years$filename[which(all.years$filename=="ughr6adt")] <- "ughr60dt"
all.years$filename[which(all.years$filename=="drhr6adt")] <- "drhr61dt"
all.years$DHSYEAR[which(all.years$DHSCC=="TG" & all.years$DHSYEAR==2014)] <- 2013
all.years$DHSYEAR[which(all.years$DHSCC=="KE" & all.years$DHSYEAR==2009)] <- 2008
all.years$DHSYEAR[which(all.years$DHSCC=="SN" & all.years$DHSYEAR==2013)] <- 2012
all.years <- rbind(all.years,data.frame(DHSCC="ID",DHSYEAR=2003,filename="idhr42dt"))
all.years <- rbind(all.years,data.frame(DHSCC="CF",DHSYEAR=1994,filename="cfhr31dt"))
all.years$DHSYEAR[which(all.years$DHSCC=="HN" & all.years$DHSYEAR==2012)] <- 2011
all.years <- rbind(all.years,data.frame(DHSCC="NI",DHSYEAR=1998,filename="nihr31dt"))
all.years <- rbind(all.years,data.frame(DHSCC="MA",DHSYEAR=2003,filename="mahr43dt"))
all.years$DHSYEAR[which(all.years$DHSCC=="MD" & all.years$DHSYEAR==2009)] <- 2008

#################################
####Spatial join in QGIS here####
#################################

dat <- read.dbf("spatial_joined_clusters/joined_clusters.dbf",as.is=TRUE)

regions <- dat[c("DHSCC","DHSCLUST","uniqueid")]

setnames(all.years,"DHSYEAR","DHSYEAR")
dat <- join(dat,all.years,by=c("DHSCC","DHSYEAR"))

filenames <- unique(dat[c("filename","DHSYEAR")])
filenames <- filenames[complete.cases(filenames),]

isos <- read.csv("D:/Documents/Data/DHS map/isos.csv",as.is=TRUE)

povcal <- function(dhscc,year){
  library(curl)
  yearDict <- list(
    "1990"=1990
    ,"1991"=1990
    ,"1992"=1993
    ,"1993"=1993
    ,"1994"=1993
    ,"1995"=1996
    ,"1996"=1996
    ,"1997"=1996
    ,"1998"=1999
    ,"1999"=1999
    ,"2000"=1999
    ,"2001"=2002
    ,"2002"=2002
    ,"2003"=2002
    ,"2004"=2005
    ,"2005"=2005
    ,"2006"=2005
    ,"2007"=2008
    ,"2008"=2008
    ,"2009"=2008
    ,"2010"=2012
    ,"2011"=2011
    ,"2012"=2012
    ,"2013"=2012
    ,"2014"=2012
    ,"2015"=2012
    ,"2016"=2012
    ,"2017"=2012
  )
  isos <- read.csv("D:/Documents/Data/DHS map/isos.csv",as.is=TRUE)
  cuts <- read.csv("D:/Documents/Data/DHS map/povcuts.csv",na.strings="",as.is=TRUE)
  sub <- subset(isos,cc==dhscc)
  #   subcuts <- subset(cuts,DHSYEAR==year)
  subcuts <- subset(cuts,DHSYEAR==yearDict[as.character(year)])
  if(nrow(sub)>0){
    iso3 <- sub$iso3[1]
  }else{
    iso3 <- readline(prompt=paste0("Enter ISO3 for ",dhscc,": "))
  }
  if(nrow(subcuts)>0){
    cut <- subcuts$cutpoint[1]
  }else{
    cut <- as.numeric(readline(prompt=paste0("Enter cutpoint for ",year,": ")))
  }
  url <- paste0(
    "http://iresearch.worldbank.org/PovcalNet/Detail.aspx?Format=Detail&C0="
    ,iso3
    ,"_3"
    ,"&PPP0=0&PL0="
    ,cut
    ,"&Y0="
    ,yearDict[as.character(year)]
    ,"&NumOfCountries=1"
  )
  con <- curl(url)
  open(con)
  text <- readLines(curl(url))
  closeAllConnections()
  grepResults <- grep("Headcount(HC): ",text,fixed=TRUE)
  if(length(grepResults)>0){
    datum <- as.numeric(trimws(strsplit(text[grepResults[length(grepResults)]],":")[[1]][2]))
    if(datum>1){
      datum <- datum/100
    }
  }else{
    datum <- NA
  }
  return(datum)
}

weighted.percentile <- function(x,w,prob,na.rm=TRUE){
  df <- data.frame(x,w)
  if(na.rm){
    df <- df[which(complete.cases(df)),]
  }
  #Sort
  df <- df[order(df$x),]
  sumw <- sum(df$w)
  df$cumsumw <- cumsum(df$w)
  #For each percentile
  cutList <- c()
  cutNames <-c()
  for(i in 1:length(prob)){
    p <- prob[i]
    pStr <- paste0(round(p*100,digits=2),"%")
    sumwp <- sumw*p
    df$above.prob <- df$cumsumw>=sumwp
    thisCut <- df$x[which(df$above.prob==TRUE)[1]]
    cutList <- c(cutList,thisCut)
    cutNames <- c(cutNames,pStr)
  }
  names(cutList) <- cutNames
  return(cutList)
}

dataList <- list()
dataIndex <- 1

for(i in 1:nrow(filenames)){
  hrBase <- filenames$filename[i]
  year <- filenames$DHSYEAR[i]
  cc <- toupper(substr(hrBase,1,2))
  dhscc <- tolower(cc)
  phase <- substr(hrBase,5,6)
  hrFile <- paste0(
    "D:/Documents/Data/DHSauto/"
    ,hrBase
    ,"/"
    ,cc
    ,"HR"
    ,phase
    ,"FL"
    ,".csv"
  )
  dhs <- read.csv(hrFile,as.is=TRUE)
  #   iso3 <- subset(isos,cc==dhscc)$iso3[1]
  
  names(dhs)[which(names(dhs)=="hv271")] <- "wealth"
  dhs$wealth <- dhs$wealth/100000
  
  #Rename sample.weights var
  names(dhs)[which(names(dhs)=="hv005")] <- "sample.weights"
  dhs$weights <- dhs$sample.weights/1000000
  
  #Rename cluster/hh var
  names(dhs)[which(names(dhs)=="hv001")] <- "DHSCLUST"
  names(dhs)[which(names(dhs)=="hv002")] <- "household"
  
  povcalcut <- povcal(dhscc,year)
  if(is.na(povcalcut) & dhscc=="eg"){
    povcalcut <- 0.0202
  }else if(is.na(povcalcut) & dhscc=="id"){
    povcalcut <- 0.1590
  }
  povcalperc <- weighted.percentile(dhs$wealth,dhs$weights,prob=povcalcut)
  dhs$p20 <- (dhs$wealth < povcalperc)
  
  dhs$DHSCC <- cc
  dhs <- join(dhs,regions,by=c("DHSCC","DHSCLUST"))
  
  dhs.tab <- data.table(dhs)
  region.tab <- dhs.tab[,.(p20=weighted.mean(p20,weights)),by=.(uniqueid)]
  dataList[[dataIndex]] <- region.tab
  dataIndex <- dataIndex + 1
}

metaRegions <- rbindlist(dataList)
metaRegions$p20[which(is.na(metaRegions$p20))] <- -1
write.csv(metaRegions,"D:/Documents/Data/DHS map/all_regions.csv",na="",row.names=FALSE)
