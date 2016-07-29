####Function and setup####
library(Hmisc)
library(plyr)
library(foreign)
library(data.table)

wd <- "D:/Documents/Data/MICSmeta/"
setwd(wd)

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
  cuts <- read.csv("D:/Documents/Data/DHS map/cuts.full.csv",na.strings="",as.is=TRUE)
  sub <- subset(isos,cc==dhscc)
  #   subcuts <- subset(cuts,DHSYEAR==year)
  subcuts <- subset(cuts,DHSYEAR==yearDict[as.character(year)])
  if(nrow(sub)>0){
    iso3 <- sub$iso3[1]
  }else{
    iso3 <- readline(prompt=paste0("Enter ISO3 for ",dhscc,": "))
  }
  data <- c()
  steps <- names(cuts)[2:6]
  for(step in steps){
    cut <- subcuts[step][1,1]
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
    data <- c(data,datum)
  }
  names(data) <- steps
  return(data)
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

dir <- "D:/Documents/Data/DHSauto/ughr72dt"

message(basename(dir))
hrwd <- dir
if(!file_test(op="-d", hrwd)){next;}

hrBase <- basename(hrwd)
iso2 <- toupper(substr(hrBase,1,2))
phase <- substr(hrBase,5,6)

hrwd <- paste0("D:/Documents/Data/DHSauto/",tolower(iso2),"hr",phase,"dt/")
if(!file_test(op="-d", hrwd)){next;}

hr <- read.csv(paste0(hrwd,iso2,"HR",phase,"FL.csv")
               ,na.strings="",as.is=TRUE,check.names=FALSE)

names(hr)[which(names(hr)=="hv271")] <- "wealth"
hr$wealth <- hr$wealth/100000

#Rename sample.weights var
names(hr)[which(names(hr)=="hv005")] <- "sample.weights"
hr$weights <- hr$sample.weights/1000000

names(hr)[which(names(hr)=="hv001")] <- "cluster"
names(hr)[which(names(hr)=="hv002")] <- "household"

povcalcuts <- povcal("ug",2014)
povcalperc <- weighted.percentile(hr$wealth,hr$weights,prob=povcalcuts)
names(povcalperc) <- c("p20","p40","p50","p60","p80")

hr$p20 <- (hr$wealth < povcalperc["p20"])
hr$p40 <- (hr$wealth >= povcalperc["p20"] & hr$wealth < povcalperc["p40"])
hr$p50 <- (hr$wealth >= povcalperc["p20"] & hr$wealth < povcalperc["p50"])
hr$p60 <- (hr$wealth >= povcalperc["p40"] & hr$wealth < povcalperc["p60"])
hr$p80 <- (hr$wealth >= povcalperc["p60"] & hr$wealth < povcalperc["p80"])
hr$p100 <- (hr$wealth >= povcalperc["p80"])
hr$gp50 <- (hr$wealth >= povcalperc["p50"])

keep <- c("cluster","household","wealth","weights","p20","p50","gp50")

ug <- hr[keep]

setwd("D:/Documents/Data/WorldPop")

codes <- read.csv("reverse_spatial_join.csv")

codes <- codes[c("cluster","reg_name")]

ug <- data.table(ug)

ug.by.cluster <- ug[,
  .(
    p20=weighted.mean(p20,weights,na.rm=TRUE)
    ,p50=weighted.mean(p50,weights,na.rm=TRUE)
    ,gp50=weighted.mean(gp50,weights,na.rm=TRUE)
    ),by=.(cluster)]

ug <- data.table(join(ug,codes,by="cluster"))
ug.region.by.weighted.mean <- ug[,.(
  p20=weighted.mean(p20,weights,na.rm=TRUE)
  ,p50=weighted.mean(p50,weights,na.rm=TRUE)
  ,gp50=weighted.mean(gp50,weights,na.rm=TRUE)
),by=.(reg_name)]

write.csv(ug.region.by.weighted.mean,"ug.region.by.weighted.mean.csv",row.names=FALSE,na="")
write.csv(ug.by.cluster,"ug.by.cluster.csv",row.names=FALSE,na="")
