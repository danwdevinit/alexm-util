library(curl)

wd <- "D:/Documents/Data/DHS map"
setwd(wd)

dhscc <- "al"
year <- 2012
# isos <- read.csv("D:/Documents/Data/MICSmeta/headcounts.csv",as.is=TRUE)
# isos <- isos[c("filename","iso2","iso3")]
# isos$iso2 <- tolower(isos$iso2)
# ccOrIso2 <- function(filename,iso2s){
#   results <- c()
#   for(i in 1:length(filename)){
#     cc <- substr(filename[i],1,2)
#     dt <- substr(filename[i],7,8)
#     iso2 <- iso2s[i]
#     if(dt=="dt"){
#       result <- cc
#     }else{
#       result <- iso2
#     }
#     results <- c(results,result)
#   }
#   return(results)
# }
# isos$cc <- ccOrIso2(isos$filename,isos$iso2)
# isos <- isos[c("cc","iso2","iso3")]
# write.csv(isos,"isos.csv",na="",row.names=FALSE)

# cuts <- read.csv("D:/Documents/Data/DHS map/cuts.csv",na.strings="")
# library(zoo)
# library(plyr)
# interpolateCol <- function(x,colname)
# {
#   naLen <- nrow(x[which(is.na(x[,colname])),])
#   allLen <- nrow(x)
#   valueLen <- allLen-naLen
#   ival <- x[,colname]
#   x[,paste("original",colname,sep=".")] <- ival 
#   if(valueLen>=2)
#   {
#     interpVals <- na.approx(x[,colname])
#     xIndex = 1
#     while(is.na(x[,colname][xIndex])){xIndex<-xIndex+1}
#     for(i in 1:length(interpVals))
#     {
#       ival[xIndex] <- interpVals[i]
#       xIndex<-xIndex+1
#     }
#   }
#   x[,colname] <- ival 
#   return(x)
# }
# cuts <- interpolateCol(cuts,"cutpoint")
# write.csv(cuts,"D:/Documents/Data/DHS map/povcuts.csv",na="",row.names=FALSE)

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