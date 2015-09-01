#install.packages("rsdmx")
#install.packages("plyr")
#install.packages("dplyr")
#install.packages("httpuv")
library(rsdmx)
library(plyr)
suppressPackageStartupMessages(library("dplyr"))
library(httpuv)

#OECD Codelist Func####
OECDCode <- function(indicator){
  #Structure URL
  sRoot <- "http://stats.oecd.org/restsdmx/sdmx.ashx/GetDataStructure/"
  t1sUrl <- paste0(sRoot
                  ,indicator
                  )
  #Fetch data
  t1ssdmx <- readSDMX(t1sUrl)
  #get codelists
  cls <- t1ssdmx@codelists
  codelists <- sapply(cls@codelists, function(x) x@id)
  #Flatten
  variable <- c()
  id <- c()
  name <- c()
  for(i in 1:length(codelists)){
    suffix <- paste("CL_",indicator,"_",sep="")
    clName <- substr(codelists[i],nchar(suffix)+1,nchar(codelists[i]))
    codelist <- cls@codelists[i][[1]]@Code
    for(j in 1:length(codelist)){
      ID <- codelist[j][[1]]@id
      NAME <- codelist[j][[1]]@label$en
      id <- c(id,ID)
      name <- c(name,NAME)
      variable <- c(variable,clName)
    }
    df <- data.frame(variable,id,name)
  }
  #return
  return(df)
}

#E.g.####
codes <- OECDCode("CRS1")
recips <- subset(codes,variable=="RECIPIENT")
recipNames <- c(
  'Afghanistan',
  'Algeria',
  'Bangladesh',
  'Burkina Faso',
  'Burundi',
  'Central African Republic',
  'Chad',
  'Colombia',
  "Côte d'Ivoire",
  "Democratic People's Republic of Korea",
  'Democratic Republic of the Congo',
  'Ethiopia',
  'Guinea',
  'Haiti',
  'Indonesia',
  'Iraq',
  'Jordan',
  'Kenya',
  'Lebanon',
  'Liberia',
  'Mali',
  'Myanmar',
  'Nepal',
  'Niger',
  'Nigeria',
  'Pakistan',
  'Philippines',
  'Sierra Leone',
  'Somalia',
  'South Sudan',
  'Sri Lanka',
  'Sudan',
  'Syrian Arab Republic',
  'Uganda',
  'Ukraine',
  'West Bank and Gaza Strip',
  'Yemen',
  'Zimbabwe'
  )
mergeSet <- data.frame(recipNames,rep(1))
recips <- merge(
  recips,
  mergeSet,
  by.x=c("name"),
  by.y=c("recipNames")
  )
recipStr <- paste(recips$id,collapse="+")