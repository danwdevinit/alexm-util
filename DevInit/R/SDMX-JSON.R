#Necessary libraries
library(RCurl)
library(rjson)
#For testing####
library(rsdmx)
#OECD Func#
OECD <- function(url,concept=FALSE){
  #Separate out data URL components
  dRoot <- "http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/"
  indicator <- strsplit(substr(url,nchar(dRoot)+1,nchar(url)),"/")[[1]][1]
  filter <- substr(url,nchar(dRoot)+1+nchar(indicator),nchar(url))
  #Structure URL
  sRoot <- "http://stats.oecd.org/restsdmx/sdmx.ashx/GetDataStructure/"
  t1sUrl <- paste(sRoot
                  ,indicator
                  ,sep = "")
  #Fetch data
  t1dsdmx <- readSDMX(url)
  t1ssdmx <- readSDMX(t1sUrl)
  #Convert to DF
  t1 <- as.data.frame(t1dsdmx)
  #get codelists
  cls <- t1ssdmx@codelists
  codelists <- sapply(cls@codelists, function(x) x@id)
  #Recode
  for(i in 1:length(codelists)){
    suffix <- paste("CL_",indicator,"_",sep="")
    clName <- substr(codelists[i],nchar(suffix)+1,nchar(codelists[i]))
    codelist <- cls@codelists[i][[1]]@Code
    for(j in 1:length(codelist)){
      id <- codelist[j][[1]]@id
      name <- codelist[j][[1]]@label$en
      if(clName %in% colnames(t1)){
        t1[clName][which(t1[clName]==id),] <- name
      }
    }
  }
  #get concepts
  concepts <- as.data.frame(t1ssdmx@concepts)
  if(concept){
    return(concepts)
  }else{
    return(t1)
  }
}
#Equivalent URL for SDMX-XML
sdmxUrl <- "http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/TABLE1/20005+20001.1.5+1010+2102.1121+1122.A+D+N/all?startTime=2005&endTime=2014"

sdmxData <- OECD(sdmxUrl)
#end testing####

#Arbitrary short test URL
url <- "http://stats.oecd.org/SDMX-JSON/data/TABLE1/20005+20001.1.5+1010+2102.1121+1122.A+D+N/all?startTime=2005&endTime=2014&dimensionAtObservation=allDimensions"

#CURL content
content <- getURL(url, httpheader = list('User-Agent' = 'rsdmx-json/0.0.1'), ssl.verifypeer = FALSE, .encoding = "UTF-8")

#check the presence of a BOM
BOM <- "\ufeff"
if(attr(regexpr(BOM, content), "match.length") != - 1){
  content <- gsub(BOM, "", content)
}

#Parse JSON
rawJson <- fromJSON(content)

#Extract data
rawData <- rawJson$dataSets[[1]]$observations

#Extract structure
rawStructure <- rawJson$structure

#dimensions are coded in row names (e.g. 0:0:0:0:0:0)
dimensions <- rawStructure$dimensions[[1]]

#attributes are coded in values, with [1] being obs_value, and all else as defined by this variable
attributes <- rawStructure$attributes$observation

#column names are derived from both dimensions and attributes
names <- c(sapply(dimensions, "[[", 2),"obsValue",sapply(attributes, "[[", 1))
ndim <- length(sapply(dimensions, "[[", 2))
#Placeholder for OBS_VALUE
natt <- 1+length(sapply(attributes, "[[", 1))
ncol <- ndim+natt

#Set column names. We've arbitrarily put dimensions first and attributes second
data <- setNames(data.frame(matrix(ncol = ncol, nrow = 0)),names)

#Parse raw data
for(i in 1:length(rawData)){
  row <- rawData[i]
  #First, take the row names (dimensions), split them out, and parse them
  rawDimensions <- names(row)
  splitDimensions <- strsplit(rawDimensions,":")[[1]]
  for(j in 1:length(splitDimensions)){
    dimensionReference <- dimensions[[j]]$values
    dimensionIndex <- as.integer(splitDimensions[j])+1
    dimensionValue <- dimensionReference[[dimensionIndex]][[2]]
    #observation i, at column j is equal to 'dimensionValue', the +1 in the index corrects for how R indexes
    #I picked [[2]] for "name", if you want "id" it needs to be [[1]]
    data[i,j] <- dimensionValue
  }
  for(j in 1:length(row[[1]])){
    if(j>1){
      attributeReference <- attributes[[j-1]]$values
      rawAttIndex <- row[[1]][[j]]
      if(is.null(rawAttIndex)){
        #Case for when null crops up in the value (when there aren't any options)
        attributeValue <- NA
      }else{
        attributeIndex <- as.integer(rawAttIndex)+1
        attributeValue <- attributeReference[[attributeIndex]][[2]]
        #I picked [[2]] for "name", if you want "id" it needs to be [[1]]
      }
    }else{
      #The first one is just the value, it doesn't have a structure
      attributeValue <- as.double(row[[1]][[j]])
    }
    #We have to add ndim to account for the fact that we've shifted the attributes to the back
    data[i,ndim+j] <- attributeValue
  }
}

#Just to match the prior SDMX var names
names(data)[which(names(data)=="Year")] <- "obsTime"
