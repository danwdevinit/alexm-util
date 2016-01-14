#Necessary libraries
library(RCurl)
library(rjson)

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

#Initialize data. We've arbitrarily put dimensions first and attributes second
data <- matrix(ncol=ncol,nrow=length(rawData))

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

#Make our matrix a data frame
data <- setNames(data.frame(data,stringsAsFactors=FALSE),names)

#Just to match the prior SDMX var names
names(data)[which(names(data)=="Year")] <- "obsTime"
