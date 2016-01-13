#Necessary libraries
library(RCurl)
library(rjson)

OECD <- function(url){
  content <- getURL(url, httpheader = list('User-Agent' = 'rsdmx-json/0.0.1'), ssl.verifypeer = FALSE, .encoding = "UTF-8")
  BOM <- "\ufeff"
  if(attr(regexpr(BOM, content), "match.length") != - 1){
    content <- gsub(BOM, "", content)
  }
  rawJson <- fromJSON(content)
  rawData <- rawJson$dataSets[[1]]$observations
  rawStructure <- rawJson$structure
  dimensions <- rawStructure$dimensions[[1]]
  attributes <- rawStructure$attributes$observation
  names <- c(sapply(dimensions, "[[", 2),"obsValue",sapply(attributes, "[[", 1))
  ndim <- length(sapply(dimensions, "[[", 2))
  natt <- 1+length(sapply(attributes, "[[", 1))
  ncol <- ndim+natt
  data <- setNames(data.frame(matrix(ncol = ncol, nrow = 0)),names)
  for(i in 1:length(rawData)){
    row <- rawData[i]
    rawDimensions <- names(row)
    splitDimensions <- strsplit(rawDimensions,":")[[1]]
    for(j in 1:length(splitDimensions)){
      dimensionReference <- dimensions[[j]]$values
      dimensionIndex <- as.integer(splitDimensions[j])+1
      dimensionValue <- dimensionReference[[dimensionIndex]][[2]]
      data[i,j] <- dimensionValue
    }
    for(j in 1:length(row[[1]])){
      if(j>1){
        attributeReference <- attributes[[j-1]]$values
        rawAttIndex <- row[[1]][[j]]
        if(is.null(rawAttIndex)){
          attributeValue <- NA
        }else{
          attributeIndex <- as.integer(rawAttIndex)+1
          attributeValue <- attributeReference[[attributeIndex]][[2]]
        }
      }else{
        attributeValue <- as.double(row[[1]][[j]])
      }
      data[i,ndim+j] <- attributeValue
    }
  }
  names(data)[which(names(data)=="Year")] <- "obsTime"
  return(data)
}
