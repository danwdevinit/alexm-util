#install.packages('plyr')
require(plyr)

setwd("C:/git/digital-platform/latest-year")
filenames <- list.files("C:/git/digital-platform/country-year", pattern="*.csv", full.names=TRUE)

latestYear <- function(data)
{
  return(
    ddply(data,.(id),function(x)
    {
      naLen <- nrow(x[which(is.na(x$value)),])
      allLen <- nrow(x)
      valueLen <- allLen-naLen
      if(valueLen>=1)
      {
        for(i in 1:length(x$value))
        {
          if(!is.na(x$value[i])){
            latestId = x$id[i]
            latestYear = x$year[i]
            latestVal = x$value[i]
          }
        }
      }
      else
      {
        latestId = x$id[1]
        latestYear = max(x$year)
        latestVal = NA
      }
      y <- c(latestYear,latestVal)
      return(y)
    }
    )
  )
}

for (i in 1:length(filenames))
{
  data <- read.csv(filenames[i], header = TRUE,sep=",",na.strings="",check.names=FALSE)
  names <- colnames(data)
  if("id" %in% names & "year" %in% names & "value" %in% names)
  {
    data <- data[order(data$id,data$year),]
    data <- latestYear(data)
    names(data)[names(data) == "V1"] <- "year"
    names(data)[names(data) == "V2"] <- "value"
    write.csv(data,basename(filenames[i]),row.names=FALSE,na="")
  }
}