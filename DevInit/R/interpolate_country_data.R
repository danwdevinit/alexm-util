#install.packages('zoo')
#install.packages('plyr')
require(zoo)
require(plyr)

setwd("C:/git/alexm-util/DevInit/R/tmp")
filenames <- list.files("C:/git/digital-platform/country-year", pattern="*.csv", full.names=TRUE)

interpolateSimple <- function(data)
{
  return(
    ddply(data,.(id),function(x)
    {
      naLen <- nrow(x[which(is.na(x$value)),])
      allLen <- nrow(x)
      valueLen <- allLen-naLen
      if(valueLen>=2)
      {
        interpVals <- na.approx(x$value)
        xIndex = 1
        while(is.na(x$value[xIndex])){xIndex<-xIndex+1}
        for(i in 1:length(interpVals))
        {
          x$value <- interpVals[i]
          xIndex<-xIndex+1
        }
      }
      return(x)
    }
  ))
}

for (i in 1:length(filenames))
{
  data <- read.csv(filenames[i], header = TRUE,sep=",")
  names <- colnames(data)
  if(length(names)==3 & "id" %in% names & "year" %in% names & "value" %in% names)
  {
    data <- data[order(data$id,data$year),]
    data <- interpolateSimple(data)
    write.csv(data,paste("interpolated",basename(filenames[i]),sep="-"),row.names=FALSE)
  }
}