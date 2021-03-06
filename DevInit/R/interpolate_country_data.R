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
      ival <- x$value
      x[,"original-value"] <- ival
      if(valueLen>=2)
      {
        interpVals <- na.approx(x$value)
        xIndex = 1
        while(is.na(x$value[xIndex])){xIndex<-xIndex+1}
        for(i in 1:length(interpVals))
        {
          ival[xIndex] <- interpVals[i]
          xIndex<-xIndex+1
        }
      }
      x$value <- ival
      return(x)
    }
    )
  )
}

for (i in 1:length(filenames))
{
  data <- read.csv(filenames[i], header = TRUE,sep=",",na.strings="",check.names=FALSE)
  names <- colnames(data)
  if(length(names)==3 & "id" %in% names & "year" %in% names & "value" %in% names)
  {
    data <- data[order(data$id,data$year),]
    data <- interpolateSimple(data)
    write.csv(data,basename(filenames[i]),row.names=FALSE,na="")
  }
}