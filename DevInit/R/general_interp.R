#install.packages('zoo')
#install.packages('plyr')
require(zoo)
require(plyr)

setwd("C:/git/alexm-util/DevInit/R/tmp")

interpolateCol <- function(data,colname)
{
  return(
    ddply(data,.(id),function(x)
    {
      naLen <- nrow(x[which(is.na(x[,colname])),])
      allLen <- nrow(x)
      valueLen <- allLen-naLen
      ival <- x[,colname]
      if(valueLen>=2)
      {
        interpVals <- na.approx(x[,colname])
        xIndex = 1
        while(is.na(x[,colname][xIndex])){xIndex<-xIndex+1}
        for(i in 1:length(interpVals))
        {
          ival[xIndex] <- interpVals[i]
          xIndex<-xIndex+1
        }
      }
      x[,paste("i",colname,sep="-")] <- ival 
      return(x)
    }
    ))
}

data <- read.csv("C:/git/digital-platform/country-year/employment-by-sector.csv", header = TRUE,sep=",")
names <- colnames(data)
data <- interpolateCol(data,"employment.agriculture")
data <- interpolateCol(data,"employment.industry")
data <- interpolateCol(data,"employment.services")
data <- sapply(data,as.character)
data[is.na(data)] <- ""
write.csv(data,"employment-by-sector.csv",row.names=FALSE)
