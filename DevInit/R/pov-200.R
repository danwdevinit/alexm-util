#install.packages('WDI')
#install.packages('zoo')
#install.packages('plyr')
library(WDI)
require(zoo)
require(plyr)

wd <- "C:/git/digital-platform/country-year/"
setwd(wd)

indicator <- "SI.POV.2DAY"

#Download####
entityPath <- "C:/git/digital-platform/reference/entity.csv"
entities <- read.csv(entityPath, header = TRUE,na.strings="",check.names=FALSE,stringsAsFactors=FALSE)
entities <- entities[which(entities$type=="country"),]$id

dat <- WDI(country = "all", 
           indicator = indicator, 
           start = 1990, 
           end = 2015)

names(dat)[names(dat) == indicator] <- "value"
names(dat)[names(dat) == "iso2c"] <- "id"
keep <- c("id","year","value")
dat <- dat[keep]
id <- c()
year <- c()
value <- c()
for(i in 1:nrow(dat)){
  if(dat$id[i] %in% entities){
    id <- c(id,dat$id[i])
    year <- c(year,dat$year[i])
    value <- c(value,dat$value[i])
  }
}
dat <- data.frame(id,year,value,stringsAsFactors=FALSE)
dat <- dat[order(dat$id,dat$year),]

#Filter####
isoPath <- "C:/git/alexm-util/DevInit/R/povCalISOs.csv"

isos <- read.csv(isoPath, header = TRUE,na.strings="",check.names=FALSE,stringsAsFactors=FALSE)
isos <- isos$iso

id <- c()
year <- c()
value <- c()

for(i in 1:nrow(dat)){
  if(dat$id[i] %in% isos){
    id <- c(id,dat$id[i])
    year <- c(year,dat$year[i])
    value <- c(value,dat$value[i])
  }
}
dat <- data.frame(id,year,value)

#Interpolate####
interpolateCol <- function(data,colname)
{
  return(
    ddply(data,.(id),function(x)
    {
      naLen <- nrow(x[which(is.na(x[,colname])),])
      allLen <- nrow(x)
      valueLen <- allLen-naLen
      ival <- x[,colname]
      x[,paste("original",colname,sep="-")] <- ival 
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
      x[,colname] <- ival 
      return(x)
    }
    )
  )
}

dat <- interpolateCol(dat,"value")

#Write####
write.csv(dat,"poverty-200.csv",row.names=FALSE,na="")