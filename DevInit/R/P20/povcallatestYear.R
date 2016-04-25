library(plyr)

wd <- "C:/git/alexm-util/DevInit/PovCalNet/"
setwd(wd)
data <- read.csv("data.csv",header=FALSE,as.is=TRUE)

names(data)[which(names(data)=="V4")] <- "id"
names(data)[which(names(data)=="V7")] <- "type"
names(data)[which(names(data)=="V8")] <- "year"
names(data)[which(names(data)=="V10")] <- "ppp"
names(data)[which(names(data)=="V24")] <- "pop"

keep <- c("id","type","ppp","pop","year")
data <- data[keep]

data$ppp <- as.double(data$ppp)
data$pop <- as.double(data$pop)
data$year <- as.double(data$year)

df <- ddply(data,.(id,type,ppp),function(x){
    for(i in length(x$pop):1){
      if(!is.na(x$pop[i])){
        latestYear = x$year[i]
        latestVal = x$pop[i]
      }
    }
  y <- c(latestYear,latestVal)
  return(y)
})

write.csv(df,"data.csv",na="",row.names=FALSE)
