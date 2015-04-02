wd<- "C:/git/digital-platform/country-year"
isoPath <- "C:/git/alexm-util/DevInit/R/povCalISOs.csv"
indicator <- "poverty-200"
datPath <- paste("./",indicator,".csv",sep="")
setwd(wd)

isos <- read.csv(isoPath, header = TRUE,na.strings="",check.names=FALSE,stringsAsFactors=FALSE)
isos <- isos$iso

dat <- read.csv(datPath, header = TRUE,na.strings="",check.names=FALSE,stringsAsFactors=FALSE)

id <- c()
year <- c()
value <- c()
o.value <- c()

for(i in 1:nrow(dat)){
  if(dat$id[i] %in% isos){
    id <- c(id,dat$id[i])
    year <- c(year,dat$year[i])
    value <- c(value,dat$value[i])
    o.value <- c(o.value,dat[["original-value"]][i])
  }
}
dat <- data.frame(id,year,value,o.value)
names(dat)[names(dat) == "o.value"] <- "original-value"
write.csv(dat,datPath,row.names=FALSE,na="")