reid <- function(indicator){
  wd <- "C:/git/digital-platform/country-year/"
  path <- paste0(wd,indicator,".csv")
  setwd(wd)
  entities <- read.csv("../reference/entity.csv",na.strings=c(""),check.names=FALSE)
  keep <- c("id","dac-id")
  entities <- entities[keep]
  names(entities) <- c("new-id","dac-id")
  data <- read.csv(path, header = TRUE,sep=",",na.strings="",check.names=FALSE)
  data <- merge(
    data
    ,entities
    ,by.x="id"
    ,by.y="dac-id"
    ,all.x=TRUE)
  data <- data[c(ncol(data),2:(ncol(data)-1))]
  names(data)[1] <- "id"
  write.csv(data,path,row.names=FALSE,na="")
}

