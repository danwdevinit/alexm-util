wd<- "C:/git/digital-platform/country-year"
odaPath <- "./in-oda-gross.csv"
poorPeoplePath <- "./poor-people.csv"
setwd(wd)

oda <- read.csv(odaPath, header = TRUE,na.strings="",check.names=FALSE,stringsAsFactors=FALSE)
pp <- read.csv(poorPeoplePath, header = TRUE,na.strings="",check.names=FALSE,stringsAsFactors=FALSE)
entityPath <- "C:/git/digital-platform/reference/entity.csv"
entities <- read.csv(entityPath, header = TRUE,na.strings="",check.names=FALSE,stringsAsFactors=FALSE)
entities <- entities[which(entities$type=="country"),]$id

dat <- merge(oda, 
             pp, 
             by = c("id","year"),                    
             sort = FALSE)
dat$value <- dat$value.x/dat$value.y
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
dat <- data.frame(id,year,value)
dat <- dat[order(dat$id,dat$year),]

write.csv(dat,"oda-per-poor-person.csv",row.names=FALSE,na="")