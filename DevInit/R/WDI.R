#install.packages('WDI')
library(WDI)
new_cache <- WDIcache()

wd <- "C:/git/digital-platform/country-year/"
entityPath <- "C:/git/digital-platform/reference/entity.csv"
entities <- read.csv(entityPath, header = TRUE,na.strings="",check.names=FALSE,stringsAsFactors=FALSE)
entities <- entities$id
setwd(wd)
indicator <- "NY.GNP.MKTP.KD"

dat <- WDI(country = "all", 
           indicator = indicator, 
           start = 2000, 
           end = 2015,
           extra = TRUE,
           cache = new_cache)

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
dat <- data.frame(id,year,value)
dat <- dat[order(dat$id,dat$year),]

write.csv(dat,"gni-usd-2005.csv",row.names=FALSE,na="")