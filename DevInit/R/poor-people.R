wd<- "C:/git/digital-platform/country-year"
povPath <- "./poverty-125.csv"
popPath <- "./population-total.csv"
setwd(wd)

pov <- read.csv(povPath, header = TRUE,na.strings="",check.names=FALSE,colClasses=c("character","numeric","numeric","numeric"))
pop <- read.csv(popPath, header = TRUE,na.strings="",check.names=FALSE,colClasses=c("character","numeric","numeric"))

dat <- merge(pov, 
             pop, 
             by = c("id","year"),                    
             sort = FALSE
             ,suffixes=c(".pov",".pop"))
dat <- transform(dat,value.pov=value.pov/100)
dat$value <- dat$value.pov*dat$value.pop
keep <- c("id","year","value")
dat <- dat[keep]
dat <- dat[order(dat$id,dat$year),]

write.csv(dat,"poor-people.csv",row.names=FALSE,na="")