path<- "C:/git/digital-platform/reference/"
setwd(path)

deflator <- read.csv("gdp-deflator-constant-2012.csv"
                     ,header=T
                     ,as.is=T
                     ,na.strings="")

rate <- read.csv("exchange-rates.csv"
                     ,header=T
                     ,as.is=T
                     ,na.strings="")

dat <- merge(deflator
             ,rate,
             by=c("id","year")
             ,suffix=c(".def",".fx"))

dat$value <- dat$value.def*dat$value.fx

dat <- dat[c("id","year","value")]

write.csv(dat,"current-ncu-to-constant-2012-usd.csv",na="",row.names=F)