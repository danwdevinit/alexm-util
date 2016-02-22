path<- "C:/git/digital-platform/country-year/"
setwd(path)

df <- read.csv("./domestic.csv",colClasses=c("character","numeric","character","character","character","character","character","character","character","numeric","numeric"), header = TRUE,sep=",",na.strings="",stringsAsFactors=FALSE)

totalRevGrants <- subset(df,l1=="total-revenue-and-grants" & is.na(l2))
grantPhrases <- c("grants","official-grants","total-grants")
totalGrants <- subset(df,l1=="total-revenue-and-grants" & (l2 %in% grantPhrases) & is.na(l3))
# setdiff(unique(totalRevGrants$id),unique(totalGrants$id))
# jmGrants <- subset(df,id=="JM" & l3=="grants" & is.na(l4))
# totalGrants <- rbind(totalGrants,jmGrants)
keep <- c("id","year","value.ncu","budget.type")
totalRevGrants <- totalRevGrants[keep]
totalGrants <- totalGrants[keep]
dat <- merge(
  totalRevGrants
  ,totalGrants
  ,by=c("id","year","budget.type")
  ,suffix=c(".total",".grant")
  ,all.x=TRUE
  )
dat <- transform(dat,value=(value.ncu.total-value.ncu.grant))
keep <- c("id","year","value","budget.type")
dat <- dat[keep]

converter <- read.csv("../reference/implied-ppp.csv",colClasses=c("character","numeric","numeric"), header = TRUE,sep=",",na.strings="",stringsAsFactors=FALSE)
deflator <- read.csv("../reference/gdp-deflator-constant-2012.csv",colClasses=c("character","numeric","numeric"), header = TRUE,sep=",",na.strings="",stringsAsFactors=FALSE)
names(deflator) <- c("id","year","value.defl")

dat <- merge(
  dat
  ,converter
  ,by=c("id","year")
  ,suffix=c(".dat",".ppp")
  ,all.x=TRUE
)

dat <- merge(
  dat
  ,deflator
  ,by=c("id","year")
  ,all.x=TRUE
)

dat <- transform(dat,value=(value.dat*value.defl)/value.ppp)
keep <- c("id","year","value","budget.type")
dat <- dat[keep]

pop <- read.csv("./population-total.csv",colClasses=c("character","numeric","numeric"), header = TRUE,sep=",",na.strings="",stringsAsFactors=FALSE)

dat <- merge(
  dat,
  pop,
  by=c("id","year"),
  suffix=c(".rev",".pop"),
  all.x=TRUE
  )

dat <- transform(dat,value=value.rev/value.pop)
dat <- dat[keep]

names(dat) <- c("id","year","value","budget-type")

write.csv(dat,"./non-grant-revenue-PPP-capita.csv",row.names=FALSE,na="")
