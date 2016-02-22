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
  ,suffix=c(".total",".grant"))
dat <- transform(dat,value=(value.ncu.grant/value.ncu.total)*100)
keep <- c("id","year","value","budget.type")
dat <- dat[keep]
names(dat) <- c("id","year","value","budget-type")

write.csv(dat,"./grants-pct-totalrevenue.csv",row.names=FALSE,na="")