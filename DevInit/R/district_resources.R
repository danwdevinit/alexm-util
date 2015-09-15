library(plyr)

path<- "C:/git/digital-platform/country-year"
setwd(path)

df <- read.csv("./uganda-finance.csv", header = TRUE,sep=",",na.strings="",stringsAsFactors=FALSE)

#Total
exp <- subset(df,l1=="expenditure")
total <- ddply(exp,.(id,year,budget.type),summarize,value=sum(value,na.rm=TRUE),value.ncu=sum(value.ncu,na.rm=TRUE))
names(total) <- c("id","year","budget.type","total.value","total.value.ncu")

#Education
educ <- subset(exp,l2=="education")
educ <- ddply(educ,.(id,year,budget.type),summarize,value=sum(value,na.rm=TRUE),value.ncu=sum(value.ncu,na.rm=TRUE))
names(educ) <- c("id","year","budget.type","educ.value","educ.value.ncu")

#Health
health <- subset(exp,l2=="health")
health <- ddply(health,.(id,year,budget.type),summarize,value=sum(value,na.rm=TRUE),value.ncu=sum(value.ncu,na.rm=TRUE))
names(health) <- c("id","year","budget.type","health.value","health.value.ncu")

#Agri
agri <- subset(exp,l2=="production-and-marketing")
agri <- ddply(agri,.(id,year,budget.type),summarize,value=sum(value,na.rm=TRUE),value.ncu=sum(value.ncu,na.rm=TRUE))
names(agri) <- c("id","year","budget.type","agri.value","agri.value.ncu")

#Join
dfs <- list(
  total
  ,educ
  ,health
  ,agri
  )

data <- join_all(dfs)

data <- subset(data,id!="#N/A")

data <- transform(data,educ.percent=100*(educ.value/total.value))
data <- transform(data,health.percent=100*(health.value/total.value))
data <- transform(data,agri.percent=100*(agri.value/total.value))

keep <- c("id","year","budget.type","educ.percent","health.percent","agri.percent")
data <- data[keep]

educ.percent <- data[,c(1,2,3,4)]
names(educ.percent)[3:4] <- c("budget-type","value")

health.percent <- data[,c(1,2,3,5)]
names(health.percent)[3:4] <- c("budget-type","value")

agri.percent <- data[,c(1,2,3,6)]
names(agri.percent)[3:4] <- c("budget-type","value")

write.csv(educ.percent,"uganda-educ-percent.csv",row.names=FALSE,na="")
write.csv(health.percent,"uganda-health-percent.csv",row.names=FALSE,na="")
write.csv(agri.percent,"uganda-agri-percent.csv",row.names=FALSE,na="")

#Revenue
rev <- subset(df,l1=="revenue")
totalrev <- ddply(rev,.(id,year,budget.type),summarize,value=sum(value,na.rm=TRUE),value.ncu=sum(value.ncu,na.rm=TRUE))
names(totalrev) <- c("id","year","budget.type","total.value","total.value.ncu")
local <- subset(rev,l2=="locally-raised-revenues")
local <- ddply(local,.(id,year,budget.type),summarize,value=sum(value,na.rm=TRUE),value.ncu=sum(value.ncu,na.rm=TRUE))
names(local) <- c("id","year","budget.type","local.value","local.value.ncu")
donor <- subset(rev,l2=="donor-funding")
donor <- ddply(donor,.(id,year,budget.type),summarize,value=sum(value,na.rm=TRUE),value.ncu=sum(value.ncu,na.rm=TRUE))
names(donor) <- c("id","year","budget.type","donor.value","donor.value.ncu")

revdfs <- list(
  totalrev
  ,local
  ,donor
  )

revdata <- join_all(revdfs)

revdata <- subset(revdata,id!="#N/A")

revdata <- transform(revdata,local.percent=100*(local.value/total.value))
revdata <- transform(revdata,donor.percent=100*(donor.value/total.value))

keep <- c("id","year","budget.type","local.percent","donor.percent")
revdata <- revdata[keep]

local.percent <- revdata[,c(1,2,3,4)]
names(local.percent)[3:4] <- c("budget-type","value")

donor.percent <- revdata[,c(1,2,3,5)]
names(donor.percent)[3:4] <- c("budget-type","value")

write.csv(local.percent,"uganda-local-percent.csv",row.names=FALSE,na="")
write.csv(donor.percent,"uganda-donor-percent.csv",row.names=FALSE,na="")