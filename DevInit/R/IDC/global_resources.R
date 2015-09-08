library(plyr)
library(ggplot2)
library(treemap)

wd <- "C:/git/digital-platform/country-year"
setwd(wd)

flownames <- read.csv("../reference/flow-name.csv",na.strings="",stringsAsFactors=FALSE)
keep <- c("id","flow.category","type","name")
flownames <- flownames[keep]
data <- read.csv("intl-flows-recipients.csv",na.strings="",stringsAsFactors=FALSE)
data <- merge(
  data
  ,flownames
  ,by.x="flow.name"
  ,by.y="id")
catByYear <- ddply(data,.(year,flow.category),summarize,value=sum(value,na.rm=TRUE)) 

###Area####
p <- ggplot(catByYear,aes(year,value)) +
  geom_area(aes(colour=flow.category,fill=flow.category),position='stack')
p

latestYear <- subset(catByYear,year==2013)
###Pie####
p2 <- ggplot(latestYear,aes(fill=flow.category,x=year,y=value)) +
  geom_bar(position="fill", stat="identity") +
  coord_polar(theta="y") + 
  theme_bw() +
  theme(panel.grid=element_blank()) +
  theme(axis.text=element_blank()) +
  theme(axis.ticks=element_blank()) +
  theme(axis.title=element_blank()) +
  theme(strip.background = element_blank()) +
  theme(panel.border = element_blank()) +
  theme(legend.title = element_blank()) +
  theme(strip.text = element_text(size=15))
p2
####Treemap####
treemap(latestYear
        ,index="flow.category"
        ,vSize="value"
        ,title=""
        ,palette=c("#ffffff")
)