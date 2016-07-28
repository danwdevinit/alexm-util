#By Alex Miller

#install.packages("plyr")
library(plyr)

wd <- "C:/git/alexm-util/DevInit/Uganda/Spotlight"
setwd(wd)

#Define the datasets we want to work with
datasets <- c("Expenditure-15-16.csv"
              ,"Expenditure-15-16-missing.csv"
)


#Iterate through the datasets
data <- read.csv(datasets[1],header=T,as.is=T,na.strings="")
for(i in 2:length(datasets)){
  dataset <- datasets[i]
  #Read it in
  dat <- read.csv(dataset,header=T,as.is=T,na.strings="")
  common_cols <- intersect(names(data),names(dat))
  dat <- dat[common_cols]
  data <- rbind(data[common_cols],dat)
}

data$Value <- as.numeric(gsub(",","",data$Value))

refugees <- subset(data, grepl("efugee", Workplan) | grepl("efugee",Revenue.Source))

refByYear <- ddply(refugees,.(year),summarize,ref=sum(Budget,na.rm=TRUE))

totByYear <- ddply(data,.(year),summarize,tot=sum(Budget,na.rm=TRUE))

refByYear <- merge(refByYear,totByYear,by=c("year"))

refByYear <- transform(refByYear,refPer = (ref/tot))

library(ggplot2)
library(scales)
q <- ggplot(refByYear, aes(x=year, y=refPer))
q <- q + geom_bar(stat="identity")
q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1))
q <- q + ylab("Percent of National Budget Pertaining to Refugees")
q <- q + scale_y_continuous(labels=percent)
q

p <- ggplot(totByYear, aes(x=year, y=tot))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p <- p + ylab("Total National Budget")
p
