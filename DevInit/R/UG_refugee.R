#By Alex Miller

#install.packages("plyr")
library(plyr)

wd <- "C:/Users/alexm/Documents/Ugandan Budget Data/"
setwd(wd)

#Define the datasets we want to work with
datasets <- c("2009-10 Approved Estimates.csv"
              ,"2010-11 Approved Estimates.csv"
              ,"2011-12 Approved Estimates.csv"
              ,"2012-13 Approved Estimates.csv"
              ,"2013-14 Draft Detailed Estimates 10.7.13.csv"
              ,"2014-15 Approved Estimates.csv"
              ,"2015-16 Draft Estimates Vol 1.csv"
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

data$Budget <- as.numeric(gsub(",","",data$Budget))

refugees <- subset(data, grepl("efugee", Department) | grepl("efugee",Economic.Function) | grepl("efugee",Output) | grepl("efugee",Programme))

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
