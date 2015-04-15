#install.packages('plyr')
#require(devtools)
#install_github('ramnathv/rCharts')
library(plyr)
library(rCharts)
####Data####

#Set the working directory
wd <- "S:/Projects/Programme resources/Data/GHA calcs and analyses/April 2015/Datasets - do not edit/CRS/"
setwd(wd)

#Define the datasets we want to work with
dataset <- "CRS 2013 data.csv"
dat <- read.csv(dataset,stringsAsFactors=FALSE,encoding="latin1")
dat <- ddply(dat,.(donorname,purposecode,purposename),summarize,value=sum(usd_disbursement_defl,na.rm=TRUE))

dat <- dat[order(-dat$value),][1:100,]

d1 <- rPlot(x = 'donorname'
             , y = 'purposename'
             , color = 'value'
             , data = dat
             , type = 'tile')
d1$guides("{color: {scale: {type: gradient, lower: white, upper: steelblue}}}")

d1