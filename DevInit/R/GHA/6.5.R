###6.5 2009-2013DAC donors bilateral humanitarian assistance by expenditure type, 2008-2013
###Data source: OECD CRS, via downloadCRS.js, extractAndRecodeCRS.py, & crs2.R
###Date of download: 14 April 2015
###By Alex Miller
#install.packages('plyr')
#install.packages('ggplot2')
library(plyr)
library(ggplot2)

#Parallel sum for creating new var with NA's present
psum <- function(..., na.rm=FALSE) { 
  x <- list(...)
  rowSums(matrix(unlist(x), ncol=length(x)), na.rm=na.rm)
}

#Set the working directory
wd <- "S:/Projects/Programme resources/Data/GHA calcs and analyses/April 2015/Datasets - do not edit/CRS/"
setwd(wd)

#Define the datasets we want to work with
datasets <- c("CRS 2013 data.csv"
              ,"CRS 2012 data.csv"
              ,"CRS 2011 data.csv"
              ,"CRS 2010 data.csv"
              ,"CRS 2009 data.csv"
              ,"CRS 2008 data.csv")
#Define the purposecodes we want to filter by
purposes <- c(72010,72040,72050,73010,74010)

#Set up empty variables
Year <- c()
donorname <- c()
usd_disbursement_defl <- c()
purposecode <- c()
purposename <- c()

#Iterate through the datasets
for(i in 1:length(datasets)){
  dataset <- datasets[i]
  #Read it in
  dat <- read.csv(dataset,stringsAsFactors=FALSE)
  #Keep only those rows which purposecode == purposes
  dat <- dat[which(dat$purposecode %in% purposes),]
  #Keep only those rows which have usd_disbursement_defl
  dat <- dat[complete.cases(dat$usd_disbursement_defl),]
  #Append to our blank variables
  Year <- c(Year,dat$Year)
  donorname <- c(donorname,dat$donorname)
  usd_disbursement_defl <- c(usd_disbursement_defl,dat$usd_disbursement_defl)
  purposecode <- c(purposecode,dat$purposecode)
  purposename <- c(purposename,dat$purposename)
  #Repeat
}

#Create a dataframe out of our new variables
dat <- data.frame(Year
  ,donorname
  ,usd_disbursement_defl
  ,purposecode
  ,purposename
  ,stringsAsFactors=FALSE)

#Create a pivot table, grouping by Year, donorname, & purposecode
#And making a new variable named usd_sum, which is the sum of
#usd_disbursement_defl
pivot <- ddply(dat
   ,.(Year,donorname,purposecode,purposename)
   ,summarize,usd_sum=sum(usd_disbursement_defl))

#Reshape wide
pivotlong <- pivot
pivot <- reshape(pivot,idvar=c("donorname","purposecode","purposename"),timevar="Year",direction="wide")
#Add sum
pivot <- transform(pivot, usd_sum.total=psum(usd_sum.2008, usd_sum.2009, usd_sum.2010, usd_sum.2011, usd_sum.2012, usd_sum.2013, na.rm=TRUE))
#Sort
pivot <- pivot[order(-pivot$usd_sum.total,pivot$donorname),]
#Rename variables
names(pivot)[which(substr(names(pivot),0,3)=="usd")] <-
  substr(names(pivot)[which(substr(names(pivot),0,3)=="usd")],9,nchar(names(pivot)[which(substr(names(pivot),0,3)=="usd")]))

#Export as csv
write.csv(pivot,"./Subsets/2008-2013 Donor by expenditure type by year.csv",row.names=FALSE,na="")

#Create another pivot table, grouping by donorname, & purposecode
#And making a new variable named usd_sum, which is the sum of
#usd_disbursement_defl
pivot2 <- ddply(dat
               ,.(donorname,purposecode,purposename)
               ,summarize,usd_sum=sum(usd_disbursement_defl))

#Reshape wide
pivot2long <- pivot2
pivot2 <- reshape(pivot2[c("donorname","purposecode","usd_sum")],idvar="donorname",timevar="purposecode",direction="wide")
#Add sum
pivot2 <- transform(pivot2, usd_sum.total=psum(usd_sum.73010, usd_sum.72010, usd_sum.72040, usd_sum.72050, usd_sum.74010, na.rm=TRUE))
#Sort
pivot2 <- pivot2[order(-pivot2$usd_sum.total),]
#Rename variables
names(pivot2)[which(substr(names(pivot2),0,3)=="usd")] <-
  substr(names(pivot2)[which(substr(names(pivot2),0,3)=="usd")],9,nchar(names(pivot2)[which(substr(names(pivot2),0,3)=="usd")]))
#Write
write.csv(pivot2,"./Subsets/2008-2013 Donor by expenditure type.csv",row.names=FALSE,na="")
