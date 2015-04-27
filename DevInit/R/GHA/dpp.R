library(plyr)
#Set the working directory
wd <- "S:/Projects/Programme resources/Data/GHA calcs and analyses/April 2015/Datasets - do not edit/CRS/Other/CRS CSV files April 2015/"
setwd(wd)

#Pull in DAC
dacPath <- "C:/git/alexm-util/DevInit/R/GHA/dac.csv"
dac <- read.csv(dacPath,header=T,as.is=T)

#Define the datasets we want to work with
datasets <- c("CRS 2013 data.csv"
              ,"CRS 2012 data.csv"
              ,"CRS 2011 data.csv"
              ,"CRS 2010 data.csv"
              ,"CRS 2009 data.csv"
              ,"CRS 2008 data.csv"
              ,"CRS 2007 data.csv"
              ,"CRS 2006 data.csv"
              ,"CRS 2004-05 data.csv"
              ,"CRS 2002-03 data.csv"
              ,"CRS 2000-01 data.csv"
)
#Define the purposecodes we want to filter by
purposes <- c(74010)

#Define the flownames we want to filter by
flownames <- c("ODA Grants","ODA Grant-Like", "ODA Loans", "Equity Investment")

#Set up empty variables
Year <- c()
recipientname <- c()
usd_disbursement_defl <- c()
purposecode <- c()
purposename <- c()

#Iterate through the datasets
for(i in 1:length(datasets)){
  dataset <- datasets[i]
  #Read it in
  dat <- read.csv(dataset,stringsAsFactors=FALSE,encoding="latin1")
  #Keep only those rows which purposecode == purposes
  dat <- dat[which(dat$purposecode %in% purposes),]
  #Keep only those rows which flowname == flownames
  dat <- dat[which(dat$flowname %in% flownames),]
  #Keep only those rows which donorname == dac
  dat <- dat[which(dat$donorname %in% dac$donorname),]
  #Keep only those rows which have usd_disbursement_defl
  dat <- dat[complete.cases(dat$usd_disbursement_defl),]
  #Append to our blank variables
  Year <- c(Year,dat$Year)
  recipientname <- c(recipientname,dat$recipientname)
  usd_disbursement_defl <- c(usd_disbursement_defl,dat$usd_disbursement_defl)
  purposecode <- c(purposecode,dat$purposecode)
  purposename <- c(purposename,dat$purposename)
  #Repeat
}

#Create a dataframe out of our new variables
dat <- data.frame(Year
                  ,recipientname
                  ,usd_disbursement_defl
                  ,purposecode
                  ,purposename
                  ,stringsAsFactors=FALSE)

#Pull in ISO3 list
iso3Path <- "C:/git/alexm-util/DevInit/R/GHA/CRS-iso3.csv"
iso3s <- read.csv(iso3Path,as.is=T,header=T)

#Merge Iso3 list with dataframe
dat <- merge(dat,
             iso3s,
             by="recipientname",
             all.x=TRUE)

#Create a pivot table, grouping by recipientname to calculate top 10
#And making a new variable named usd_sum, which is the sum of
#usd_disbursement_defl
pivot <- ddply(dat
               ,.(recipientname)
               ,summarize,usd_sum=sum(usd_disbursement_defl,na.rm=TRUE))

#Sort
pivot <- pivot[order(-pivot$usd_sum),]

#Set the working directory to a different folder
wd <- "S:/Projects/Programme resources/Data/GHA calcs and analyses/April 2015/Datasets - do not edit/DPP expenditure/"
setwd(wd)

#Export as csv
write.csv(pivot,"2000-2013 CRS DPP.csv",row.names=FALSE,na="")
