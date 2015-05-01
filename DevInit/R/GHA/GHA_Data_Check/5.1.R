#Data check for:
#5.1. Humanitarian funding channels 2013
#By Alex Miller

#Required packages: rJava, xlsx
#install.packages("xlsx")
#install.packages("rJava")
#Add jvm.dll to your PATH, w/ 64 bit java
#install.packages("plyr")
library(rJava)
library(xlsx)
library(plyr)

#Check claim that OECD DAC donors gave 14.3 billion in 2013####

#Set the working directory
wd <- "S:/Projects/Programme resources/Data/GHA calcs and analyses/April 2015/Datasets - do not edit/CRS/Other/CRS CSV files April 2015/"
setwd(wd)

#Define DAC donors
dacPath <- "C:/git/alexm-util/DevInit/R/GHA/dac.csv"
dac <- read.csv(dacPath,header=T,as.is=T)

#Define the dataset we want to work with
dataset <- "CRS 2013 data.csv"

#Define the flownames we want to filter by
flownames <- c("ODA Grants","ODA Grant-Like", "ODA Loans", "Equity Investment")

#Define the purposecodes we want to filter by
purposes <- c(72010,72040,72050,73010,74010)

#Read in dataset
dat <- read.csv(dataset,stringsAsFactors=FALSE,encoding="latin1")
#Keep only those rows which flowname == flownames
dat <- dat[which(dat$flowname %in% flownames),]
#Keep only those rows which purposecode == purposes
dat <- dat[which(dat$purposecode %in% purposes),]

#Create new var, 0 for non-dac, 1 for dac
dat <- transform(dat,dac=donorname %in% dac$donorname)

#Pivot to summarize data by dac/non-dac
dactotals <- ddply(dat,.(dac),summarize,total=sum(usd_disbursement_defl,na.rm=TRUE))

msg <- paste("DAC 2013 total for humanitarian purposecodes in the CRS is",round(dactotals$total[which(dactotals$dac==TRUE)],1),"million")
print(msg)
msg <- paste("Non-DAC 2013 total for humanitarian purposecodes in the CRS is",round(dactotals$total[which(dactotals$dac==FALSE)],1),"million")
print(msg)

#Evidently it doesn't match up
#Formula on \\DIPR-DC01\data\Company Data\Projects\Programme resources\Data\GHA calcs and analyses\April 2015\Int-HA\Int HA from Governments.xlsx
#Is sum of DAC, plus EU institutions times
#One minus... DAC's % of total flows plus Turkey's % of total flows
#Divided by All donor % (1)
#Which is the percent of EU flows not from DAC or Turkey
#Primary problem seems to be that the chart is titled
#Humanitarian funding
#But the source data has been drawn from
#http://stats.oecd.org/OECDStat_Metadata/ShowMetadata.ashx?Dataset=TABLE1&ShowOnWeb=true&Lang=en

