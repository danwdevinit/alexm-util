#Data check for:
#5.10a
#By Alex Miller

#install.packages("plyr")
library(plyr)

#Doublecheck FTS figures 5.10a####

wd <- "S:/Projects/Programme resources/Data/GHA calcs and analyses/April 2015/NGOs/Data check/"
setwd(wd)

#Define the datasets we want to work with
datasets <- c("FTS-2014 final.csv"
              ,"FTS-2013 final.csv"
              ,"FTS-2012 final.csv"
              ,"FTS-2011 final.csv"
              ,"FTS-2010 final.csv"
)


#Iterate through the datasets
data <- read.csv(datasets[1],header=T,as.is=T,na.strings="#N/A")
keep <- names(data)[which(substr(names(data),1,1)!="X")]
data <- data[keep]
for(i in 2:length(datasets)){
  dataset <- datasets[i]
  #Read it in
  dat <- read.csv(dataset,header=T,as.is=T,na.strings="#N/A")
  common_cols <- intersect(names(data),names(dat))
  dat <- dat[common_cols]
  data <- rbind(data[common_cols],dat)
}

#Drop Domestic Response==Yes
data <- data[which(data$Domestic.Response=="No"),]

#Fix capitalization error
data[which(data["Appealing.Agency.NATIONAL.INTERNATIONAL"]=="Affiliated national NGO"),]["Appealing.Agency.NATIONAL.INTERNATIONAL"] <- "Affiliated National NGO"

#Calculate sums
sumsByType = ddply(data
                   ,.(Appealing.Agency.NATIONAL.INTERNATIONAL,Emergency.year)
                   ,summarize
                   ,total=sum(Comm.contr.deflated,na.rm=TRUE))
names(sumsByType)[1] <- "NGO.Type"
sumsByType <- transform(sumsByType,billions=total/1000)
sumsByType
#Figures check out
#However, missing 65 billion in uncategorized...
#And there are at least 1679 organizations marked as NGO
#That are missing a National/International tag
count(data[which(is.na(data["Appealing.Agency.NATIONAL.INTERNATIONAL"])),]["Appealing.Agency..type"])

#5.10b####
subsetERF <- subset(data,Code.Name=="ERF")
#Calculate sums
sumsByTypeERF = ddply(subsetERF
                   ,.(Appealing.Agency.NATIONAL.INTERNATIONAL)
                   ,summarize
                   ,total=sum(Comm.contr.deflated,na.rm=TRUE))
names(sumsByTypeERF)[1] <- "NGO.Type"
sumsByTypeERF
#Again, figures check out
#However, missing 227.6 million in uncategorized...
#And there are 82 organizations marked as NGO
#That are missing a National/International tag
count(data[which(is.na(subsetERF["Appealing.Agency.NATIONAL.INTERNATIONAL"])),]["Appealing.Agency..type"])

#In 2014, only 16 local NGOs and 80 national NGOs were####
#recorded as having received funding in UN OCHA FTS,
#down from 22 and 95 respectively in 2013
subset2014 <- subset(data,Emergency.year==2014)
localsubset <- subset(subset2014
                      ,Appealing.Agency.NATIONAL.INTERNATIONAL=="Local NGOs")
nationalsubset <- subset(subset2014
                         ,Appealing.Agency.NATIONAL.INTERNATIONAL=="National NGO")
nrow(unique(localsubset["Recipient.Organization"]))
#16 unique local NGOs in 2014
nrow(unique(nationalsubset["Recipient.Organization"]))
#80 unique national NGOs in 2014
subset2013 <- subset(data,Emergency.year==2013)
localsubset <- subset(subset2013
                      ,Appealing.Agency.NATIONAL.INTERNATIONAL=="Local NGOs")
nationalsubset <- subset(subset2013
                         ,Appealing.Agency.NATIONAL.INTERNATIONAL=="National NGO")
nrow(unique(localsubset["Recipient.Organization"]))
#22 unique local NGOs
nrow(unique(nationalsubset["Recipient.Organization"]))
#95 unique national NGOs


#Between 2010 and 2014, local and national NGOs combined####
#received US$243 million 1.6% of the total given 
#directly to NGOs and 0.3% of the total international 
#response over the period
localAndNational <- subset(data
  ,(Appealing.Agency.NATIONAL.INTERNATIONAL=="National NGO" | 
      Appealing.Agency.NATIONAL.INTERNATIONAL=="Local NGOs"))
localNationalSum <- sum(localAndNational$Comm.contr.deflated,na.rm=TRUE)
localNationalSum
#243.0739
NGOSum <- sum(subset(data,Appealing.Agency.Code.Name=="NGOs")$Comm.contr.deflated,na.rm=TRUE)
(localNationalSum/NGOSum)*100
#1.595719
totalSum <- sum(data$Comm.contr.deflated,na.rm=TRUE)
(localNationalSum/totalSum)*100
#0.3024516
#All correct

#Total funding given directly to local and national####
# NGOs combined has declined steadily from a peak of
# US$58 million in 2011 to US$46.6 million in 2014
localAndNational <- subset(data
  ,(Appealing.Agency.NATIONAL.INTERNATIONAL=="National NGO" | 
  Appealing.Agency.NATIONAL.INTERNATIONAL=="Local NGOs"))
sum2011 <- sum(subset(localAndNational,Emergency.year==2011)$Comm.contr.deflated,na.rm=TRUE)
sum2011
#57.96926
sum2014 <- sum(subset(localAndNational,Emergency.year==2014)$Comm.contr.deflated,na.rm=TRUE)
sum2014
#46.62757

#Overall funding to NGOs as a percentage of total####
#funding peaked at 18% in 2010, over the five year
# period between 2010 and 2014
ngos <- subset(data
             ,Appealing.Agency..type=="NGOs")
sumNGOs2010 <- sum(subset(ngos,Emergency.year==2010)$Comm.contr.deflated,na.rm=TRUE)
sum2010 <- sum(subset(data,Emergency.year==2010)$Comm.contr.deflated,na.rm=TRUE)
(sumNGOs2010/sum2010)*100
#!!! PROBLEM I'm getting  21.37464
#Possible issues: Are there duplicates between the years?
#Does the sheet for 2011 contain 2010 emergencies?
ngos <- subset(data
               ,(!is.na(Appealing.Agency.NATIONAL.INTERNATIONAL)
                 & Appealing.Agency.NATIONAL.INTERNATIONAL!="Undefined"))
sumNGOs2010 <- sum(subset(ngos,Emergency.year==2010)$Comm.contr.deflated,na.rm=TRUE)
sum2010 <- sum(subset(data,Emergency.year==2010)$Comm.contr.deflated,na.rm=TRUE)
(sumNGOs2010/sum2010)*100
#Even this gives me 18.7%
