#FTS Recipient coding set
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

data$USD.committed.contributed <- as.numeric(gsub(",","",data$USD.committed.contributed))

#Fix capitalization error
data[which(data["Appealing.Agency.NATIONAL.INTERNATIONAL"]=="Affiliated national NGO"),]["Appealing.Agency.NATIONAL.INTERNATIONAL"] <- "Affiliated National NGO"


df <- ddply(data,.(Recipient.Organization,Appealing.Agency.NATIONAL.INTERNATIONAL),summarize,donorCount=length(unique(Donor)),totalContrib=sum(USD.committed.contributed,na.rm=TRUE),countryCount=length(unique(Destination.Country)))

df$class <- as.factor(df$Appealing.Agency.NATIONAL.INTERNATIONAL)

library(nnet)
test <- multinom(class~donorCount+totalContrib+countryCount,df)

summary(test)

z <- summary(test)$coefficients/summary(test)$standard.errors
z

# 2-tailed z test
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

# Predictions
head(pp <- fitted(test))

dses <- data.frame(ses = c("low", "middle", "high"), write = mean(df$totalContrib))
predict(test, newdata = dses, "probs")
