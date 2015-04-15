###By Alex Miller

#Set the working directory
wd <- "S:/Projects/Programme resources/Data/GHA calcs and analyses/April 2015/Datasets - do not edit/CRS/"
setwd(wd)

#Define the datasets we want to work with
datasets <- c("CRS 2013 data.csv"
              ,"CRS 2012 data.csv"
              ,"CRS 2011 data.csv"
              ,"CRS 2010 data.csv"
              ,"CRS 2009 data.csv")
#Define the purposecodes we want to filter by
purposes <- c(72010,72040,72050,73010,74010)

#Iterate through the datasets
#Read the first one and append it to data
data <- read.csv(datasets[1],stringsAsFactors=FALSE,encoding="latin1")
#Filter it by purposes
data <- data[which(data$purposecode %in% purposes),]
#Read through the rest
for(i in 2:length(datasets)){
  dataset <- datasets[i]
  #Read it in
  dat <- read.csv(dataset,stringsAsFactors=FALSE,encoding="latin1")
  #Keep only those rows which purposecode == purposes
  dat <- dat[which(dat$purposecode %in% purposes),]
  #Append it to data
  data <- rbind(data,dat)
  #Repeat
}

#Export as csv
write.csv(data,"./Subsets/2009-2013 CRS Relevant Purposecodes Only.csv",row.names=FALSE,na="")
