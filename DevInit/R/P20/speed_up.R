####Function and setup####
library(Hmisc)
library(plyr)
library(foreign)
library(data.table)
####Run function####
# set our working directory, change this if using on another machine
wd <- "D:/Documents/Data/DHSauto/"
setwd(wd)

# List out all the directories in our wd, this is where our data is contained
dirs <- list.dirs(wd,full.names=TRUE)

# Loop through every dir
for(i in 2:length(dirs)){
  dir <- dirs[i]
  hrBase <- basename(dir)
  message(hrBase)
  files <- list.files(dir,"*.dta",ignore.case=TRUE,full.names=TRUE)
  if(length(files)>0){
    for(j in 1:length(files)){
      file <- files[j]
      fileBase <- basename(file)
      fileName <- substr(fileBase,1,nchar(fileBase)-4)
      csvs <- list.files(dir,"*.csv",ignore.case=TRUE)
      if(!(paste0(fileName,".csv") %in% csvs)){
        data <- read.dta(file)
        write.csv(data,paste0(dir,"/",fileName,".csv"),row.names=FALSE,na="")  
      }
    }
  }
}