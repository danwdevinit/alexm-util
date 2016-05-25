####Function and setup####
library(Hmisc)
library(plyr)
library(foreign)
library(data.table)

setwd("D:/Documents/Data/DHSmeta/")

cwi.rebase <- function(tl.cuts,cuts,data){
  cut.df <- data.frame(cuts,tl.cuts)
  cut.lm <- lm(tl.cuts~cuts)
  alpha <- cut.lm$coefficients[[1]]
  beta <- cut.lm$coefficients[[2]]
  data$cwi <- alpha+(beta*data$wealth)
  
  nameOrder = c("filename","iso2","year","household","cluster","sample.weights","cwi","wealth","ubn","inade.materials","crowded","inade.sani","hed","tv","phone","car","fridge")
  data <- data[nameOrder]
  
  return(
    data
  )
}
####Run function####
# set our working directory, change this if using on another machine
wd <- "D:/Documents/Data/DHSauto/"
setwd(wd)

# List out all the directories in our wd, this is where our data is contained
dirs <- list.dirs(wd,full.names=TRUE)

dataList <- list()
dataIndex <- 1

rebase_dir <- "cdhr61dt"
tl.cuts <- read.csv(paste0("D:/Documents/Data/DHSauto/",rebase_dir,"/cuts.csv"))$cuts

# Loop through every dir
for(i in 2:length(dirs)){
  # for(i in 1209:length(dirs)){
  dir <- dirs[i]
  # Pull some coded info out of the dir name
  country <- tolower(substr(basename(dir),1,2))
  recode <- tolower(substr(basename(dir),3,4))
  phase <- as.integer(substr(basename(dir),5,5))
  # For this analysis, we're only interested in individual member recodes, or "hr"
  if(recode=="hr" & phase>=6){
    files <- list.files(dir)
    if("cuts.csv" %in% files){
      message(basename(dir))
      cuts <- read.csv(paste0(dir,"/cuts.csv"),na.strings="",as.is=TRUE)$cuts
      data <- read.csv(paste0(dir,"/wealth.csv"),na.strings="",as.is=TRUE)
      dataList[[dataIndex]] <- cwi.rebase(tl.cuts,cuts,data)
      dataIndex <- dataIndex + 1 
    }
  }
}

wd <- "D:/Documents/Data/DHSmeta"
setwd(wd)
metaData <- rbindlist(dataList,fill=TRUE)
write.csv(metaData,paste0("global_cwi_base_",rebase_dir,".csv"),row.names=FALSE,na="")