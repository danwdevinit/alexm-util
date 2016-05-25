####Function and setup####
library(Hmisc)
library(plyr)
library(foreign)
library(data.table)

setwd("D:/Documents/Data/DHSmeta/")
classes <- read.csv("global_cwi_classes.csv",na.strings=c("","NAN"),as.is=TRUE)

tl.cuts <- read.csv("D:/Documents/Data/DHSauto/tlhr61dt/cuts.csv")$cuts

cwi <- function(hrwd){
  if(!file_test(op="-d", hrwd)){message("HR WD invalid");return(NA);}
  
  hrBase <- basename(hrwd)
  iso2 <- toupper(substr(hrBase,1,2))
  phase <- substr(hrBase,5,6)
  
  toilets.classes <- subset(classes,filename==hrBase & type=="toilets")
  water.classes <- subset(classes,filename==hrBase & type=="water")
  floor.classes <- subset(classes,filename==hrBase & type=="floor")
  wall.classes <- subset(classes,filename==hrBase & type=="wall")
  if(nrow(wall.classes)==0){message("Missing from codebook!");return(hrBase)}
  if(nrow(water.classes)==0){message("Missing from codebook!");return(hrBase)}
  if(nrow(floor.classes)==0){message("Missing from codebook!");return(hrBase)}
  if(nrow(toilets.classes)==0){message("Missing from codebook!");return(hrBase)}
  return(NA)
}
####Run function####
# set our working directory, change this if using on another machine
wd <- "D:/Documents/Data/DHSauto/"
setwd(wd)

# List out all the directories in our wd, this is where our data is contained
dirs <- list.dirs(wd,full.names=TRUE)

dataList <- c()

# Loop through every dir
for(i in 2:length(dirs)){
  # for(i in 1209:length(dirs)){
  dir <- dirs[i]
  # Pull some coded info out of the dir name
  country <- tolower(substr(basename(dir),1,2))
  recode <- tolower(substr(basename(dir),3,4))
  phase <- as.integer(substr(basename(dir),5,5))
  # For this analysis, we're only interested in individual member recodes, or "hr"
  if(recode=="hr" & phase>=5){
    message(basename(dir))
    #     files <- c()
    cwi.missing <- cwi(dir)
    if(!is.na(cwi.missing)){
      dataList <- c(dataList,cwi.missing)
    }
    }
  }

wd <- "D:/Documents/Data/DHSmeta"
setwd(wd)
write.csv(dataList,"cwi_missing.csv",row.names=FALSE,na="")