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

sum.sqr.diffs <- c()
dir.names <- c()

# Loop through every dir
for(i in 2:length(dirs)){
  dir <- dirs[i]
  # Pull some coded info out of the dir name
  country <- tolower(substr(basename(dir),1,2))
  recode <- tolower(substr(basename(dir),3,4))
  phase <- as.integer(substr(basename(dir),5,5))
  # For this analysis, we're only interested in individual member recodes, or "hr"
  if(recode=="hr" & phase>=5){
    message(basename(dir))
    files <- list.files(dir)
    if("cuts_urban.csv" %in% files){
      urban.cuts <- read.csv(paste0(dir,"/cuts_urban.csv"),na.strings="",as.is=TRUE)$cuts
      rural.cuts <- read.csv(paste0(dir,"/cuts_rural.csv"),na.strings="",as.is=TRUE)$cuts
      cut.diffs <- urban.cuts-rural.cuts
      sum.sqr.diff <- sum(cut.diffs*cut.diffs)
      sum.sqr.diffs <- c(sum.sqr.diffs,sum.sqr.diff)
      dir.name <- basename(dir)
      dir.names <- c(dir.names,dir.name)
    }
  }
}

names(sum.sqr.diffs) <- dir.names
sum.sqr.diffs <- sum.sqr.diffs[order(sum.sqr.diffs)]
sum.sqr.diffs
