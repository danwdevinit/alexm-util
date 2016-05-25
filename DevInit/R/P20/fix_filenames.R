# set our working directory, change this if using on another machine
wd <- "D:/Documents/Data/DHSauto/"
setwd(wd)

# List out all the directories in our wd, this is where our data is contained
dirs <- list.dirs(wd,full.names=TRUE)

# Loop through every dir
for(i in 2:length(dirs)){
  dir <- dirs[i]
  # Pull some coded info out of the dir name
  country <- tolower(substr(basename(dir),1,2))
  recode <- tolower(substr(basename(dir),3,4))
  phase <- as.integer(substr(basename(dir),5,5))
  # For this analysis, we're only interested in individual member recodes, or "hr"
  if(recode=="hr" & phase==6){
    message(basename(dir))
    if(file.exists(paste0(dir,"/cuts_means.csv"))){
      file.remove(paste0(dir,"/cuts_means.csv"))
      file.remove(paste0(dir,"/cuts.csv"))
      file.remove(paste0(dir,"/wealth_means.csv"))
      file.remove(paste0(dir,"/wealth.csv"))
      file.rename(paste0(dir,"/wealth_replication.csv"),paste0(dir,"/wealth.csv"))
      file.rename(paste0(dir,"/cuts_replication.csv"),paste0(dir,"/cuts.csv"))
    }
  }
}