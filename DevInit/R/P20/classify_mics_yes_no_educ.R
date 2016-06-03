####Function and setup####
library(Hmisc)
library(plyr)
library(foreign)
library(data.table)

setwd("D:/Documents/Data/MICSmeta")

varNames <- read.csv("toiletsVars.csv",as.is=TRUE,na.strings="")
assetVars <- read.csv("assetVars.csv",as.is=TRUE,na.strings="NAN")

cwi.class <- function(hrwd){
  hrBase <- basename(hrwd)
  
  hr <- read.csv(paste0(hrwd,"/hh.csv"),as.is=TRUE,na.strings="",check.names=FALSE)
  
  #Exit function if really low MICS phase
  insufficient <- c("Trinidad and Tobago MICS 2006 SPSS Datasets","Madagascar (South)_ MICS4_Datasets")
  if(typeof(hr$hh1)=="NULL" | hrBase %in% insufficient){
    return(NA)
  }
  
  toiletVar <- subset(varNames,filename==hrBase)$toiletsVar
  if(is.na(toiletVar)){
    share.toilet.var=NA
  }else if(toiletVar=="ws8"){
    share.toilet.var="ws9"
  }else if(toiletVar=="ws7"){
    share.toilet.var="ws8"
  }else{
    share.toilet.var=NA
  }
  
  ir <- read.csv(paste0(hrwd,"/hl.csv"),as.is=TRUE,na.strings="",check.names=FALSE)
  
  #Rename urban var
  names(hr)[which(names(hr)=="hh6")] <- "urban.rural"
  
  #Rename car/truck var
  if(typeof(hr$hc10e)=="NULL"){
      carVar <- "hc9f"
      fridgeVar <- "hc8e"
      phoneVar <- "hc8d"
      tvVar <- "hc8c"
  }else{
    carVar <- "hc10e"
    fridgeVar <- "hc9f"
    phoneVar <- "hc9e"
    tvVar <- "hc9c"
  }

  if(hrBase %in% assetVars$filename){
    this.name <- subset(assetVars,filename==hrBase)[1,]
    #Ignore subvariants like trucks, color tv and freezers for now
    carVar <- this.name$carVar
    fridgeVar <- this.name$fridgeVar
    phoneVar <- this.name$phoneVar
    tvVar <- this.name$tvVar
  }

  missing.some <- 0
  
  #Rename car/truck var
  names(hr)[which(names(hr)==carVar)] <- "car"
  if(typeof(hr$car)=="NULL" | typeof(hr$car)=="logical" | length(hr$car[which(!is.na(hr$car))])==0){message("Car missing!");hr$car<-NA;missing.some<-1;}
  
  #Rename fridge var
  names(hr)[which(names(hr)==fridgeVar)] <- "fridge"
  if(typeof(hr$fridge)=="NULL" | typeof(hr$fridge)=="logical" | length(hr$fridge[which(!is.na(hr$fridge))])==0){message("Fridge missing!");hr$fridge<-NA;missing.some<-1;}
  
  #Rename phone var
  names(hr)[which(names(hr)==phoneVar)] <- "phone"
  if(typeof(hr$phone)=="NULL" | typeof(hr$phone)=="logical" | length(hr$phone[which(!is.na(hr$phone))])==0){message("Phone missing!");hr$phone<-NA;missing.some<-1;}
  
  #Rename tv var
  names(hr)[which(names(hr)==tvVar)] <- "tv"
  if(typeof(hr$tv)=="NULL" | typeof(hr$tv)=="logical" | length(hr$tv[which(!is.na(hr$tv))])==0){message("TV missing!");hr$tv<-NA;missing.some<-1;}
  
  
  #Rename share toilets var
  names(hr)[which(names(hr)==share.toilet.var)] <- "share.toilets"
  if(typeof(hr$share.toilets)=="NULL" | typeof(hr$share.toilets)=="logical" | length(hr$share.toilets[which(!is.na(hr$share.toilets))])==0){hr$share.toilets<-NA}
  
  nepals <- c("Nepal_MICS5_Datasets","Nepal (Mid-and Far-Western Regions)_MICS4_Datasets")
  if(hrBase %in% nepals){
    names(ir)[which(names(ir)=="ed3")] <- "attended"
    ir$school <- NA
    names(ir)[which(names(ir)=="ed4b")] <- "grade"
  }else if(hrBase=="Mongolia_MICS4_Datasets"){
    names(ir)[which(names(ir)=="ed3")] <- "attended"
    names(ir)[which(names(ir)=="ed4_a")] <- "school"
    names(ir)[which(names(ir)=="ed4_b")] <- "grade"
  }else{
    if(typeof(ir$ed4a)=="NULL" | typeof(ir$ed4a)=="logical" | length(ir$ed4a[which(!is.na(ir$ed4a))])==0){
      if(typeof(ir$ed3a)=="NULL" | typeof(ir$ed3a)=="logical" | length(ir$ed3b[which(!is.na(ir$ed3a))])==0){
        message("Educ missing!");educ.missing <- TRUE
      }else{
        names(ir)[which(names(ir)=="ed2")] <- "attended"
        names(ir)[which(names(ir)=="ed3a")] <- "school"
        names(ir)[which(names(ir)=="ed3b")] <- "grade"
      }
    }else{
      names(ir)[which(names(ir)=="ed3")] <- "attended"
      names(ir)[which(names(ir)=="ed4a")] <- "school"
      names(ir)[which(names(ir)=="ed4b")] <- "grade"
    } 
  }
  
  return(
    list(
      yes.no.missing = unique(tolower(c(ir$attended,hr$tv,hr$fridge,hr$car,hr$phone,hr$share.toilets)))
      ,school = unique(tolower(ir$school))
      ,urban.rural = unique(tolower(hr$urban.rural))
      ,missing = missing.some
    )
  )
}
####Run function####
# set our working directory, change this if using on another machine
wd <- "D:/Documents/Data/MICSauto/"
setwd(wd)

# List out all the directories in our wd, this is where our data is contained
dirs <- list.dirs(wd,full.names=TRUE)

yes.no.missing <- c()
school <- c()
urban.rural <- c()
missing.filenames <- c()

# Loop through every dir
for(i in 2:length(dirs)){
  dir <- dirs[i]
  message(basename(dir))
  dataList <- cwi.class(dir)
  if(!is.na(dataList)){
    yes.no.missing <- unique(c(yes.no.missing,dataList[["yes.no.missing"]]))
    school <- unique(c(school,dataList[["school"]]))
    urban.rural <- unique(c(urban.rural,dataList[["urban.rural"]]))
    if(dataList[["missing"]]==1){
      missing.filenames <- c(missing.filenames,basename(dir))
    }
  }
}

wd <- "D:/Documents/Data/MICSmeta"
setwd(wd)
write.csv(yes.no.missing,"yes.no.missing.csv",row.names=FALSE,na="")
write.csv(school,"school.csv",row.names=FALSE,na="")
write.csv(urban.rural,"urban.rural.csv",row.names=FALSE,na="")
write.csv(missing.filenames,"missing.filenames.csv",row.names=FALSE,na="")