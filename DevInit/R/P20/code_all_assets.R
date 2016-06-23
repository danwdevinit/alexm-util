####Function and setup####
library(Hmisc)
library(plyr)
library(foreign)
library(data.table)

setwd("D:/Documents/Data/DHSmeta/")
classes <- read.csv("global_cwi_classes.csv",na.strings=c("","NAN"),as.is=TRUE)

assets <- function(hrwd){
  if(!file_test(op="-d", hrwd)){message("HR WD invalid");return(NA);}
  
  hrBase <- basename(hrwd)
  iso2 <- toupper(substr(hrBase,1,2))
  phase <- substr(hrBase,5,6)
  
  toilets.classes <- subset(classes,filename==hrBase & type=="toilets")
  water.classes <- subset(classes,filename==hrBase & type=="water")
  floor.classes <- subset(classes,filename==hrBase & type=="floor")
  wall.classes <- subset(classes,filename==hrBase & type=="wall")
  if(nrow(wall.classes)==0){stop("Missing from codebook!")}
  if(nrow(water.classes)==0){stop("Missing from codebook!")}
  if(nrow(floor.classes)==0){stop("Missing from codebook!")}
  if(nrow(toilets.classes)==0){stop("Missing from codebook!")}
  
  hr <- read.csv(paste0(hrwd,"/",iso2,"HR",phase,"FL.csv")
                 ,na.strings="",as.is=TRUE,check.names=FALSE)
  
  #Rename car/truck var
  names(hr)[which(names(hr)=="hv212")] <- "car"
  if(typeof(hr$car)=="NULL" | typeof(hr$car)=="logical" | length(hr$car[which(!is.na(hr$car))])==0){message("Car missing!");car.missing<-TRUE}else{car.missing<-FALSE}
  
  #Rename fridge var
  names(hr)[which(names(hr)=="hv209")] <- "fridge"
  if(typeof(hr$fridge)=="NULL" | typeof(hr$fridge)=="logical" | length(hr$fridge[which(!is.na(hr$fridge))])==0){message("Fridge missing!");fridge.missing<-TRUE}else{fridge.missing<-FALSE}
  
  #Rename phone var
  names(hr)[which(names(hr)=="hv221")] <- "phone"
  if(typeof(hr$phone)=="NULL" | typeof(hr$phone)=="logical" | length(hr$phone[which(!is.na(hr$phone))])==0){message("Phone missing!");phone.missing<-TRUE}else{phone.missing<-FALSE}
  
  #Rename tv var
  names(hr)[which(names(hr)=="hv208")] <- "tv"
  if(typeof(hr$tv)=="NULL" | typeof(hr$tv)=="logical" | length(hr$tv[which(!is.na(hr$tv))])==0){message("TV missing!");tv.missing<-TRUE}else{tv.missing<-FALSE}
  
  #Rename wall var
  names(hr)[which(names(hr)=="hv214")] <- "wall"
  if(typeof(hr$wall)=="NULL"){message("Missing wall!");hr$wall<-NA}
  
  #Rename floor var
  names(hr)[which(names(hr)=="hv213")] <- "floor"
  if(typeof(hr$floor)=="NULL"){message("Missing floor!");hr$floor<-NA}
  
  #Rename sleeping rooms var
  if(typeof(hr$hv216)=="NULL" | typeof(hr$hv216)=="logical" | length(hr$hv216[which(!is.na(hr$hv216))])==0){
    if(typeof(hr$sh40)=="NULL" | typeof(hr$sh40)=="logical" | length(hr$sh40[which(!is.na(hr$sh40))])==0){
      hr$sleeping.rooms <- NA
    }else{
      names(hr)[which(names(hr)=="sh40")] <- "sleeping.rooms"
      hr[which(hr$sleeping.rooms==99),] <- NA  
    }
  }else{
    names(hr)[which(names(hr)=="hv216")] <- "sleeping.rooms"
    hr[which(hr$sleeping.rooms==99),] <- NA 
  }
  
  #Rename members var
  names(hr)[which(names(hr)=="hv009")] <- "members"
  
  #Rename drinking water var
  names(hr)[which(names(hr)=="hv201")] <- "water"
  if(typeof(hr$water)=="NULL"){message("Missing water!");hr$water<-NA}
  
  #Rename toilets var
  names(hr)[which(names(hr)=="hv205")] <- "toilets"
  if(typeof(hr$toilets)=="NULL"){message("Missing toilets!");hr$toilets<-NA}
  
  #Rename share toilets var
  names(hr)[which(names(hr)=="hv225")] <- "share.toilets"
  if(typeof(hr$share.toilets)=="NULL" | typeof(hr$share.toilets)=="logical" | length(hr$share.toilets[which(!is.na(hr$share.toilets))])==0){share.toilets.missing<-TRUE}else{share.toilets.missing<-FALSE}
  
  
  #Rename cluster/hh var
  names(hr)[which(names(hr)=="hv001")] <- "cluster"
  names(hr)[which(names(hr)=="hv002")] <- "household"
  
  #Recode HR level vars
  hr <- transform(hr
                  ,crowded = ((members/sleeping.rooms)>3)
  )
  
  recode.wall <- function(x){
    item <- subset(wall.classes,value==tolower(x))
    if(nrow(item)==0){return(NA)}
    else{item$inadequate[1]}
  }
  hr$inade.wall <- sapply(hr$wall,recode.wall)
  
  recode.floor <- function(x){
    item <- subset(floor.classes,value==tolower(x))
    if(nrow(item)==0){return(NA)}
    else{item$inadequate[1]}
  }
  hr$inade.floor <- sapply(hr$floor,recode.floor)
  
  code.inade.water <- function(waterV){
    inade.water <- c()
    for(i in 1:length(waterV)){
      water <- tolower(waterV[i])
      item <- subset(water.classes,value==water)
      if(nrow(item)==0){
        inade.water <- c(inade.water,NA)
      }else{
        inade.water <- c(inade.water,item$rural.inadequate[1])
      }
    }
    return(inade.water)
  }
  
  hr$inade.water <- code.inade.water(hr$water)
  
  code.toilets <- function(toiletsV,share.toiletsV,share.toilets.missing){
    inade.toilets <- c()
    for(i in 1:length(toiletsV)){
      toilets <- tolower(toiletsV[i])
      share.toilets <- tolower(share.toiletsV[i])
      item <- subset(toilets.classes,value==toilets)
      if(share.toilets.missing){
        share.toilets = 0
      }
      if(is.na(share.toilets)){
        share.toilets = 0
      }
      if(share.toilets==1 | share.toilets=="yes"){
        inade.toilet = 1
      }else{
        inade.toilet = item$inadequate[1]
      }
      inade.toilets <- c(inade.toilets,inade.toilet)
    }
    return(inade.toilets)
  }
  hr$inade.toilets <- code.toilets(hr$toilets,hr$share.toilets,share.toilets.missing)
  
  recode.asset <- function(x){
    if(is.null(x)){return(NA)}
    else if(is.na(x) | x==9){return(NA)}
    else if(x==1 | tolower(x)=="yes"){return(1)}
    else if(x==0 | tolower(x)=="no"){return(0)}
    else{return(NA)}
  }
  
  ###Replication method
  #Calc wealth where half of households own tv
  if(!(tv.missing)){
    hr$tv <- sapply(hr$tv,recode.asset)
  }
  
  #Calc wealth where half of households own fridge
  if(!(fridge.missing)){
    hr$fridge <- sapply(hr$fridge,recode.asset)
  }
  
  #Calc wealth where half of households own car
  if(!(car.missing)){
    hr$car <- sapply(hr$car,recode.asset)
  }
  
  #Calc wealth where half of households own phone
  if(!(phone.missing)){
    hr$phone <- sapply(hr$phone,recode.asset)
  }
  
  
  keep = c("household","cluster","inade.wall","inade.floor","members","sleeping.rooms","crowded","inade.water","inade.toilets","tv","phone","car","fridge")
  hrNames <- names(hr)
  namesDiff <- setdiff(keep,hrNames)
  if(length(namesDiff)>0){
    for(y in 1:length(namesDiff)){
      hr[namesDiff[y]] <- NA
      message(paste("Missing variable",namesDiff[y]))
    } 
  }
  data <- hr[keep]
  data$filename <- hrBase
  
  return(data)
}
####Run function####
# set our working directory, change this if using on another machine
wd <- "D:/Documents/Data/MICSmeta"
setwd(wd)

countryMeta <- read.csv("headcounts.csv",as.is=TRUE)

wd <- "D:/Documents/Data/DHSauto/"
setwd(wd)

# List out all the directories in our wd, this is where our data is contained
dirs <- list.dirs(wd,full.names=TRUE)

dataList <- list()
dataIndex <- 1

# Loop through every dir
for(i in 2:length(dirs)){
  dir <- dirs[i]
  # Pull some coded info out of the dir name
  country <- tolower(substr(basename(dir),1,2))
  recode <- tolower(substr(basename(dir),3,4))
  phase <- as.integer(substr(basename(dir),5,5))
  # For this analysis, we're only interested in individual member recodes, or "hr"
  if(basename(dir) %in% countryMeta$filename){
    message(basename(dir))
    data <- assets(dir) 
    if(length(data)>1){
      dataList[[dataIndex]] <- data
      dataIndex <- dataIndex + 1 
    }
  }
}

####Function and setup####
library(Hmisc)
library(plyr)
library(foreign)
library(data.table)

setwd("D:/Documents/Data/MICSmeta")
varNames <- read.csv("mics_meta_vars_complete.csv",as.is=TRUE,na.strings="")
classes <- read.csv("global_mics_classes.csv",as.is=TRUE,na.strings="NAN")

mics.assets <- function(hh){
  hr <- data.frame(hh,as.is=TRUE,check.names=FALSE)
  hrBase <- hh$filename
  
  file.varName <- subset(varNames,filename==hrBase)
  
  share.toiletsVar <- subset(file.varName,match=="share.toilets")$varName
  toiletsVar <- subset(file.varName,match=="toilets")$varName
  
  carVar <- subset(file.varName,match=="car")$varName
  fridgeVar <- subset(file.varName,match=="fridge")$varName
  phoneVar <- subset(file.varName,match=="phone")$varName
  tvVar <- subset(file.varName,match=="tv")$varName
  
  #Exit function if really low MICS phase
  if(typeof(hr$hh1)=="NULL"){
    return(NA)
  }
  
  toilets.classes <- subset(classes,filename==hrBase & type=="toilets")
  water.classes <- subset(classes,filename==hrBase & type=="water")
  floor.classes <- subset(classes,filename==hrBase & type=="floor")
  wall.classes <- subset(classes,filename==hrBase & type=="wall")
  ynm.classes <- subset(classes,filename==hrBase & type=="ynm")
  if(nrow(wall.classes)==0){stop("Missing from codebook!")}
  if(nrow(water.classes)==0){stop("Missing from codebook!")}
  if(nrow(floor.classes)==0){stop("Missing from codebook!")}
  if(nrow(toilets.classes)==0){stop("Missing from codebook!")}
  missing.vals <- subset(ynm.classes,is.na(ynm))$value
  no.vals <- subset(ynm.classes,ynm==0)$value
  yes.vals <- subset(ynm.classes,ynm==1)$value
  
  #Exit function if really low MICS phase
  if(typeof(hr$hh1)=="NULL"){
    return(NA)
  }

  #check car/truck var
  if(length(carVar)<=0){message("Car missing!");car.missing<-TRUE}else{car.missing<-FALSE}
  
  #check fridge var
  if(length(fridgeVar)<=0){message("Fridge missing!");fridge.missing<-TRUE}else{fridge.missing<-FALSE}
  
  #check phone var
  if(length(phoneVar)<=0){message("Phone missing!");phone.missing<-TRUE}else{phone.missing<-FALSE}
  
  #check tv var
  if(length(tvVar)<=0){message("TV missing!");tv.missing<-TRUE}else{tv.missing<-FALSE}
  
  #Rename wall var
  names(hr)[which(names(hr)=="hc5")] <- "wall"
  if(typeof(hr$wall)=="NULL"){message("No wall!");hr$wall<-NA}
  
  #Rename floor var
  names(hr)[which(names(hr)=="hc3")] <- "floor"
  if(typeof(hr$floor)=="NULL"){message("No floor!");hr$floor<-NA}
  
  #Rename drinking water var
  names(hr)[which(names(hr)=="ws1")] <- "water"
  if(typeof(hr$water)=="NULL"){message("No water!");hr$water<-NA}
  
  #Rename toilets var
  names(hr)[which(names(hr)==toiletsVar)] <- "toilets"
  if(typeof(hr$toilets)=="NULL"){message("No toilets!");hr$toilets<-NA}
  
  #Rename share toilets var
  names(hr)[which(names(hr)==share.toiletsVar)] <- "share.toilets"
  if(typeof(hr$share.toilets)=="NULL" | typeof(hr$share.toilets)=="logical" | length(hr$share.toilets[which(!is.na(hr$share.toilets))])==0){share.toilets.missing<-TRUE}else{share.toilets.missing<-FALSE}
  
  #Rename sleeping rooms var
  names(hr)[which(names(hr)=="hc2")] <- "sleeping.rooms"
  if(typeof(hr$sleeping.rooms)=="NULL"){message("No sleeping.rooms!");hr$sleeping.rooms<-NA}
  
  #Rename members var
  names(hr)[which(names(hr)=="hh11")] <- "members"
  names(hr)[which(names(hr)=="hh1")] <- "cluster"
  names(hr)[which(names(hr)=="hh2")] <- "household"
  
  hr <- transform(hr
                  ,crowded = ((members/sleeping.rooms)>3)
  )
  
  recode.wall <- function(x){
    item <- subset(wall.classes,value==tolower(x))
    if(nrow(item)==0){return(NA)}
    else{item$inadequate[1]}
  }
  hr$inade.wall <- sapply(hr$wall,recode.wall)
  
  recode.floor <- function(x){
    item <- subset(floor.classes,value==tolower(x))
    if(nrow(item)==0){return(NA)}
    else{item$inadequate[1]}
  }
  hr$inade.floor <- sapply(hr$floor,recode.floor)
  
  code.inade.water <- function(waterV){
    inade.water <- c()
    for(i in 1:length(waterV)){
      water <- tolower(waterV[i])
      item <- subset(water.classes,value==water)
      if(nrow(item)==0){
        inade.water <- c(inade.water,NA)
      }else{
        inade.water <- c(inade.water,item$rural.inadequate[1])
      }
    }
    return(inade.water)
  }
  
  hr$inade.water <- code.inade.water(hr$water)
  
  code.toilets <- function(toiletsV,share.toiletsV,share.toilets.missing){
    inade.toilets <- c()
    for(i in 1:length(toiletsV)){
      toilets <- tolower(toiletsV[i])
      share.toilets <- tolower(share.toiletsV[i])
      item <- subset(toilets.classes,value==toilets)
      if(share.toilets.missing){
        share.toilets = 0
      }
      if(is.na(share.toilets)){
        share.toilets = 0
      }
      if(share.toilets %in% missing.vals){
        share.toilets = 0
      }
      if(share.toilets %in% yes.vals){
        inade.toilet = 1
      }else{
        inade.toilet = item$inadequate[1]
      }
      inade.toilets <- c(inade.toilets,inade.toilet)
    }
    return(inade.toilets)
  }
  hr$inade.toilets <- code.toilets(hr$toilets,hr$share.toilets,share.toilets.missing)
  
  recode.asset <- function(xV,x1V=rep(NA),x2V=rep(NA)){
    result <- c()
    for(i in 1:length(xV)){
      x <- tolower(xV[i])
      x1 <- tolower(x1V[i])
      if(length(x1)<=0){x1 = rep(NA)}
      x2 <- tolower(x2V[i])
      if(length(x2)<=0){x2 = rep(NA)}
      
      if(x %in% missing.vals & x1 %in% missing.vals & x2 %in% missing.vals){
        result <- c(result,NA)
      }else{
        result <- c(result,min(sum(x %in% yes.vals,x1 %in% yes.vals,x2 %in% yes.vals,na.rm=TRUE),1))
      }
    }
    return(result)
  }
  
  ###Replication method
  #Calc wealth where half of households own tv
  if(!(tv.missing)){
    hr$tv <- recode.asset(hr[[tvVar[1]]],hr[[tvVar[2]]],hr[[tvVar[3]]])
  }
  
  #Calc wealth where half of households own fridge
  if(!(fridge.missing)){
    hr$fridge <- recode.asset(hr[[fridgeVar[1]]],hr[[fridgeVar[2]]],hr[[fridgeVar[3]]])
  }
  
  #Calc wealth where half of households own car
  if(!(car.missing)){
    hr$car <- recode.asset(hr[[carVar[1]]],hr[[carVar[2]]],hr[[carVar[3]]])
  }
  
  #Calc wealth where half of households own phone
  if(!(phone.missing)){
    hr$phone <- recode.asset(hr[[phoneVar[1]]],hr[[phoneVar[2]]],hr[[phoneVar[3]]])
  }
  
  keep = c("household","cluster","inade.wall","inade.floor","members","sleeping.rooms","crowded","inade.water","inade.toilets","tv","phone","car","fridge")
  hrNames <- names(hr)
  namesDiff <- setdiff(keep,hrNames)
  if(length(namesDiff)>0){
    for(y in 1:length(namesDiff)){
      hr[namesDiff[y]] <- NA
      message(paste("Missing variable",namesDiff[y]))
    } 
  }
  data <- hr[keep]
  data$filename <- hrBase
  
  return(data)
}
####Run function####

# set our working directory, change this if using on another machine
wd <- "D:/Documents/Data/MICSauto/"
setwd(wd)

# List out all the directories in our wd, this is where our data is contained
dirs <- list.dirs(wd,full.names=TRUE)

for(i in 2:length(dirs)){
  dir <- dirs[i]
  if(basename(dir) %in% countryMeta$filename){
    if(exists("hh")){rm(hh)}
    if(exists("hl")){rm(hl)}
    load(paste0(dir,"/","hh.RData"))
    load(paste0(dir,"/","hl.RData"))
    names(hh) <- tolower(names(hh))
    names(hl) <- tolower(names(hl))
    if(typeof(hh$hh1)!="NULL"){
      message(basename(dir))
      data <- mics.assets(hh)
      if(length(data)>1){
        dataList[[dataIndex]] <- data
        dataIndex <- dataIndex + 1 
      }
    } 
  }
}

total.assets <- rbindlist(dataList,fill=TRUE)

wd <- "D:/Documents/Data/ChinaSurvey/"
setwd(wd)

load("dat2012.RData")
load("wealth.RData")

hr <- dat

#Rename car/truck var
names(hr)[which(names(hr)=="Automobile")] <- "car"
if(typeof(hr$car)=="NULL" | typeof(hr$car)=="logical" | length(hr$car[which(!is.na(hr$car))])==0){message("Car missing!");car.missing<-TRUE}else{car.missing<-FALSE}

#Rename fridge var
names(hr)[which(names(hr)=="Refrigerator.Freezer")] <- "fridge"
if(typeof(hr$fridge)=="NULL" | typeof(hr$fridge)=="logical" | length(hr$fridge[which(!is.na(hr$fridge))])==0){message("Fridge missing!");fridge.missing<-TRUE}else{fridge.missing<-FALSE}

#Rename phone var
names(hr)[which(names(hr)=="Mobile.phone")] <- "phone"
if(typeof(hr$phone)=="NULL" | typeof(hr$phone)=="logical" | length(hr$phone[which(!is.na(hr$phone))])==0){message("Phone missing!");phone.missing<-TRUE}else{phone.missing<-FALSE}

#Rename tv var
names(hr)[which(names(hr)=="TV")] <- "tv"
if(typeof(hr$tv)=="NULL" | typeof(hr$tv)=="logical" | length(hr$tv[which(!is.na(hr$tv))])==0){message("TV missing!");tv.missing<-TRUE}else{tv.missing<-FALSE}

#Rename members var
names(hr)[which(names(hr)=="familysize")] <- "members"

#Rename drinking water var
names(hr)[which(names(hr)=="fb1")] <- "water"
if(typeof(hr$water)=="NULL"){message("Missing water!");hr$water<-NA}

#Rename toilets var
names(hr)[which(names(hr)=="fb7")] <- "toilets"
if(typeof(hr$toilets)=="NULL"){message("Missing toilets!");hr$toilets<-NA}

names(hr)[which(names(hr)=="cid")] <- "cluster"
names(hr)[which(names(hr)=="fid12")] <- "household"


code.crowded <- function(fb6_s_1V,fb6_s_2V,fb6_s_3V){
  crowdedV <- c()
  inadequate.vals <- c(
    "Children aged over 12 live in the same room with the parents"
    ,"Family members of three generations live in the same room"
    ,"Children of different genders aged over 12 live in the same room"
    ,"Beds are laid out at night and folded up during the daytime"
    ,"Beds are laid out in the living room"
  )
  for(i in 1:length(fb6_s_1V)){
    fb6_s_1 <- fb6_s_1V[i]
    fb6_s_2 <- fb6_s_2V[i]
    fb6_s_3 <- fb6_s_3V[i]
    if(fb6_s_1 %in% inadequate.vals |fb6_s_2 %in% inadequate.vals | fb6_s_3 %in% inadequate.vals){
      crowded <- 1
    }else{
      crowded <- 0
    }
    crowdedV <- c(crowdedV,crowded)
  }
  return(crowdedV)
}

hr$crowded <- code.crowded(hr$fb6_s_1,hr$fb6_s_2,hr$fb6_s_2)

rural.inade.waters <- c(
  "River/Lake water"
  ,"Rainwater"
  ,"Cellar water"
  ,"Pond water"
)
missing.water <- c("Other [Please specify]",NA)

code.inade.water <- function(waterV){
  inade.water <- c()
  for(i in 1:length(waterV)){
    water <- waterV[i]
    if(water %in% missing.water){
      inade.water <- c(inade.water,NA)
    }else{
      inade.water <- c(inade.water,water %in% rural.inade.waters)
    }
  }
  return(inade.water)
}

hr$inade.water <- code.inade.water(hr$water)

inade.toilets <- c(
  "Outdoor public flush toilet"
  ,"Outdoor public non-flush toilet"
)
missing.toilets <- c(NA,"NA","Unknown","Other [Please specify]")

code.toilets <- function(toiletsV){
  inade.toiletsV <- c()
  for(i in 1:length(toiletsV)){
    toilets <- toiletsV[i]
    if(toilets %in% missing.toilets){
      inade.toilet <- NA
    }else if(toilets %in% inade.toilets){
      inade.toilet <- 1
    }else{
      inade.toilet <- 0
    }
    inade.toiletsV <- c(inade.toiletsV,inade.toilet)
  }
  return(inade.toiletsV)
}
hr$inade.toilets <- code.toilets(hr$toilets)

keep = c("household","cluster","inade.wall","inade.floor","members","sleeping.rooms","crowded","inade.water","inade.toilets","tv","phone","car","fridge")
hrNames <- names(hr)
namesDiff <- setdiff(keep,hrNames)
if(length(namesDiff)>0){
  for(y in 1:length(namesDiff)){
    hr[namesDiff[y]] <- NA
    message(paste("Missing variable",namesDiff[y]))
  } 
}
data <- hr[keep]
data$filename <- "China"

total.assets <- rbind(total.assets,data,fill=TRUE)


wd <- "D:/Documents/Data/MICSmeta"
setwd(wd)
save(total.assets,file="total.assets.RData")
