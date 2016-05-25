####Function and setup####
library(Hmisc)
library(plyr)
library(foreign)
library(data.table)

setwd("D:/Documents/Data/DHSmeta/")
classes <- read.csv("global_cwi_classes.csv",na.strings=c("","NAN"),as.is=TRUE)

mpi <- function(hrwd){
  if(!file_test(op="-d", hrwd)){message("HR WD invalid");return(NA);}
  
  hrBase <- basename(hrwd)
  iso2 <- toupper(substr(hrBase,1,2))
  phase <- substr(hrBase,5,6)
  
  hr <- read.dta(paste0(hrwd,"/",iso2,"HR",phase,"FL.dta"))
  
  #Rename wealth var
  names(hr)[which(names(hr)=="hv271")] <- "wealth"
  hr$wealth <- hr$wealth/100000
  if(typeof(hr$wealth)=="NULL" | typeof(hr$wealth)=="logical" | length(hr$wealth[which(!is.na(hr$wealth))])==0){message("Wealth missing!");return(NA)}
  
  #Rename survey year var
  names(hr)[which(names(hr)=="hv007")] <- "year"
  
  #Rename sample.weights var
  names(hr)[which(names(hr)=="hv005")] <- "sample.weights"
  
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
  
  if(sum(car.missing,fridge.missing,phone.missing,tv.missing)>1){
    return(NA)
  }
  
  #Electricity, cooking fuel, Radio, bike, motorbike
  names(hr)[which(names(hr)=="hv206")] <- "electricity"
  names(hr)[which(names(hr)=="hv207")] <- "radio"
  names(hr)[which(names(hr)=="hv210")] <- "bicycle"
  names(hr)[which(names(hr)=="hv211")] <- "motorbike"
  names(hr)[which(names(hr)=="hv226")] <- "cooking.fuel"
  
  #Rename floor var
  names(hr)[which(names(hr)=="hv213")] <- "floor"
  if(typeof(hr$floor)=="NULL"){message("Missing floor!");hr$floor<-NA}
  
  #Rename drinking water var
  names(hr)[which(names(hr)=="hv201")] <- "water"
  if(typeof(hr$water)=="NULL"){message("Missing water!");hr$water<-NA}
  #Rename drinking water var
  names(hr)[which(names(hr)=="hv204")] <- "water.time"
  if(typeof(hr$water.time)=="NULL"){message("Missing water time!");hr$water.time<-NA}
  
  #Rename toilets var
  names(hr)[which(names(hr)=="hv205")] <- "toilets"
  if(typeof(hr$toilets)=="NULL"){message("Missing toilets!");hr$toilets<-NA}
  
  #Rename share toilets var
  names(hr)[which(names(hr)=="hv225")] <- "share.toilets"
  if(typeof(hr$share.toilets)=="NULL" | typeof(hr$share.toilets)=="logical" | length(hr$share.toilets[which(!is.na(hr$share.toilets))])==0){share.toilets.missing<-TRUE;hr$share.toilets<-NA}else{share.toilets.missing<-FALSE}
  
  names(hr)[which(names(hr)=="hv001")] <- "cluster"
  names(hr)[which(names(hr)=="hv002")] <- "household"
  
  recode.floor <- function(x){
    if((typeof(x)=="double" | typeof(x)=="integer") & !is.factor(x)){
      if(x<20){
        return(1)
      }else if(x==96){
        return(0)
      }else if(x==99){
        return(NA)
      }else{
        return(0)
      }
    }else{
      if(
        grepl("dirt",x,ignore.case=TRUE)
        | grepl("sand",x,ignore.case=TRUE)
        | grepl("dung",x,ignore.case=TRUE)
        | grepl("clay",x,ignore.case=TRUE)
        | grepl("earth",x,ignore.case=TRUE)
        | grepl("mud",x,ignore.case=TRUE)
      ){
        return(1)
      }else if(grepl("other",x,ignore.case=TRUE)){
        return(NA)
      }else if(grepl("missing",x,ignore.case=TRUE)){
        return(NA)
      }else{
        return(0)
      }
    }
  }
  hr$inade.floor <- sapply(hr$floor,recode.floor)
  
  water.classes <- subset(classes,filename==hrBase & type=="water")
  
  code.inade.water <- function(waterV,water.timeV){
    inade.water <- c()
    for(i in 1:length(waterV)){
      water.time <- water.timeV[i]
      if(is.na(water.time)){
        water.time=NA
      }else if(water.time==996){
        water.time=0
      }else if(water.time==998){
        water.time=NA
      }else if(water.time==999){
        water.time=NA
      }
      water <- tolower(waterV[i])
      item <- subset(water.classes,value==water)
      if(nrow(item)==0){
        if(is.na(water.time)){
          inade.water <- c(inade.water,NA)
        }else{
          inade.water <- c(inade.water,water.time>30)
        }
      }else{
        inade.water <- c(inade.water,
                         min(sum(item$rural.inadequate[1],water.time>30,na.rm=TRUE),1)
        )
      }
    }
    return(inade.water)
  }
  
  hr$inade.water <- code.inade.water(hr$water,hr$water.time)
  
  toilets.classes <- subset(classes,filename==hrBase & type=="toilets")
  
  code.toilets <- function(toiletsV,share.toiletsV,share.toilets.missing){
    inade.toilets <- c()
    for(i in 1:length(toiletsV)){
      toilets <- tolower(toiletsV[i])
      share.toilets <- tolower(share.toiletsV[i])
      item <- subset(toilets.classes,value==toilets)
      if(share.toilets.missing){
        share.toilets = 0
      }
      else if(is.na(share.toilets)){
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
  
  
  if(!(tv.missing)){
    hr$tv <- sapply(hr$tv,recode.asset)
  }
  
  if(!(fridge.missing)){
    hr$fridge <- sapply(hr$fridge,recode.asset)
  }
  
  if(!(car.missing)){
    hr$car <- sapply(hr$car,recode.asset)
  }
  
  if(!(phone.missing)){
    hr$phone <- sapply(hr$phone,recode.asset)
  }
  hr$electricity <- sapply(hr$electricity,recode.asset)
  hr$inade.electricity <- !hr$electricity
  hr$radio <- sapply(hr$radio,recode.asset)
  hr$bicycle <- sapply(hr$bicycle,recode.asset)
  hr$motorbike <- sapply(hr$motorbike,recode.asset)
  
  recode.all.assets <- function(radioV,tvV,phoneV,bicycleV,motorbikeV,fridgeV,carV){
    inade.assets <- c()
    for(i in 1:length(radioV)){
      radio <- radioV[i]
      tv <- tvV[i]
      bicycle <- bicycleV[i]
      motorbike <- motorbikeV[i]
      fridge <- fridgeV[i]
      car <- carV[i]
      inade.asset <- sum(radio,tv,bicycle,motorbike,fridge,na.rm=TRUE)<2 & car==0
      inade.assets <- c(inade.assets,inade.asset)
    }
    return(inade.assets)
  }
  hr$inade.assets <- recode.all.assets(hr$radio,hr$tv,hr$phone,hr$bicycle,hr$motorbike,hr$fridge,hr$car)
  
  recode.fuel <- function(x){
    if((typeof(x)=="double" | typeof(x)=="integer") & !is.factor(x)){
      if(x>=6 & x<=11){
        return(1)
      }else if(x==96){
        return(0)
      }else if(x==99){
        return(NA)
      }else{
        return(0)
      }
    }else{
      if(
        grepl("coal",x,ignore.case=TRUE)
        | grepl("wood",x,ignore.case=TRUE)
        | grepl("straw",x,ignore.case=TRUE)
        | grepl("dung",x,ignore.case=TRUE)
        | grepl("agri",x,ignore.case=TRUE)
        | grepl("grass",x,ignore.case=TRUE)
        | grepl("shrub",x,ignore.case=TRUE)
      ){
        return(1)
      }else if(grepl("other",x,ignore.case=TRUE)){
        return(NA)
      }else if(grepl("missing",x,ignore.case=TRUE)){
        return(NA)
      }else{
        return(0)
      }
    }
  }
  hr$inade.fuel <- sapply(hr$cooking.fuel,recode.fuel)
  
  hr <- transform(hr,
                  inade.living.standard = 
                    inade.electricity +
                    inade.toilets +
                    inade.water +
                    inade.floor +
                    inade.fuel +
                    inade.assets
  )
  
  hr <- transform(hr
                  ,ade.living.standard =
                    6-inade.living.standard)
  
  keep <- c(
    "cluster"
    ,"household"
    ,"wealth"
    ,"ade.living.standard"
    ,"inade.living.standard"
    ,"sample.weights"
    ,"inade.electricity"
    ,"inade.toilets"
    ,"inade.water"
    ,"inade.floor"
    ,"inade.fuel"
    ,"inade.assets"
  )
  
  data <- hr[keep]
  return(data)
}
####Run function####
# set our working directory, change this if using on another machine
wd <- "D:/Documents/Data/DHSauto/"
setwd(wd)

# List out all the directories in our wd, this is where our data is contained
dirs <- list.dirs(wd,full.names=TRUE)

dataList <- list()
dataIndex <- 1

# Loop through every dir
# for(i in 2:length(dirs)){
  for(i in 1383:length(dirs)){
  dir <- dirs[i]
  # Pull some coded info out of the dir name
  country <- tolower(substr(basename(dir),1,2))
  recode <- tolower(substr(basename(dir),3,4))
  phase <- as.integer(substr(basename(dir),5,5))
  # For this analysis, we're only interested in individual member recodes, or "hr"
  if(recode=="hr" & phase>=6){
    message(basename(dir))
      data <- mpi(dir) 
      dataList[[dataIndex]] <- data
      dataIndex <- dataIndex + 1 
  }
}

wd <- "D:/Documents/Data/DHSmeta"
setwd(wd)
metaData <- rbindlist(dataList,fill=TRUE)
write.csv(metaData,"global_mpi.csv",row.names=FALSE,na="")

metaData$weights <- metaData$sample.weights/1000000

weighted.percentile <- function(x,w,prob,na.rm=TRUE){
  df <- data.frame(x,w)
  if(na.rm){
    df <- df[which(complete.cases(df)),]
  }
  #Sort
  df <- df[order(df$x),]
  sumw <- sum(df$w)
  df$cumsumw <- cumsum(df$w)
  #For each percentile
  cutList <- c()
  cutNames <-c()
  for(i in 1:length(prob)){
    p <- prob[i]
    pStr <- paste0(round(p*100,digits=2),"%")
    sumwp <- sumw*p
    df$above.prob <- df$cumsumw>=sumwp
    thisCut <- df$x[which(df$above.prob==TRUE)[1]]
    cutList <- c(cutList,thisCut)
    cutNames <- c(cutNames,pStr)
  }
  names(cutList) <- cutNames
  return(cutList)
}

quints <- weighted.percentile(metaData$ade.living.standard,metaData$weights,prob=seq(0,1,length=6))

global.cwi <- read.csv("global_cwi.csv",na.strings="",as.is=TRUE)

global.cwi$living.standard <- metaData$ade.living.standard

write.csv(global.cwi,"mpi_and_cwi.csv",row.names=FALSE,na="")

fit <- lm(cwi~living.standard,data=global.cwi)
summary(fit)
library(ggplot2)
library(hexbin)
p <- ggplot(global.cwi,aes(x=living.standard,y=cwi)) + stat_binhex()
p
global.cwi$living.f <- factor(global.cwi$living.standard,levels=c(0:6))

plot(cwi~living.f,data=global.cwi)
cor(global.cwi$cwi,global.cwi$living.standard,use="na.or.complete")
