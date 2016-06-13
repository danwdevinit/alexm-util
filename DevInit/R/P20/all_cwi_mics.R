####Function and setup####
library(Hmisc)
library(plyr)
library(foreign)
library(data.table)

setwd("D:/Documents/Data/MICSmeta")
varNames <- read.csv("mics_meta_vars_complete.csv",as.is=TRUE,na.strings="")
classes <- read.csv("global_mics_classes.csv",as.is=TRUE,na.strings="NAN")

tl.cuts <- read.csv("D:/Documents/Data/DHSauto/tlhr61dt/cuts.csv")$cuts

cwi <- function(hh,hl){
  hr <- data.frame(hh,as.is=TRUE,check.names=FALSE)
  ir <- data.frame(hl,as.is=TRUE,check.names=FALSE)
  hrBase <- hh$filename
  
  file.varName <- subset(varNames,filename==hrBase)
  
  attendedVar <- subset(file.varName,match=="attended")$varName
  gradeVar <- subset(file.varName,match=="grade")$varName
  schoolVar <- subset(file.varName,match=="school")$varName
  
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
  attended.classes <- subset(classes,filename==hrBase & type=="attended")
  urban.rural.classes <- subset(classes,filename==hrBase & type=="urban.rural")
  school.classes <- subset(classes,filename==hrBase & type=="school")
  if(nrow(wall.classes)==0){stop("Missing from codebook!")}
  if(nrow(water.classes)==0){stop("Missing from codebook!")}
  if(nrow(floor.classes)==0){stop("Missing from codebook!")}
  if(nrow(toilets.classes)==0){stop("Missing from codebook!")}
  missing.vals <- subset(ynm.classes,is.na(ynm))$value
  no.vals <- subset(ynm.classes,ynm==0)$value
  yes.vals <- subset(ynm.classes,ynm==1)$value
  
  missing.attended <- subset(attended.classes,is.na(attended))$value
  no.attended <- subset(attended.classes,attended==0)$value
  yes.attended <- subset(attended.classes,attended==1)$value
  
  missing.level <- subset(school.classes,is.na(level))$value
  none.level <- subset(school.classes,level=="none")$value
  preschool.level <- subset(school.classes,level=="preschool")$value
  primary.level <- subset(school.classes,level=="primary")$value
  secondary.level <- subset(school.classes,level=="secondary")$value
  higher.level <- subset(school.classes,level=="higher")$value
  
  
  #Exit function if really low MICS phase
  if(typeof(hr$hh1)=="NULL"){
    return(NA)
  }
    
  #Rename wealth var
  if(typeof(hr$wlthscor)=="NULL" | typeof(hr$wlthscor)=="logical" | length(hr$wlthscor[which(!is.na(hr$wlthscor))])==0){
    if(typeof(hr$wscore)=="NULL" | typeof(hr$wscore)=="logical" | length(hr$wscore[which(!is.na(hr$wscore))])==0){
      message("Wealth missing!");return(NA)
    }else{
      names(hr)[which(names(hr)=="wscore")] <- "wealth"
    }
  }else{
    names(hr)[which(names(hr)=="wlthscor")] <- "wealth"
  }


  #Rename survey year var
  names(hr)[which(names(hr)=="hh5y")] <- "year"
  
  #Rename sample.weights var
  names(hr)[which(names(hr)=="hhweight")] <- "sample.weights"
  
  #Rename urban var
  names(hr)[which(names(hr)=="hh6")] <- "urban.rural"
  if(typeof(hr$urban.rural)=="NULL"){message("No urban.rural!");hr$urban.rural<-NA;urban.missing<-TRUE}else{urban.missing<-FALSE}
  
  #check car/truck var
  if(length(carVar)<=0){message("Car missing!");car.missing<-TRUE}else{car.missing<-FALSE}
  
  #check fridge var
  if(length(fridgeVar)<=0){message("Fridge missing!");fridge.missing<-TRUE}else{fridge.missing<-FALSE}
  
  #check phone var
  if(length(phoneVar)<=0){message("Phone missing!");phone.missing<-TRUE}else{phone.missing<-FALSE}
  
  #check tv var
  if(length(tvVar)<=0){message("TV missing!");tv.missing<-TRUE}else{tv.missing<-FALSE}
  
  if(sum(car.missing,fridge.missing,phone.missing,tv.missing)>1){
    return(NA)
  }
  
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
  
  #Rename educ var
  names(ir)[which(names(ir)==attendedVar)] <- "attended"
  names(ir)[which(names(ir)==schoolVar)] <- "school"
  names(ir)[which(names(ir)==gradeVar)] <- "grade"
  
  #Rename age var
  names(ir)[which(names(ir)=="hl5")] <- "age"
  
  #Rename head var
  names(ir)[which(names(ir)=="hl3")] <- "head"
  
  
  #Rename cluster/hh var
  names(ir)[which(names(ir)=="hh1")] <- "cluster"
  names(ir)[which(names(ir)=="hh2")] <- "household"
  names(hr)[which(names(hr)=="hh1")] <- "cluster"
  names(hr)[which(names(hr)=="hh2")] <- "household"
  
  
  #Recode IR level to hr
  recode.educ <- function(attendedV,schoolV,gradeV){
    educV <- c()
    for(i in 1:length(attendedV)){
      attended <- tolower(attendedV[i])
      school <- tolower(schoolV[i])
      if(length(school)<=0){
        school <- NA
      }
      grade <- gradeV[i]
      ###Ignore factor grades for now... We need to code these out in the metavars
      if(is.factor(grade)){
        grade <- NA
      }
      if(!is.na(grade)){
        if(grade>90){grade<-NA}
      }
      if(attended %in% missing.attended){
        if(school %in% missing.level){
          if(is.na(grade)){
            #missing all three
            educ <- NA
          }else{
            #missing attended and level, but not grade
            if(grade>=5){
              educ <- 1
            }else{
              educ <- 0
            }
          }
        }else{
          #missing attended, but not level
          if(is.na(grade)){
            #has level, but not grade
            if(school %in% secondary.level | school %in% higher.level){
              educ <- 1
            }else if(school %in% preschool.level | school %in% none.level){
              educ <- 0
            }else{
              educ <- NA
            }
          }else{
            #missing attended and level, but not grade
            if(grade>=5){
              educ <- 1
            }else{
              educ <- 0
            }
          }
        }
      }else if(attended %in% no.attended){
        #No education
        educ <- 0
      }else{
        if(school %in% missing.level){
          if(is.na(grade)){
            #has attended, but has no level or grade
            educ <- NA
          }else{
            #has attended, missing level, but not missing grade
            if(grade>=5){
              educ <- 1
            }else{
              educ <- 0
            }
          }
        }else if(school %in% preschool.level | school %in% none.level){
          if(is.na(grade)){
            educ <- 0
          }else if(grade>=5){
            #Complete primary
            educ <- 1
          }else{
            educ <- 0
          }
        } else if(school %in% primary.level){
          if(is.na(grade)){
            educ <- NA
          }else if(grade<5){
            #Incomplete primary
            educ <- 0
          }else if(grade>=5){
            #Complete primary
            educ <- 1
          }else{
            educ <- NA
          }
        } else if(school %in% secondary.level){
          #(in)complete secondary
          educ <- 1
        } else if(school %in% higher.level){
          #(in)complete higher
          educ <- 1
        }else if(grade>=5){
          #at least 5 years of some other schooling
          educ <- 1
        }else if(grade<5){
          #not at least 5 years of some other schooling
          educ <- 0
        } else{
          #missing grade with preschool, primary, or other
          educ <- NA
        }
      }
      educV <- c(educV,educ)
    }
    return(educV)
  }

  ir$educ <- recode.educ(ir$attended,ir$school,ir$grade)
  
  members <- hr[c("cluster","household","members")]
  
  ir <- join(
    ir
    ,members
    ,by=c("cluster","household")
  )
  
  calc.hed <- function(membersV,adults.completed.primaryV){
    hedV <- c()
    for(i in 1:length(membersV)){
      members <- membersV[i]
      workers <- 1
      adults.completed.primary <- adults.completed.primaryV[i]
      
      hed = ((members/workers)>3 & adults.completed.primary==0)
      hedV <- c(hedV,hed)
    }
    return(hedV)
  }
  
  ir.table <- data.table(ir)
  hed <- ir.table[,.(
    members=mean(members,na.rm=TRUE)
    ,adults.completed.primary=sum(educ==1,na.rm=TRUE)
  ), by=.(cluster,household)]
  
  hed <- transform(hed
                   ,hed = calc.hed(members,adults.completed.primary)
  )

  hed <- data.frame(hed)
  
  #Recode HR level vars
  keep <- c("cluster","household","hed")
  hed <- hed[keep]
  hr <- join(
    hr
    ,hed
    ,by=c("cluster","household")
  )
  
  ###One worker assumption for households outside of IR
  ###Skipping for MICS now, since helevel/hhlevel can vary   
#   calc.hed.hr <- function(df){
#     hedV <- c()
#     for(i in 1:nrow(df)){
#       members <- df$members[i]
#       head.educ <- tolower(df$helevel[i])
#       workers <- 1
#       adults.completed.primary <- 0
#       
#       if(
#         head.educ %in% secondary.level | head.educ %in% higher.level
#          ){adults.completed.primary <- 1}
#       
#       hed = ((members/workers)>3 & adults.completed.primary==0)
#       hedV <- c(hedV,hed)
#     }
#     return(hedV)
#   }
#   hr[which(is.na(hr$hed)),]$hed <- calc.hed.hr(hr[which(is.na(hr$hed)),])
  
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
  
  code.materials <- function(inade.walls,inade.floors){
    inade.materials <- c()
    for(i in 1:length(inade.walls)){
      inade.wall <- inade.walls[i]
      inade.floor <- inade.floors[i]
      if(is.na(inade.wall) & is.na(inade.floor)){
        inade.material <- NA
      }else{
        inade.material <- min(sum(inade.wall,inade.floor,na.rm=TRUE),1) 
      }
      inade.materials <- c(inade.materials,inade.material)
    }
    return(inade.materials)
  }
  
  hr$inade.materials <- code.materials(hr$inade.wall,hr$inade.floor)
  
  hr <- transform(hr
                  ,crowded = ((members/sleeping.rooms)>3)
  )
  
  recode.urban.rural <- function(x){
    item <- subset(urban.rural.classes,value==tolower(x))
    if(nrow(item)==0){return(NA)}
    else{item$urban[1]}
  }
  hr$urban <- sapply(hr$urban.rural,recode.urban.rural)
  
  code.inade.water <- function(urbanV,waterV){
    inade.water <- c()
    for(i in 1:length(urbanV)){
      urban <- urbanV[i]
      water <- tolower(waterV[i])
      item <- subset(water.classes,value==water)
      if(nrow(item)==0){
        inade.water <- c(inade.water,NA)
      }else{
        if(is.na(urban)){
          #Assume stricter codebook?
          inade.water <- c(inade.water,item$urban.inadequate[1])
        }else if(urban==1){
          inade.water <- c(inade.water,item$urban.inadequate[1])
        }else if(urban==0){
          inade.water <- c(inade.water,item$rural.inadequate[1])
        }else{
          inade.water <- c(inade.water,NA)
        } 
      }
    }
    return(inade.water)
  }
  
  hr$inade.water <- code.inade.water(hr$urban,hr$water)
  
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
  
  combine.sani <- function(toiletV,waterV){
    inade.sani <- c()
    for(i in 1:length(toiletV)){
      toilet <- toiletV[i]
      water <- waterV[i]
      if(is.na(toilet) & is.na(water)){
        inade.sani <- c(inade.sani,NA)
      }else{
        inade.sani <- c(inade.sani,min(sum(toilet,water,na.rm=TRUE),1))
      }
    }
    return(inade.sani)
  }
  
  hr$inade.sani <- combine.sani(hr$inade.toilet,hr$inade.water)
  
  #Finally get to calc the UBN!
  calc.ubn <- function(inade.materialsV,crowdedV,inade.saniV,hedV){
    ubnV <- c()
    for(i in 1:length(inade.materialsV)){
      inade.materials <- inade.materialsV[i]
      crowded <- crowdedV[i]
      inade.sani <- inade.saniV[i]
      hed <- hedV[i]
      ubn <- sum(inade.materials,crowded,inade.sani,hed,na.rm=TRUE)
      ubnV <- c(ubnV,ubn)
    }
    return(ubnV)
  }
  hr$ubn <- calc.ubn(hr$inade.materials,hr$crowded,hr$inade.sani,hr$hed)
  
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
  
  
  #Generate cutpoint dfs
  cuts <- c(
    mean(hr[which(hr$car==1),]$wealth,na.rm=TRUE)
    ,mean(hr[which(hr$fridge==1),]$wealth,na.rm=TRUE)
    ,mean(hr[which(hr$phone==1),]$wealth,na.rm=TRUE)
    ,mean(hr[which(hr$tv==1),]$wealth,na.rm=TRUE)
    ,mean(hr[which(hr$ubn>=1),]$wealth,na.rm=TRUE)
    ,mean(hr[which(hr$ubn>=2),]$wealth,na.rm=TRUE)
    ,mean(hr[which(hr$ubn>=3),]$wealth,na.rm=TRUE)
    ,mean(hr[which(hr$ubn>=4),]$wealth,na.rm=TRUE)
  )
  
  keep = c("year","sample.weights","household","cluster","wealth","inade.materials","crowded","inade.sani","hed","ubn","tv","phone","car","fridge")
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
  
  cut.lm <- lm(tl.cuts~cuts)
  alpha <- cut.lm$coefficients[[1]]
  beta <- cut.lm$coefficients[[2]]
  data$cwi <- alpha+(beta*data$wealth)
  
  nameOrder = c("filename","year","household","cluster","sample.weights","cwi","wealth","ubn","inade.materials","crowded","inade.sani","hed","tv","phone","car","fridge")
  data <- data[nameOrder]
  
  return(
    list(
      data = data
      ,cuts = cuts
    )
  )
}
####Run function####

# set our working directory, change this if using on another machine
wd <- "D:/Documents/Data/MICSauto/"
setwd(wd)

# List out all the directories in our wd, this is where our data is contained
dirs <- list.dirs(wd,full.names=TRUE)

dataList <- list()
dataIndex <- 1

for(i in 2:length(dirs)){
  dir <- dirs[i]
  if(file.exists(paste0(dir,"/cwi.RData"))){
    message(basename(dir))
    if(exists("data")){rm(data)}
    if(exists("cut.df")){rm(cut.df)}
    load(paste0(dir,"/cwi.RData"))
    dataList[[dataIndex]] <- data
    dataIndex <- dataIndex + 1 
  }else{
    if(exists("hh")){rm(hh)}
    if(exists("hl")){rm(hl)}
    load(paste0(dir,"/","hh.RData"))
    load(paste0(dir,"/","hl.RData"))
    names(hh) <- tolower(names(hh))
    names(hl) <- tolower(names(hl))
    if(typeof(hh$hh1)!="NULL"){
      message(basename(dir))
      cwiList <- cwi(hh,hl)
      if(!is.na(cwiList)){
        cuts <- cwiList[["cuts"]]
        data <- cwiList[["data"]]
        labels <- c("Own car","Own fridge","Own telephone","Own TV","1 or more UBN","2 or more UBN","3 or more UBN","4 or more UBN")
        cut.df <- data.frame(labels,cuts)
        save(data,cut.df,file=paste0(dir,"/cwi.RData"))
        dataList[[dataIndex]] <- data
        dataIndex <- dataIndex + 1 
      }
    }
  }
}

wd <- "D:/Documents/Data/MICSmeta"
setwd(wd)
metaData <- rbindlist(dataList,fill=TRUE)
write.csv(metaData,"global_mics_cwi.csv",row.names=FALSE,na="")
mics.cwi <- metaData
save(mics.cwi,file="global_mics_cwi.RData")

###Debug
# cwi.tab <- data.table(mics.cwi)
# cwi.collapse <- cwi.tab[
#   ,.(mean.cwi=weighted.mean(cwi,sample.weights,na.rm=TRUE)
#       ,mean.ubn=weighted.mean(ubn,sample.weights,na.rm=TRUE)
#       ,mean.mat=weighted.mean(inade.materials,sample.weights,na.rm=TRUE)
#       ,mean.crowd=weighted.mean(crowded,sample.weights,na.rm=TRUE)
#       ,mean.sani=weighted.mean(inade.sani,sample.weights,na.rm=TRUE)
#       ,mean.hed=weighted.mean(hed,sample.weights,na.rm=TRUE)
#       ,mean.tv=weighted.mean(tv,sample.weights,na.rm=TRUE)
#       ,mean.phone=weighted.mean(phone,sample.weights,na.rm=TRUE)
#       ,mean.car=weighted.mean(car,sample.weights,na.rm=TRUE)
#       ,mean.fridge=weighted.mean(fridge,sample.weights,na.rm=TRUE)
#      )
#   , by=.(filename)]