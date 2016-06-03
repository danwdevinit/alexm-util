####Function and setup####
library(Hmisc)
library(plyr)
library(foreign)
library(data.table)

setwd("D:/Documents/Data/MICSmeta/")
classes <- read.csv("global_mics_classes.csv",na.strings=c("","NAN"),as.is=TRUE)
varNames <- read.csv("toiletsVars.csv",as.is=TRUE,na.strings="")
assetVars <- read.csv("assetVars.csv",as.is=TRUE,na.strings="NAN")

hrwd <- "D:/Documents/Data/MICSauto/Ukraine_MICS4_Datasets"

tl.cuts <- read.csv("D:/Documents/Data/DHSauto/tlhr61dt/cuts.csv")$cuts

yes <- c("yes",1,"qui","si","sin")
no <- c("no",0,2,"non")
missing <-c(NA,"missing","dk")
preschool <- c("presc","pre-sc")
primary <- c("prim")
secondary <- c("second")
higher <- c("voca","univ","colle","high")
urban <- c("urban")
rural <- c("rural")

cwi <- function(hrwd){
  if(!file_test(op="-d", hrwd)){message("HR WD invalid");return(NA);}
  
  hrBase <- basename(hrwd)
  
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
  
  toilets.classes <- subset(classes,filename==hrBase & type=="toilets")
  water.classes <- subset(classes,filename==hrBase & type=="water")
  floor.classes <- subset(classes,filename==hrBase & type=="floor")
  wall.classes <- subset(classes,filename==hrBase & type=="wall")
  if(nrow(wall.classes)==0){stop("Missing from codebook!")}
  if(nrow(water.classes)==0){stop("Missing from codebook!")}
  if(nrow(floor.classes)==0){stop("Missing from codebook!")}
  if(nrow(toilets.classes)==0){stop("Missing from codebook!")}
  
  hr <- read.csv(paste0(hrwd,"/hh.csv"),
                 as.is=TRUE,
                 na.strings="",check.names=FALSE)
  
  #Exit function if really low MICS phase
  insufficient <- c("Trinidad and Tobago MICS 2006 SPSS Datasets","Madagascar (South)_ MICS4_Datasets")
  if(typeof(hr$hh1)=="NULL" | hrBase %in% insufficient){
    return(NA)
  }
  
  ir <- read.csv(paste0(hrwd,"/hl.csv"),as.is=TRUE,na.strings="",check.names=FALSE)
  
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
  
  #Rename car/truck var
  names(hr)[which(names(hr)==carVar)] <- "car"
  if(typeof(hr$car)=="NULL" | typeof(hr$car)=="logical" | length(hr$car[which(!is.na(hr$car))])==0){message("Car missing!");car.missing<-TRUE}else{car.missing<-FALSE}
  
  #Rename fridge var
  names(hr)[which(names(hr)==fridgeVar)] <- "fridge"
  if(typeof(hr$fridge)=="NULL" | typeof(hr$fridge)=="logical" | length(hr$fridge[which(!is.na(hr$fridge))])==0){message("Fridge missing!");fridge.missing<-TRUE}else{fridge.missing<-FALSE}
  
  #Rename phone var
  names(hr)[which(names(hr)==phoneVar)] <- "phone"
  if(typeof(hr$phone)=="NULL" | typeof(hr$phone)=="logical" | length(hr$phone[which(!is.na(hr$phone))])==0){message("Phone missing!");phone.missing<-TRUE}else{phone.missing<-FALSE}
  
  #Rename tv var
  names(hr)[which(names(hr)==tvVar)] <- "tv"
  if(typeof(hr$tv)=="NULL" | typeof(hr$tv)=="logical" | length(hr$tv[which(!is.na(hr$tv))])==0){message("TV missing!");tv.missing<-TRUE}else{tv.missing<-FALSE}
  
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
  names(hr)[which(names(hr)==toiletVar)] <- "toilets"
  if(typeof(hr$toilets)=="NULL"){message("No toilets!");hr$toilets<-NA}

  #Rename share toilets var
  names(hr)[which(names(hr)==share.toilet.var)] <- "share.toilets"
  if(typeof(hr$share.toilets)=="NULL" | typeof(hr$share.toilets)=="logical" | length(hr$share.toilets[which(!is.na(hr$share.toilets))])==0){share.toilets.missing<-TRUE}else{share.toilets.missing<-FALSE}

  #Rename sleeping rooms var
  names(hr)[which(names(hr)=="hc2")] <- "sleeping.rooms"
  if(typeof(hr$sleeping.rooms)=="NULL"){message("No sleeping.rooms!");hr$sleeping.rooms<-NA}
  
  #Rename members var
  names(hr)[which(names(hr)=="hh11")] <- "members"
  
  #Rename educ var
  nepals <- c("Nepal_MICS5_Datasets","Nepal (Mid-and Far-Western Regions)_MICS4_Datasets")
  if(hrBase %in% nepals){
    names(ir)[which(names(ir)=="ed3")] <- "attended"
    ir$school <- NA
    names(ir)[which(names(ir)=="ed4b")] <- "grade"
  }else if(hrBase=="Mongolia_MICS4_Datasets"){
    names(ir)[which(names(ir)=="ed3")] <- "attended"
    names(ir)[which(names(ir)=="ed4_a")] <- "school"
    names(ir)[which(names(ir)=="ed4_b")] <- "grade"
  }else if(hrBase=="Thailand MICS 2005-2006 SPSS Datasets"){
    names(ir)[which(names(ir)=="ed2")] <- "attended"
    names(ir)[which(names(ir)=="ed3a")] <- "school"
    names(ir)[which(names(ir)=="ed3b")] <- "grade"
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
      grade <- gradeV[i]
      if(is.na(attended)){
        educ <- NA
      }else if(attended %in% no){
        #No education
        educ <- 0
      }else{
        if(school %in% missing){
          educ <- NA
        } else if(sum(sapply(preschool,grepl,x=school),na.rm=TRUE)>0){
          if(is.na(grade)){
            educ <- 0
          }else if(grade>=5 & grade<98){
            #Complete primary
            educ <- 1
          }else{
            educ <- 0
          }
        } else if(sum(sapply(primary,grepl,x=school),na.rm=TRUE)>0){
          if(is.na(grade)){
            educ <- NA
          }else if(grade<5){
            #Incomplete primary
            educ <- 0
          }else if(grade>=5 & grade<98){
            #Complete primary
            educ <- 1
          }else{
            educ <- NA
          }
        } else if(sum(sapply(secondary,grepl,x=school),na.rm=TRUE)>0){
          #(in)complete secondary
          educ <- 1
        } else if(sum(sapply(higher,grepl,x=school),na.rm=TRUE)>0){
          #(in)complete higher
          educ <- 1
        }else if(grade>=5 & grade<98){
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
  calc.hed.hr <- function(df){
    hedV <- c()
    for(i in 1:nrow(df)){
      members <- df$members[i]
      head.educ <- tolower(df$helevel[i])
      workers <- 1
      adults.completed.primary <- 0
      
      if(
        sum(sapply(c(primary,secondary,higher),grepl,x=head.educ),na.rm=TRUE)>0
         ){adults.completed.primary <- 1}
      
      hed = ((members/workers)>3 & adults.completed.primary==0)
      hedV <- c(hedV,hed)
    }
    return(hedV)
  }
  hr[which(is.na(hr$hed)),]$hed <- calc.hed.hr(hr[which(is.na(hr$hed)),])
  
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
    if(is.null(x)){return(NA)}
    else if(is.na(x)){return(NA)}
    else if(tolower(x) %in% missing){return(NA)}
    else if(tolower(x) %in% urban){return(1)}
    else if(tolower(x) %in% rural){return(0)}
    else{return(NA)}
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
        if(urban==1){
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
      if(share.toilets %in% yes){
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
  
  recode.asset <- function(x){
    if(is.null(x)){return(NA)}
    else if(is.na(x) | x==9){return(NA)}
    else if(tolower(x) %in% missing){return(NA)}
    else if(tolower(x) %in% yes){return(1)}
    else if(tolower(x) %in% no){return(0)}
    else{return(NA)}
  }
  
  ###Replication method
  #Calc wealth where half of households own tv
  if(!(tv.missing)){
    hr$tv <- sapply(hr$tv,recode.asset)
    tv.glm <- glm(tv~wealth,data=hr,family="binomial")
    tv.pred.wealth <- (-1*tv.glm$coefficients[[1]])/tv.glm$coefficients[[2]]
  }else{
    tv.pred.wealth <- NA
  }
  
  #Calc wealth where half of households own fridge
  if(!(fridge.missing)){
    hr$fridge <- sapply(hr$fridge,recode.asset)
    fridge.glm <- glm(fridge~wealth,data=hr,family="binomial")
    fridge.pred.wealth <- (-1*fridge.glm$coefficients[[1]])/fridge.glm$coefficients[[2]]
  }else{
    fridge.pred.wealth <- NA
  }
  
  #Calc wealth where half of households own car
  if(!(car.missing)){
    hr$car <- sapply(hr$car,recode.asset)
    car.glm <- glm(car~wealth,data=hr,family="binomial")
    car.pred.wealth <- (-1*car.glm$coefficients[[1]])/car.glm$coefficients[[2]]
  }else{
    car.pred.wealth <- NA
  }
  
  #Calc wealth where half of households own phone
  if(!(phone.missing)){
    hr$phone <- sapply(hr$phone,recode.asset)
    phone.glm <- glm(phone~wealth,data=hr,family="binomial")
    phone.pred.wealth <- (-1*phone.glm$coefficients[[1]])/phone.glm$coefficients[[2]]
  }else{
    phone.pred.wealth <- NA
  }
  
  #Calc wealth where half of households have various UBNs
  pred.wealth.4 <- mean(hr[which(hr$ubn>=4),]$wealth,na.rm=TRUE)
  pred.wealth.3 <- mean(hr[which(hr$ubn>=3),]$wealth,na.rm=TRUE)
  pred.wealth.2 <- mean(hr[which(hr$ubn>=2),]$wealth,na.rm=TRUE)
  pred.wealth.1 <- mean(hr[which(hr$ubn>=1),]$wealth,na.rm=TRUE)
  
  #Generate cutpoint dfs
  cuts <- c(
    car.pred.wealth
    ,fridge.pred.wealth
    ,phone.pred.wealth
    ,tv.pred.wealth
    ,pred.wealth.1
    ,pred.wealth.2
    ,pred.wealth.3
    ,pred.wealth.4
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

# Loop through every dir
for(i in 2:length(dirs)){
  dir <- dirs[i]
  message(basename(dir))
  files <- list.files(dir)
  if("wealth.csv" %in% files){
    dataList[[dataIndex]] <- read.csv(paste0(dir,"/wealth.csv"),na.strings="",as.is=TRUE)
    dataIndex <- dataIndex + 1 
  }else{
    cwiList <- cwi(dir) 
    if(!is.na(cwiList)){
      cuts <- cwiList[["cuts"]]
      data <- cwiList[["data"]]
      labels <- c("Own car","Own fridge","Own telephone","Own TV","1 or more UBN","2 or more UBN","3 or more UBN","4 or more UBN")
      cut.df <- data.frame(labels,cuts)
      write.csv(cut.df,paste(dir,"cuts.csv",sep="/"),row.names=FALSE,na="")
      write.csv(data,paste(dir,"wealth.csv",sep="/"),row.names=FALSE,na="")
      dataList[[dataIndex]] <- data
      dataIndex <- dataIndex + 1 
    }
  }
}

wd <- "D:/Documents/Data/MICSmeta"
setwd(wd)
metaData <- rbindlist(dataList,fill=TRUE)
write.csv(metaData,"global_cwi.csv",row.names=FALSE,na="")