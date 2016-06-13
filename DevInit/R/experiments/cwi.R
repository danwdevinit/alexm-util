library(Hmisc)
library(plyr)
library(foreign)

ben.hrwd <- "D:/Documents/Data/DHSauto/bjhr51dt/"
setwd(ben.hrwd)

ben.hr <- read.dta("BJHR51FL.dta")

ben.irwd <- "D:/Documents/Data/DHSauto/bjir51dt/"
setwd(ben.irwd)

ben.ir <- read.dta("BJIR51FL.dta")

viet.hrwd <- "D:/Documents/Data/DHSauto/vnhr41dt"
setwd(viet.hrwd)

viet.hr <- read.dta("VNHR41FL.dta")

viet.irwd <- "D:/Documents/Data/DHSauto/vnir41dt"
setwd(viet.irwd)

viet.ir <- read.dta("VNIR41FL.dta")

viet.wiwd <- "D:/Documents/Data/DHSauto/vnwi41dt"
setwd(viet.wiwd)

viet.wi <- read.dta("VNWI41FL.dta")
names(viet.wi) <- c("hhid","wealth","wealth.cat")

viet.hr <- join(
  viet.hr
  ,viet.wi
  ,by=c("hhid")
  )

wd <- "D:/Documents/Data/DHSmeta"
setwd(wd)

#Rename wealth var
names(ben.hr)[which(names(ben.hr)=="hv271")] <- "wealth"
ben.hr$wealth <- ben.hr$wealth/100000

#Rename urban var
names(ben.hr)[which(names(ben.hr)=="hv025")] <- "urban.rural"
names(viet.hr)[which(names(viet.hr)=="hv025")] <- "urban.rural"

#Rename car/truck var
names(ben.hr)[which(names(ben.hr)=="hv212")] <- "car"
names(viet.hr)[which(names(viet.hr)=="hv212")] <- "car"

#Rename fridge var
names(ben.hr)[which(names(ben.hr)=="hv209")] <- "fridge"
names(viet.hr)[which(names(viet.hr)=="hv209")] <- "fridge"

#Rename phone var
names(ben.hr)[which(names(ben.hr)=="hv221")] <- "phone"
names(viet.hr)[which(names(viet.hr)=="hv221")] <- "phone"

#Rename tv var
names(ben.hr)[which(names(ben.hr)=="hv208")] <- "tv"
names(viet.hr)[which(names(viet.hr)=="hv208")] <- "tv"

#Rename wall var
names(ben.hr)[which(names(ben.hr)=="hv214")] <- "wall"
names(viet.hr)[which(names(viet.hr)=="hv214")] <- "wall"

#Rename floor var
names(ben.hr)[which(names(ben.hr)=="hv213")] <- "floor"
names(viet.hr)[which(names(viet.hr)=="hv213")] <- "floor"

#Rename sleeping rooms var
names(ben.hr)[which(names(ben.hr)=="hv216")] <- "sleeping.rooms"
names(viet.hr)[which(names(viet.hr)=="hv216")] <- "sleeping.rooms"

#Rename members var
names(ben.hr)[which(names(ben.hr)=="hv013")] <- "members"
names(viet.hr)[which(names(viet.hr)=="hv013")] <- "members"

#Rename drinking water var
names(ben.hr)[which(names(ben.hr)=="hv201")] <- "water"
names(viet.hr)[which(names(viet.hr)=="hv201")] <- "water"

#Rename toilets var
names(ben.hr)[which(names(ben.hr)=="hv205")] <- "toilets"
names(viet.hr)[which(names(viet.hr)=="hv205")] <- "toilets"

#Rename share toilets var
names(ben.hr)[which(names(ben.hr)=="hv225")] <- "share.toilets"
names(viet.hr)[which(names(viet.hr)=="hv225")] <- "share.toilets"

#Rename truancy var
names(ben.hr)[which(names(ben.hr)=="hv213")] <- "floor"
names(viet.hr)[which(names(viet.hr)=="hv213")] <- "floor"

#Rename partners occupation var
names(ben.ir)[which(names(ben.ir)=="v704")] <- "partner.occ"
names(viet.ir)[which(names(viet.ir)=="v704")] <- "partner.occ"

#Rename partners education var
names(ben.ir)[which(names(ben.ir)=="v729")] <- "partner.educ"
names(viet.ir)[which(names(viet.ir)=="v729")] <- "partner.educ"

#Rename occupation var
names(ben.ir)[which(names(ben.ir)=="v714")] <- "working"
names(viet.ir)[which(names(viet.ir)=="v714")] <- "working"

#Rename educ var
names(ben.ir)[which(names(ben.ir)=="v149")] <- "educ"
names(viet.ir)[which(names(viet.ir)=="v149")] <- "educ"

#Rename age var
names(ben.ir)[which(names(ben.ir)=="v012")] <- "age"
names(viet.ir)[which(names(viet.ir)=="v012")] <- "age"

#Rename educ var
#Different vars for viet and ben
names(ben.ir)[which(names(ben.ir)=="s109b")] <- "in.school"
names(viet.ir)[which(names(viet.ir)=="v148")] <- "in.school"

#Rename head var
names(ben.ir)[which(names(ben.ir)=="v150")] <- "head"
names(viet.ir)[which(names(viet.ir)=="v150")] <- "head"


#Rename cluster/hh var
names(ben.ir)[which(names(ben.ir)=="v001")] <- "cluster"
names(ben.ir)[which(names(ben.ir)=="v002")] <- "household"
names(viet.ir)[which(names(viet.ir)=="v001")] <- "cluster"
names(viet.ir)[which(names(viet.ir)=="v002")] <- "household"
names(ben.hr)[which(names(ben.hr)=="hv001")] <- "cluster"
names(ben.hr)[which(names(ben.hr)=="hv002")] <- "household"
names(viet.hr)[which(names(viet.hr)=="hv001")] <- "cluster"
names(viet.hr)[which(names(viet.hr)=="hv002")] <- "household"

#Recode IR level to hr
recode.partner.working <- function(x){
  if(is.na(x)){return(NA)}
  else if(is.null(x)){return(NULL)}
  else if(x==0){return(0)}
  else if(x==999){return(NA)}
  else{return(1)}
}
ben.ir$partner.working <- sapply(ben.ir$partner.occ,recode.partner.working)
viet.ir$partner.working <- sapply(viet.ir$partner.occ,recode.partner.working)
recode.working <- function(x){
  if(is.na(x)){return(NA)}
  else if(is.null(x)){return(NULL)}
  else if(x==0){return(0)}
  else if(x==9){return(NA)}
  else{return(1)}
}
ben.ir$working <- sapply(ben.ir$working,recode.working)
viet.ir$working <- sapply(viet.ir$working,recode.working)
recode.educ <- function(x){
  if(is.na(x)){return(NA)}
  else if(is.null(x)){return(NULL)}
  else if(is.factor(x)){
    if(tolower(x)=="no education" | tolower(x)=="incomplete primary"){return(0)}
    else{return(1)}
  }else{
    if(x<=1){return(0)}
    else if(x>=8){return(NA)}
    else if(x>=2){return(1)} 
  }
}
ben.ir$partner.educ <- sapply(ben.ir$partner.educ,recode.educ)
ben.ir$educ <- sapply(ben.ir$educ,recode.educ)
viet.ir$partner.educ <- sapply(viet.ir$partner.educ,recode.educ)
viet.ir$educ <- sapply(viet.ir$educ,recode.educ)
recode.in.school <- function(x){
  if(is.na(x)){return(NA)}
  else if(is.null(x)){return(NULL)}
  else if(x==0 | tolower(x)=="no"){return(0)}
  else if(x==1 | tolower(x)=="yes"){return(1)}
  else{return(NA)}
}
ben.ir$in.school <- sapply(ben.ir$in.school,recode.in.school)
viet.ir$in.school <- sapply(viet.ir$in.school,recode.in.school)
recode.head <- function(x){
  if(is.na(x)){return(NA)}
  else if(is.null(x)){return(NULL)}
  else if(x==1 | tolower(x)=="head"){return(1)}
  else{return(0)}
}
ben.ir$head <- sapply(ben.ir$head,recode.head)
viet.ir$head <- sapply(viet.ir$head,recode.head)
ben.ir.head <- subset(ben.ir,head==1)
viet.ir.head <- subset(viet.ir,head==1)

ben.members <- ben.hr[c("cluster","household","members")]

ben.ir <- join(
  ben.ir
  ,ben.members
  ,by=c("cluster","household")
  )

viet.members <- viet.hr[c("cluster","household","members")]

viet.ir <- join(
  viet.ir
  ,viet.members
  ,by=c("cluster","household")
)

calc.hed <- function(membersV,workersV,adults.completed.primaryV,young.in.schoolV,head.educV){
  hedV <- c()
  for(i in 1:length(membersV)){
    members <- membersV[i]
    workers <- workersV[i]
    adults.completed.primary <- adults.completed.primaryV[i]
    young.in.school <- young.in.schoolV[i]
    head.educ <- head.educV[i]
  
    if(workers<1){
      workers=1
    }
    if(members-young.in.school<=0){
      hed = ((members/workers)>3 & head.educ==0)
    }else{
      hed = ((members/workers)>3 & adults.completed.primary==0)
    }
    hedV <- c(hedV,hed)
  }
  return(hedV)
}

ben.hed <- ddply(ben.ir,.(cluster,household),summarise
                 ,members=mean(members,na.rm=TRUE)
                 ,workers=sum(working==1,na.rm=TRUE)+sum(partner.working==1,na.rm=TRUE)
                 ,adults.completed.primary=sum((educ==1 & age>24),na.rm=TRUE)+sum((educ==1 & age<=24 & in.school==0),na.rm=TRUE)
                 ,young.in.school=sum((age<=24 & in.school==1),na.rm=TRUE)
                 )

ben.head.educ <- ben.ir.head[c("cluster","household","educ")]
names(ben.head.educ) <- c("cluster","household","head.educ")

ben.hed <- join(
  ben.hed
  ,ben.head.educ
  ,by=c("cluster","household")
)

ben.hed <- transform(ben.hed
                     ,hed = calc.hed(members,workers,adults.completed.primary,young.in.school,head.educ)
                     )

viet.hed <- ddply(viet.ir,.(cluster,household),summarise
                 ,members=mean(members,na.rm=TRUE)
                 ,workers=sum(working==1,na.rm=TRUE)+sum(partner.working==1,na.rm=TRUE)
                 ,adults.completed.primary=sum((educ==1 & age>24),na.rm=TRUE)+sum((educ==1 & age<=24 & in.school==0),na.rm=TRUE)
                 ,young.in.school=sum((age<=24 & in.school==1),na.rm=TRUE)
)

viet.head.educ <- viet.ir.head[c("cluster","household","educ")]
names(viet.head.educ) <- c("cluster","household","head.educ")

viet.hed <- join(
  viet.hed
  ,viet.head.educ
  ,by=c("cluster","household")
)

viet.hed <- transform(viet.hed
                     ,hed = calc.hed(members,workers,adults.completed.primary,young.in.school,head.educ)
)

#Recode HR level vars
keep <- c("cluster","household","hed")
ben.hed <- ben.hed[keep]
viet.hed <- viet.hed[keep]
ben.hr <- join(
  ben.hr
  ,ben.hed
  ,by=c("cluster","household")
)
viet.hr <- join(
  viet.hr
  ,viet.hed
  ,by=c("cluster","household")
)

###One worker assumption for households outside of IR, ben widemax = 18, viet = 36
calc.hed.hr <- function(df){
  hedV <- c()
  for(i in 1:nrow(df)){
    members <- df$members[i]
    workers <- 1
    adults.completed.primary <- 0
    head.educ <- FALSE
    
    for(j in 1:99){
      ageVar <- paste0("hv105_",j)
      educVar <- paste0("hv106_",j)
      if(typeof(df[[ageVar]])!="NULL"){
        age <- df[[ageVar]][i]
        educ <- df[[educVar]][i]
        if(!is.na(age)){
          if(age>=15 & age<=49){
            if(!is.na(educ)){
              if(recode.educ(educ)==1){
                adults.completed.primary <- adults.completed.primary + 1
              }
            }
          }
        }
      }else{
        break;
      }
    }
    
    hed = ((members/workers)>3 & adults.completed.primary==0)
    hedV <- c(hedV,hed)
  }
  return(hedV)
}
ben.hr[which(is.na(ben.hr$hed)),]$hed <- calc.hed.hr(ben.hr[which(is.na(ben.hr$hed)),])
viet.hr[which(is.na(viet.hr$hed)),]$hed <- calc.hed.hr(viet.hr[which(is.na(viet.hr$hed)),])


recode.wall <- function(x){
  if(is.null(x)){return(NA)}
  else if(is.na(x)){return(NA)}
  else if(x==96 | x==99){return(NA)}
  else if(x<=29){return(1)}
  else{return(0)}
}
ben.hr$inade.wall <- sapply(ben.hr$wall,recode.wall)
viet.hr$inade.wall <- sapply(viet.hr$wall,recode.wall)

recode.floor <- function(x){
  if(is.null(x)){return(NA)}
  else if(is.na(x)){return(NA)}
  else if(x==96 | x==99){return(NA)}
  else if(x<=29){return(1)}
  else{return(0)}
}
nat.rud.floors <- c(
  "natural"
  ,"earth, sand"
  ,"dung"
  ,"rudimentary"
  ,"wood, planks"
  ,"palm, bamboo"
  ,"rough wood/bamboo"
  )
recode.floor.factor <- function(x){
  if(is.null(x)){return(NA)}
  else if(is.na(x)){return(NA)}
  else if(tolower(x)=="other" | tolower(x)=="missing value"){return(NA)}
  else if(tolower(x) %in% nat.rud.floors){return(1)}
  else{return(0)}
}
ben.hr$inade.floor <- sapply(ben.hr$floor,recode.floor)
viet.hr$inade.floor <- sapply(viet.hr$floor,recode.floor.factor)

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

ben.hr$inade.materials <- code.materials(ben.hr$inade.wall,ben.hr$inade.floor)
viet.hr$inade.materials <- code.materials(viet.hr$inade.wall,viet.hr$inade.floor)

ben.hr <- transform(ben.hr
                    ,crowded = ((members/sleeping.rooms)>3)
                    )
viet.hr <- transform(viet.hr,crowded = ((members/sleeping.rooms)>3) )

recode.urban.rural <- function(x){
  if(is.null(x)){return(NA)}
  else if(is.na(x)){return(NA)}
  else if(tolower(x)=="urban" | x==1){return(1)}
  else if(tolower(x)=="rural" | x==2){return(0)}
  else{return(NA)}
}
ben.hr$urban <- sapply(ben.hr$urban.rural,recode.urban.rural)
viet.hr$urban <- sapply(viet.hr$urban.rural,recode.urban.rural)


ben.code.inade.water <- function(urbanV,waterV){
  inade.water <- c()
  for(i in 1:length(urbanV)){
    urban <- urbanV[i]
    water <- waterV[i]
    if(urban==1){
      if(water==96 | water==99 | is.na(water)){
        inade.water <- c(inade.water,NA)
      }else if(water==11 | water==12 | water==71 ){
        inade.water <- c(inade.water,0)
      }else{
        inade.water <- c(inade.water,1)
      }
    }else if(urban==0){
      if(water==96 | water==99 | is.na(water)){
        inade.water <- c(inade.water,NA)
      }else if(water<=31 | water==41 | water==51 | water==52 | water==71 ){
        inade.water <- c(inade.water,0)
      }else{
        inade.water <- c(inade.water,1)
      }
    }else{
      inade.water <- c(inade.water,NA)
    }
  }
  return(inade.water)
}

viet.protected = c(
  "piped into residence"
  ,"public tap"
  ,"well in residence"
  ,"public well"
  ,"rainwater"
  )
viet.code.inade.water <- function(urbanV,waterV){
  inade.water <- c()
  for(i in 1:length(urbanV)){
    urban <- urbanV[i]
    water <- tolower(waterV[i])
    if(urban==1){
      if(water=="other" | is.na(water)){
        inade.water <- c(inade.water,NA)
      }else if(water=="piped into residence"){
        inade.water <- c(inade.water,0)
      }else{
        inade.water <- c(inade.water,1)
      }
    }else if(urban==0){
      if(water=="other" | is.na(water)){
        inade.water <- c(inade.water,NA)
      }else if(water %in% viet.protected){
        inade.water <- c(inade.water,0)
      }else{
        inade.water <- c(inade.water,1)
      }
    }else{
      inade.water <- c(inade.water,NA)
    }
  }
  return(inade.water)
}

ben.hr$inade.water <- ben.code.inade.water(ben.hr$urban,ben.hr$water)
viet.hr$inade.water <- viet.code.inade.water(viet.hr$urban,viet.hr$water)

viet.recode.toilets <- function(x){
  if(is.null(x)){return(NA)}
  else if(is.na(x) | tolower(x)=="other"){return(NA)}
  else if(tolower(x)=="own flush toilet" | x=="vent.imp.pit latrine"){return(0)}
  else{return(1)}
}
viet.hr$inade.toilets <- sapply(viet.hr$toilets,viet.recode.toilets)

ben.code.toilets <- function(toiletsV,share.toiletsV){
  inade.toilets <- c()
  for(i in 1:length(toiletsV)){
    toilets <- toiletsV[i]
    share.toilets <- share.toiletsV[i]
    if(is.na(share.toilets)){
      share.toilets = 0
    }
    if(share.toilets==1){
      inade.toilet = 1
    }else if(is.na(toilets) | toilets>=96){
      inade.toilet = NA
    }else if(toilets==11 | toilets==22){
      inade.toilet = 0
    }else{
      inade.toilet = 1
    }
    inade.toilets <- c(inade.toilets,inade.toilet)
  }
  return(inade.toilets)
}
ben.hr$inade.toilets <- ben.code.toilets(ben.hr$toilets,ben.hr$share.toilets)

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

ben.hr$inade.sani <- combine.sani(ben.hr$inade.toilet,ben.hr$inade.water)
viet.hr$inade.sani <- combine.sani(viet.hr$inade.toilet,viet.hr$inade.water)

#Finally get to calc the UBN!
ben.hr <- transform(ben.hr,ubn = inade.materials+crowded+inade.sani+hed)
viet.hr <- transform(viet.hr,ubn = inade.materials+crowded+inade.sani+hed)

#Calc wealth where half of households own tv
recode.asset <- function(x){
  if(is.null(x)){return(NA)}
  else if(is.na(x) | x==9){return(NA)}
  else if(x==1 | tolower(x)=="yes"){return(1)}
  else if(x==0 | tolower(x)=="no"){return(0)}
  else{return(NA)}
}
ben.hr$tv <- sapply(ben.hr$tv,recode.asset)
viet.hr$tv <- sapply(viet.hr$tv,recode.asset)
ben.tv.glm <- glm(tv~wealth,data=ben.hr,family="binomial")
ben.tv.pred.wealth <- (-1*ben.tv.glm$coefficients[[1]])/ben.tv.glm$coefficients[[2]]
viet.tv.glm <- glm(tv~wealth,data=viet.hr,family="binomial")
viet.tv.pred.wealth <- (-1*viet.tv.glm$coefficients[[1]])/viet.tv.glm$coefficients[[2]]

#Calc wealth where half of households own fridge
ben.hr$fridge <- sapply(ben.hr$fridge,recode.asset)
viet.hr$fridge <- sapply(viet.hr$fridge,recode.asset)
ben.fridge.glm <- glm(fridge~wealth,data=ben.hr,family="binomial")
ben.fridge.pred.wealth <- (-1*ben.fridge.glm$coefficients[[1]])/ben.fridge.glm$coefficients[[2]]
viet.fridge.glm <- glm(fridge~wealth,data=viet.hr,family="binomial")
viet.fridge.pred.wealth <- (-1*viet.fridge.glm$coefficients[[1]])/viet.fridge.glm$coefficients[[2]]

#Calc wealth where half of households own car
ben.hr$car <- sapply(ben.hr$car,recode.asset)
viet.hr$car <- sapply(viet.hr$car,recode.asset)
ben.car.glm <- glm(car~wealth,data=ben.hr,family="binomial")
ben.car.pred.wealth <- (-1*ben.car.glm$coefficients[[1]])/ben.car.glm$coefficients[[2]]
viet.car.glm <- glm(car~wealth,data=viet.hr,family="binomial")
viet.car.pred.wealth <- (-1*viet.car.glm$coefficients[[1]])/viet.car.glm$coefficients[[2]]

#Calc wealth where half of households own phone
ben.hr$phone <- sapply(ben.hr$phone,recode.asset)
viet.hr$phone <- sapply(viet.hr$phone,recode.asset)
ben.phone.glm <- glm(phone~wealth,data=ben.hr,family="binomial")
ben.phone.pred.wealth <- (-1*ben.phone.glm$coefficients[[1]])/ben.phone.glm$coefficients[[2]]
viet.phone.glm <- glm(phone~wealth,data=viet.hr,family="binomial")
viet.phone.pred.wealth <- (-1*viet.phone.glm$coefficients[[1]])/viet.phone.glm$coefficients[[2]]

#Calc wealth where half of households have various UBNs
# ben.hr$ubn4 <- ben.hr$ubn>=4
# ben.hr$ubn3 <- ben.hr$ubn>=3
# ben.hr$ubn2 <- ben.hr$ubn>=2
# ben.hr$ubn1 <- ben.hr$ubn>=1
# ben.4.glm <- glm(ubn4~wealth,data=ben.hr,family="binomial")
# ben.4.pred.wealth <- (-1*ben.4.glm$coefficients[[1]])/ben.4.glm$coefficients[[2]]
# ben.3.glm <- glm(ubn3~wealth,data=ben.hr,family="binomial")
# ben.3.pred.wealth <- (-1*ben.3.glm$coefficients[[1]])/ben.3.glm$coefficients[[2]]
# ben.2.glm <- glm(ubn2~wealth,data=ben.hr,family="binomial")
# ben.2.pred.wealth <- (-1*ben.2.glm$coefficients[[1]])/ben.2.glm$coefficients[[2]]
# ben.1.glm <- glm(ubn1~wealth,data=ben.hr,family="binomial")
# ben.1.pred.wealth <- (-1*ben.1.glm$coefficients[[1]])/ben.1.glm$coefficients[[2]]
# 
# viet.hr$ubn4 <- viet.hr$ubn>=4
# viet.hr$ubn3 <- viet.hr$ubn>=3
# viet.hr$ubn2 <- viet.hr$ubn>=2
# viet.hr$ubn1 <- viet.hr$ubn>=1
# viet.4.glm <- glm(ubn4~wealth,data=viet.hr,family="binomial")
# viet.4.pred.wealth <- (-1*viet.4.glm$coefficients[[1]])/viet.4.glm$coefficients[[2]]
# viet.3.glm <- glm(ubn3~wealth,data=viet.hr,family="binomial")
# viet.3.pred.wealth <- (-1*viet.3.glm$coefficients[[1]])/viet.3.glm$coefficients[[2]]
# viet.2.glm <- glm(ubn2~wealth,data=viet.hr,family="binomial")
# viet.2.pred.wealth <- (-1*viet.2.glm$coefficients[[1]])/viet.2.glm$coefficients[[2]]
# viet.1.glm <- glm(ubn1~wealth,data=viet.hr,family="binomial")
# viet.1.pred.wealth <- (-1*viet.1.glm$coefficients[[1]])/viet.1.glm$coefficients[[2]]

ben.4.pred.wealth <- mean(ben.hr[which(ben.hr$ubn>=4),]$wealth,na.rm=TRUE)
ben.3.pred.wealth <- mean(ben.hr[which(ben.hr$ubn>=3),]$wealth,na.rm=TRUE)
ben.2.pred.wealth <- mean(ben.hr[which(ben.hr$ubn>=2),]$wealth,na.rm=TRUE)
ben.1.pred.wealth <- mean(ben.hr[which(ben.hr$ubn>=1),]$wealth,na.rm=TRUE)

viet.4.pred.wealth <- mean(viet.hr[which(viet.hr$ubn>=4),]$wealth,na.rm=TRUE)
viet.3.pred.wealth <- mean(viet.hr[which(viet.hr$ubn>=3),]$wealth,na.rm=TRUE)
viet.2.pred.wealth <- mean(viet.hr[which(viet.hr$ubn>=2),]$wealth,na.rm=TRUE)
viet.1.pred.wealth <- mean(viet.hr[which(viet.hr$ubn>=1),]$wealth,na.rm=TRUE)

#Generate cutpoint dfs
ben.cuts <- c(
  ben.car.pred.wealth
  ,ben.fridge.pred.wealth
  ,ben.phone.pred.wealth
  ,ben.tv.pred.wealth
  ,ben.1.pred.wealth
  ,ben.2.pred.wealth
  ,ben.3.pred.wealth
  ,ben.4.pred.wealth
  )
viet.cuts <- c(
  viet.car.pred.wealth
  ,viet.fridge.pred.wealth
  ,viet.phone.pred.wealth
  ,viet.tv.pred.wealth
  ,viet.1.pred.wealth
  ,viet.2.pred.wealth
  ,viet.3.pred.wealth
  ,viet.4.pred.wealth
)

viet.percents <- prop.table(table(viet.hr$ubn))
viet.cum.percents <- c(
  sum(viet.percents[4:1],na.rm=TRUE)
  ,sum(viet.percents[4:2],na.rm=TRUE)
  ,sum(viet.percents[4:3],na.rm=TRUE)
  ,sum(viet.percents[4],na.rm=TRUE)
)

viet.cuts[5:8] <- quantile(viet.hr$wealth,viet.cum.percents)

ben.percents <- prop.table(table(ben.hr$ubn))
ben.cum.percents <- c(
  sum(ben.percents[4:1],na.rm=TRUE)
  ,sum(ben.percents[4:2],na.rm=TRUE)
  ,sum(ben.percents[4:3],na.rm=TRUE)
  ,sum(ben.percents[4],na.rm=TRUE)
)

ben.cuts[5:8] <- quantile(ben.hr$wealth,ben.cum.percents)

cut.df <- data.frame(ben.cuts
                     ,viet.cuts
                     ,row.names=c(
                       "Car/Truck"
                       ,"Refrigerator"
                       ,"Fixed telephone"
                       ,"TV"
                       ,"UBN score 1 or higher"
                       ,"UBN score 2 or higher"
                       ,"UBN score 3 or higher"
                       ,"UBN score 4"
                       )
                     )
cut.lm <- lm(viet.cuts~ben.cuts)
alpha <- cut.lm$coefficients[[1]]
beta <- cut.lm$coefficients[[2]]
ben.hr$cwi <- alpha+(beta*ben.hr$wealth)
message("a: ",alpha,", b: ",beta)
