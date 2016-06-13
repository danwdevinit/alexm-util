setwd("D:/Documents/Data/DHSmeta/")
tl.cuts <- read.csv("D:/Documents/Data/DHSauto/tlhr61dt/cuts.csv")$cuts

wd <- "D:/Documents/Data/ChinaSurvey/"
setwd(wd)

load("dat2012.RData")
load("wealth.RData")

hr <- dat
ir <- adult

#Rename sample.weights var
names(hr)[which(names(hr)=="fswt_rescs12")] <- "sample.weights"

#Rename urban var
names(hr)[which(names(hr)=="urban12")] <- "urban.rural"

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

if(sum(car.missing,fridge.missing,phone.missing,tv.missing)>1){
  return(NA)
}

#Rename wall var
hr$wall<-NA

#Rename floor var
hr$floor<-NA

#Rename members var
names(hr)[which(names(hr)=="familysize")] <- "members"

#Rename drinking water var
names(hr)[which(names(hr)=="fb1")] <- "water"
if(typeof(hr$water)=="NULL"){message("Missing water!");hr$water<-NA}

#Rename toilets var
names(hr)[which(names(hr)=="fb7")] <- "toilets"
if(typeof(hr$toilets)=="NULL"){message("Missing toilets!");hr$toilets<-NA}

#Rename occupation var
names(ir)[which(names(ir)=="qg101")] <- "working"
if(typeof(ir$working)=="NULL"){working.missing<-TRUE}else{working.missing<-FALSE}

#Rename educ var
names(ir)[which(names(ir)=="kw1")] <- "educ"

#Rename age var
names(ir)[which(names(ir)=="cfps2012_age")] <- "age"

#Rename cluster/hh var
names(ir)[which(names(ir)=="provcd")] <- "province"
names(ir)[which(names(ir)=="countyid")] <- "county"
names(ir)[which(names(ir)=="cid")] <- "community"
names(ir)[which(names(ir)=="fid12")] <- "family"
names(hr)[which(names(hr)=="provcd")] <- "province"
names(hr)[which(names(hr)=="countyid")] <- "county"
names(hr)[which(names(hr)=="cid")] <- "community"
names(hr)[which(names(hr)=="fid12")] <- "family"


#Recode IR level to hr
recode.working <- function(x){
  if(is.na(x)){return(NA)}
  else if(is.null(x)){return(NULL)}
  else if(tolower(x)=="na" | tolower(x)=="unknown" ){return(NA)}
  else if(tolower(x)=="no"){return(0)}
  else if(tolower(x)=="yes"){return(1)}
  else{return(NA)}
}
ir$working <- sapply(ir$working,recode.working)
recode.educ <- function(x){
  if(is.na(x)){return(NA)}
  else if(is.null(x)){return(NA)}
  else if(tolower(x)=="na"){return(NA)}
  else if(tolower(x)=="never go to school" | tolower(x)=="less than primary school"){return(0)}
  else{return(1)}
}
ir$educ <- sapply(ir$educ,recode.educ)

members <- hr[c("province","county","community","family","members")]

ir <- join(
  ir
  ,members
  ,by=c("province","county","community","family")
)

calc.hed <- function(membersV,workersV,adults.completed.primaryV){
  hedV <- c()
  for(i in 1:length(membersV)){
    members <- membersV[i]
    workers <- workersV[i]
    adults.completed.primary <- adults.completed.primaryV[i]
    
    if(workers<1){
      workers=1
    }
    hed = ((members/workers)>3 & adults.completed.primary==0)
    hedV <- c(hedV,hed)
  }
  return(hedV)
}

ir.table <- data.table(ir)
hed.member <- ir.table[,.(members=mean(members,na.rm=TRUE)), by=.(province,county,community,family)]
hed.worker <- ir.table[,.(workers=sum(working,na.rm=TRUE)), by=.(province,county,community,family)]
hed.primary <- ir.table[,.(adults.completed.primary=sum(educ==1,na.rm=TRUE)), by=.(province,county,community,family)]


hed <- join_all(list(hed.member,hed.worker,hed.primary),by=c("province","county","community","family"),type="full")
missing.workers <- hed[which(hed$workers<=0),]$workers
if(length(missing.workers)>0){
  hed[which(hed$workers<=0),]$workers <- 1
}

hed <- transform(hed
                 ,hed = calc.hed(members,workers,adults.completed.primary)
)

#Recode HR level vars
keep <- c("province","county","community","family","hed")
hed <- hed[keep]
hr <- join(
  hr
  ,hed
  ,by=c("province","county","community","family")
)

hr$inade.materials <- NA

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

recode.urban.rural <- function(x){
  if(is.null(x)){return(NA)}
  else if(is.na(x)){return(NA)}
  else if(tolower(x)=="urban" | x==1){return(1)}
  else if(tolower(x)=="rural" | x==2){return(0)}
  else{return(NA)}
}
hr$urban <- sapply(hr$urban.rural,recode.urban.rural)

urban.inade.waters <- c(
  "River/Lake water"
  ,"Well/Spring water"
  ,"Rainwater"
  ,"Cellar water"
  ,"Pond water"
  )
rural.inade.waters <- c(
  "River/Lake water"
  ,"Rainwater"
  ,"Cellar water"
  ,"Pond water"
)
missing.water <- c("Other [Please specify]",NA)

code.inade.water <- function(urbanV,waterV){
  inade.water <- c()
  for(i in 1:length(urbanV)){
    urban <- urbanV[i]
    water <- waterV[i]
    if(water %in% missing.water){
      inade.water <- c(inade.water,NA)
    }else{
      if(urban==1){
        inade.water <- c(inade.water,water %in% urban.inade.waters)
      }else if(urban==0){
        inade.water <- c(inade.water,water %in% rural.inade.waters)
      }else{
        inade.water <- c(inade.water,NA)
      } 
    }
  }
  return(inade.water)
}

hr$inade.water <- code.inade.water(hr$urban,hr$water)

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

keep = c("sample.weights","province","county","community","family","wealth","inade.materials","crowded","inade.sani","hed","ubn","tv","phone","car","fridge")
hrNames <- names(hr)
namesDiff <- setdiff(keep,hrNames)
if(length(namesDiff)>0){
  for(y in 1:length(namesDiff)){
    hr[namesDiff[y]] <- NA
    message(paste("Missing variable",namesDiff[y]))
  } 
}
data <- hr[keep]

cut.df <- data.frame(cuts,tl.cuts)
cut.lm <- lm(tl.cuts~cuts)
alpha <- cut.lm$coefficients[[1]]
beta <- cut.lm$coefficients[[2]]
data$cwi <- alpha+(beta*data$wealth)

data$iso2 <- "CN"
data$year <- 2012
data$filename <- "China"

nameOrder = c("filename","iso2","year","province","county","community","family","sample.weights","cwi","wealth","ubn","inade.materials","crowded","inade.sani","hed","tv","phone","car","fridge")
data <- data[nameOrder]

save(data,cut.df,file="cwi.RData")
china.cwi <- data
save(china.cwi,file="china.cwi.RData")

# data$quint.20 <- (data$cwi <= -0.06008803)
data$quint.20 <- (data$cwi <= -1)
data$weights <- data$sample.weights/1000000
df <- data[c("quint.20","weights")]
df <- df[complete.cases(df),]
weighted.mean(df$quint.20,df$weights,na.rm=TRUE)
