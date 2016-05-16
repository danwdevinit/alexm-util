library(Hmisc)
library(plyr)
library(foreign)
library(data.table)

#use this as the input for a function later
hrwd <- "D:/Documents/Data/DHSauto/bjhr61dt/"
cwi <- function(hrwd){
  hrBase <- basename(hrwd)
  iso2 <- toupper(substr(hrBase,1,2))
  phase <- substr(hrBase,5,6)
  
  setwd(hrwd)
  
  hr <- read.dta(paste0(iso2,"HR",phase,"FL.dta"))
  
  irwd <- paste0("D:/Documents/Data/DHSauto/",tolower(iso2),"ir",phase,"dt/")
  setwd(irwd)
  
  ir <- read.dta(paste0(iso2,"IR",phase,"FL.dta"))
  
  wd <- "D:/Documents/Data/DHSmeta"
  setwd(wd)
  
  #Rename wealth var
  names(hr)[which(names(hr)=="hv271")] <- "wealth"
  hr$wealth <- hr$wealth/100000
  
  #Rename urban var
  names(hr)[which(names(hr)=="hv025")] <- "urban.rural"
  
  #Rename car/truck var
  names(hr)[which(names(hr)=="hv212")] <- "car"
  
  #Rename fridge var
  names(hr)[which(names(hr)=="hv209")] <- "fridge"
  
  #Rename phone var
  names(hr)[which(names(hr)=="hv221")] <- "phone"
  
  #Rename tv var
  names(hr)[which(names(hr)=="hv208")] <- "tv"
  
  #Rename wall var
  names(hr)[which(names(hr)=="hv214")] <- "wall"
  
  #Rename floor var
  names(hr)[which(names(hr)=="hv213")] <- "floor"
  
  #Rename sleeping rooms var
  names(hr)[which(names(hr)=="hv216")] <- "sleeping.rooms"
  
  #Rename members var
  names(hr)[which(names(hr)=="hv009")] <- "members"
  
  #Rename drinking water var
  names(hr)[which(names(hr)=="hv201")] <- "water"
  
  #Rename toilets var
  names(hr)[which(names(hr)=="hv205")] <- "toilets"
  
  #Rename share toilets var
  names(hr)[which(names(hr)=="hv225")] <- "share.toilets"
  
  #Rename partners occupation var
  names(ir)[which(names(ir)=="v705")] <- "partner.occ"
  
  #Rename partners education var
  names(ir)[which(names(ir)=="v729")] <- "partner.educ"
  
  #Rename occupation var
  names(ir)[which(names(ir)=="v714")] <- "working"
  
  #Rename educ var
  names(ir)[which(names(ir)=="v149")] <- "educ"
  
  #Rename age var
  names(ir)[which(names(ir)=="v012")] <- "age"
  
  #Rename head var
  names(ir)[which(names(ir)=="v150")] <- "head"
  
  
  #Rename cluster/hh var
  names(ir)[which(names(ir)=="v001")] <- "cluster"
  names(ir)[which(names(ir)=="v002")] <- "household"
  names(hr)[which(names(hr)=="hv001")] <- "cluster"
  names(hr)[which(names(hr)=="hv002")] <- "household"
  
  
  #Recode IR level to hr
  recode.partner.working <- function(x){
    if(is.na(x)){return(NA)}
    else if(is.null(x)){return(NULL)}
    else if(x==98 | x==99 | tolower(x)=="don't know" | tolower(x)=="missing"){return(NA)}
    else if(x==0 | tolower(x)=="did not work"){return(0)}
    else{return(1)}
  }
  ir$partner.working <- sapply(ir$partner.occ,recode.partner.working)
  recode.working <- function(x){
    if(is.na(x)){return(NA)}
    else if(is.null(x)){return(NULL)}
    else if(x==9 | tolower(x)=="missing"){return(NA)}
    else if(x==0 | tolower(x)=="no"){return(0)}
    else if(x==1 | tolower(x)=="yes"){return(1)}
    else{return(NA)}
  }
  ir$working <- sapply(ir$working,recode.working)
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
  ir$partner.educ <- sapply(ir$partner.educ,recode.educ)
  ir$educ <- sapply(ir$educ,recode.educ)
  
  members <- hr[c("cluster","household","members")]
  
  ir <- join(
    ir
    ,members
    ,by=c("cluster","household")
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
  hed.member <- ir.table[,mean(members,na.rm=TRUE), by=.(cluster,household)]
  setnames(hed.member,"V1","members")
  hed.worker <- ir.table[,sum(working,na.rm=TRUE)+sum(partner.working,na.rm=TRUE), by=.(cluster,household)]
  setnames(hed.worker,"V1","workers")
  hed.primary <- ir.table[,sum(educ==1,na.rm=TRUE), by=.(cluster,household)]
  setnames(hed.primary,"V1","adults.completed.primary")
  hed <- join_all(list(hed.member,hed.worker,hed.primary),by=c("cluster","household"),type="full")
  
  hed <- transform(hed
    ,hed = calc.hed(members,workers,adults.completed.primary)
  )
  
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
      workers <- 1
      adults.completed.primary <- 0
      
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
  hr[which(is.na(hr$hed)),]$hed <- calc.hed.hr(hr[which(is.na(hr$hed)),])
  
  nat.rud.walls <- c(
    10,  "natural"
    ,11,  "no walls"
    ,12,  "cane / palm / trunks"
    ,13,  "dirt"
    ,20,  "rudimentary"
    ,21,  "bamboo with mud"
    ,22,  "stone with mud"
    ,23,  "uncovered adobe"
    ,24,  "plywood"
    ,25,  "cardboard"
    ,26,  "reused wood"
    )
  recode.wall <- function(x){
    if(is.null(x)){return(NA)}
    else if(is.na(x)){return(NA)}
    else if(x==96 | x==99 | tolower(x)=="other" | tolower(x)=="missing"){return(NA)}
    else if(tolower(x) %in% nat.rud.walls){return(1)}
    else{return(0)}
  }
  hr$inade.wall <- sapply(hr$wall,recode.wall)
  
  nat.rud.floors <- c(
    10,  "natural"
    ,11,  "earth, sand"
    ,12,  "dung"
    ,20,  "rudimentary"
    ,21,  "wood planks"
    ,22,  "palm, bamboo"
  )
  recode.floor <- function(x){
    if(is.null(x)){return(NA)}
    else if(is.na(x)){return(NA)}
    else if(x==96 | x==99 | tolower(x)=="other" | tolower(x)=="missing"){return(NA)}
    else if(tolower(x) %in% nat.rud.floors){return(1)}
    else{return(0)}
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
    else if(tolower(x)=="urban" | x==1){return(1)}
    else if(tolower(x)=="rural" | x==2){return(0)}
    else{return(NA)}
  }
  hr$urban <- sapply(hr$urban.rural,recode.urban.rural)
  
  urban.protected.sources = c(
    11,  "piped into dwelling"
    ,12,  "piped to yard/plot"
    ,71,  "bottled water"
  )
  
  rural.protected.sources = c(
    11,  "piped into dwelling"
    ,12,  "piped to yard/plot"
    ,13,  "public tap/standpipe"
    ,20,  "tube well water"
    ,21,  "tube well or borehole"
    ,31,  "protected well"
    ,41,  "protected spring"
    ,51,  "rainwater"
    ,71,  "bottled water"
  )
  code.inade.water <- function(urbanV,waterV){
    inade.water <- c()
    for(i in 1:length(urbanV)){
      urban <- urbanV[i]
      water <- tolower(waterV[i])
      if(urban==1){
        if(is.na(water) | water == 99 | water=="other" | water=="missing" ){
          inade.water <- c(inade.water,NA)
        }else if(water %in% urban.protected.sources){
          inade.water <- c(inade.water,0)
        }else{
          inade.water <- c(inade.water,1)
        }
      }else if(urban==0){
        if(is.na(water) | water == 99 | water=="other" | water=="missing" ){
          inade.water <- c(inade.water,NA)
        }else if(water %in% rural.protected.sources){
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
  
  hr$inade.water <- code.inade.water(hr$urban,hr$water)
  
  ade.toilets = c(
    11,  "flush to piped sewer system"
    ,12,  "flush to septic tank"
    ,13,  "flush to pit latrine"
    ,21,  "ventilated improved pit latrine (vip)"
    )
  
  code.toilets <- function(toiletsV,share.toiletsV){
    inade.toilets <- c()
    for(i in 1:length(toiletsV)){
      toilets <- tolower(toiletsV[i])
      share.toilets <- tolower(share.toiletsV[i])
      if(is.na(share.toilets)){
        share.toilets = 0
      }
      if(share.toilets==1 | share.toilets=="yes"){
        inade.toilet = 1
      }else if(is.na(toilets) | toilets==96 | toilets==99 | toilets=="other" | toilets=="missing"){
        inade.toilet = NA
      }else if(toilets %in% ade.toilets){
        inade.toilet = 0
      }else{
        inade.toilet = 1
      }
      inade.toilets <- c(inade.toilets,inade.toilet)
    }
    return(inade.toilets)
  }
  hr$inade.toilets <- code.toilets(hr$toilets,hr$share.toilets)
  
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
  hr <- transform(hr,ubn = inade.materials+crowded+inade.sani+hed)
  
  #Calc wealth where half of households own tv
  recode.asset <- function(x){
    if(is.null(x)){return(NA)}
    else if(is.na(x) | x==9){return(NA)}
    else if(x==1 | tolower(x)=="yes"){return(1)}
    else if(x==0 | tolower(x)=="no"){return(0)}
    else{return(NA)}
  }
  hr$tv <- sapply(hr$tv,recode.asset)
  tv.glm <- glm(tv~wealth,data=hr,family="binomial")
  tv.pred.wealth <- (-1*tv.glm$coefficients[[1]])/tv.glm$coefficients[[2]]
  
  #Calc wealth where half of households own fridge
  hr$fridge <- sapply(hr$fridge,recode.asset)
  fridge.glm <- glm(fridge~wealth,data=hr,family="binomial")
  fridge.pred.wealth <- (-1*fridge.glm$coefficients[[1]])/fridge.glm$coefficients[[2]]
  
  #Calc wealth where half of households own car
  hr$car <- sapply(hr$car,recode.asset)
  car.glm <- glm(car~wealth,data=hr,family="binomial")
  car.pred.wealth <- (-1*car.glm$coefficients[[1]])/car.glm$coefficients[[2]]
  
  #Calc wealth where half of households own phone
  hr$phone <- sapply(hr$phone,recode.asset)
  phone.glm <- glm(phone~wealth,data=hr,family="binomial")
  phone.pred.wealth <- (-1*phone.glm$coefficients[[1]])/phone.glm$coefficients[[2]]
  
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
  
  keep = c("household","cluster","wealth","inade.materials","crowded","inade.sani","hed","ubn","tv","phone","car","fridge")
  data <- hr[keep]
  
  return(
    list(
      data = data
      ,cuts = cuts
      )
    )
}

system.time(cwiList <- cwi(hrwd))

cuts <- cwiList[["cuts"]]
data <- cwiList[["data"]] 
