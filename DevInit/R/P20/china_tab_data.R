library(data.table)
library(plyr)
library(Hmisc)

wd <- "D:/Documents/Data/MICSmeta/"
setwd(wd)

povcalcuts <- read.csv("headcounts.csv",as.is=TRUE)

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

wd <- "D:/Documents/Data/ChinaSurvey/"
setwd(wd)

load("dat2012.RData")
load("wealth.RData")

hr <- dat
ir <- famros
ch <- data.frame(child,as.is=TRUE,check.names=FALSE)

#Rename sample.weights var
names(hr)[which(names(hr)=="fswt_natcs12")] <- "sample.weights"
hr$weights <- hr$sample.weights/100000

#Rename urban var
names(hr)[which(names(hr)=="urban12")] <- "urban.rural"
recode.urban.rural <- function(x){
  if(is.null(x)){return(NA)}
  else if(is.na(x)){return(NA)}
  else if(tolower(x)=="urban" | x==1){return(1)}
  else if(tolower(x)=="rural" | x==2){return(0)}
  else{return(NA)}
}
hr$urban <- sapply(hr$urban.rural,recode.urban.rural)

#Rename educ var
names(ir)[which(names(ir)=="tb4_a12_p")] <- "educ"
recode.educ <- function(x){
  if(is.na(x)){return(NA)}
  else if(tolower(x)=="na" | tolower(x)=="unknown"){return(NA)}
  else if(tolower(x)=="illiterate/semi-literate"){return("No education, preschool")}
  else if(tolower(x)=="primary school"){return("Primary")}
  else if(tolower(x)=="junior high school"){return("Secondary")}
  else{return("Higher")}
}
ir$educ <- sapply(ir$educ,recode.educ)
ir$educ <- factor(ir$educ
                          ,levels = c("No education, preschool","Primary","Secondary","Higher")
)

#Rename age/sex var
names(ir)[which(names(ir)=="tb1b_a_p")] <- "age"
ir$age[which(ir$age<0)] <- NA
names(ir)[which(names(ir)=="tb2_a_p")] <- "sex"
ir$sex[which(ir$sex=="NA")] <- NA

#Weight and height
famros.birthdays <- famros[c("pid","fid12","tb1y_a_p","tb1m_a_p")]
ch <- data.frame(child,as.is=TRUE,check.names=FALSE)
ch <- join(
  ch
  ,famros.birthdays
  ,by=c("pid","fid12")
  )

code.age.months <- function(cyearV,cmonthV,byearV,bmonthV,ageV){
  age.monthsV <- c()
  for(i in 1:length(cyearV)){
    cyear <- cyearV[i]
    cmonth <- cmonthV[i]
    byear <- byearV[i]
    bmonth <- bmonthV[i]
    age <- ageV[i]
    if(is.na(bmonth)){
      age.months <- age*12
    }
    else if(cmonth==bmonth){
      age.months <- (cyear - byear)*12
    }else if(cmonth>bmonth){
      age.months <- (cyear - byear)*12 + (cmonth-bmonth)
    }else if(cmonth<bmonth){
      age.months <- ((cyear - byear) - 1)*12 + (12 - (bmonth-cmonth))
    }
    if(!is.na(age.months)){
      if(age.months<0){
        age.months <- 0
      } 
    }
    age.monthsV <- c(age.monthsV,age.months)
  }
  return(age.monthsV)
}
ch$tb1m_a_p[which(ch$tb1m_a_p<0)] <- NA
ch$cfps2012_age[which(ch$cfps2012_age<0)] <- NA
ch$age.months <- code.age.months(ch$cyear,ch$cmonth,ch$cfps2012_birthy_best,ch$tb1m_a_p,ch$cfps2012_age)

names(ch)[which(names(ch)=="wa103")] <- "weight.kg"
ch$weight.kg[which(ch$weight.kg<0)] <- NA
ch$weight.kg <- ch$weight.kg/2
names(ch)[which(names(ch)=="wa104")] <- "height.cm"
ch$height.cm[which(ch$height.cm<0)] <- NA
ch <- subset(ch,age.months<=60)
names(ch)[which(names(ch)=="cfps2012_gender")] <- "gender"
ch$gender <- unfactor(ch$gender)
ch$gender[which(ch$gender=="NA")] <- NA
ch$gender[which(ch$gender=="Male")] <- 1
ch$gender[which(ch$gender=="Female")] <- 2
names(ch)[which(names(ch)=="rswt_natcs12")] <- "weights"
ch$weights <- ch$weights/100000
names(ch)[which(names(ch)=="cid")] <- "cluster"
names(ch)[which(names(ch)=="fid12")] <- "household"
ch <- ch[complete.cases(ch[c("weight.kg","height.cm","age.months","gender","weights")]),]
keep <- c("cluster","household","pid","weight.kg","height.cm","age.months","gender","weights")
ch <- ch[keep]

igu.dir <- "D:/Documents/igrowup_R/"
weianthro<-read.table(paste0(igu.dir,"/weianthro.txt"),header=T,sep="",skip=0)
lenanthro<-read.table(paste0(igu.dir,"/lenanthro.txt"),header=T,sep="",skip=0)
bmianthro<-read.table(paste0(igu.dir,"/bmianthro.txt"),header=T,sep="",skip=0)
hcanthro<-read.table(paste0(igu.dir,"/hcanthro.txt"),header=T,sep="",skip=0)
acanthro<-read.table(paste0(igu.dir,"/acanthro.txt"),header=T,sep="",skip=0)
ssanthro<-read.table(paste0(igu.dir,"/ssanthro.txt"),header=T,sep="",skip=0)
tsanthro<-read.table(paste0(igu.dir,"/tsanthro.txt"),header=T,sep="",skip=0)
wflanthro<-read.table(paste0(igu.dir,"/wflanthro.txt"),header=T,sep="",skip=0)
wfhanthro<-read.table(paste0(igu.dir,"/wfhanthro.txt"),header=T,sep="",skip=0)
source(paste0(igu.dir,"igrowup_standard.r"))
source(paste0(igu.dir,"igrowup_restricted.r"))
igrowup.restricted(FileLab="ch",FilePath=igu.dir,
                   mydf=ch, sex=gender
                   , age=age.months, age.month=TRUE
                   , weight=weight.kg
                   , lenhei=height.cm
                   , sw=weights)

zscores <- read.csv(paste0(igu.dir,"ch_z_rc.csv"))
describe(zscores$flen)
plot(zscores$zlen[order(zscores$zlen)])

#Rename cluster/hh var
# names(hr)[which(names(hr)=="provcd")] <- "province"
# names(hr)[which(names(hr)=="countyid")] <- "county"
names(ir)[which(names(ir)=="fid12")] <- "household"
names(hr)[which(names(hr)=="cid")] <- "cluster"
names(hr)[which(names(hr)=="fid12")] <- "household"

#Household head
hh.heads <- unique(ir$tf10pid)
head <- subset(ir,pid %in% hh.heads)
names(head)[which(names(head)=="age")] <- "head.age"
names(head)[which(names(head)=="sex")] <- "head.sex"
keep <- c("household","head.age","head.sex")
head <- head[keep]

ir <- join(
  ir
  ,head
  ,by=c("household")
)

keep <- c("cluster","household","wealth","weights","urban")
hr <- hr[keep]

ir <- join(
  ir
  ,hr
  ,by=c("household")
  )

povcalcut <- subset(povcalcuts,filename=="China")$hc
povcalperc <- weighted.percentile(ir$wealth,ir$weights,prob=povcalcut)

ir$p20 <- (ir$wealth <= povcalperc)

keep <- c("wealth","weights","urban","educ","age","sex","cluster","household","head.sex","head.age","p20"
          ,"birth.cert","birth.reg","age.months","weight.kg","height.cm","standing.lying","child.height.age"
          ,"woman.bmi","man.bmi"
)
irNames <- names(ir)
namesDiff <- setdiff(keep,irNames)
if(length(namesDiff)>0){
  for(y in 1:length(namesDiff)){
    ir[namesDiff[y]] <- NA
    message(paste("Missing variable",namesDiff[y]))
  } 
}
ir <- ir[keep]
