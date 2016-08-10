library(Hmisc)
library(plyr)
library(foreign)
library(data.table)
library(descr)
library(WDI)
library(varhandle)
require(zoo)

# Stop crosstab from plotting everything
options(descr.plot = FALSE)

setwd("D:/Documents/Data/MICSmeta")
load("total_triple.RData")

data.total$sex <- factor(data.total$sex,levels=c("Male","Female"))

pop.confidence <- function(x,y,w,pop){
  ct <- crosstab(x,y,weight=w,prop.t=TRUE,drop.levels=FALSE)
  props <- ct$prop.tbl
  cv <- sd(w,na.rm=TRUE)/mean(w,na.rm=TRUE)
  deft <- cv*cv+1
  n <- ct$total.n
  SEs <- sqrt(((1-(n/pop))/n)*(pop/(pop-1))*(props*(1-props)))
  corrected.SEs <- SEs*deft
  low.end <- (props-(2*corrected.SEs))*pop
  low.end <- pmax(low.end,0)
  estimate.point <- props*pop
  high.end <- (props+(2*corrected.SEs))*pop
  high.end <- pmin(high.end,pop)
  return(
    list(
      low = low.end
      ,estimate = estimate.point
      ,high = high.end
    )
  )
}

countryMeta <- read.csv("headcounts.csv",as.is=TRUE)

indicator <- "SI.POV.NAHC"

dat <- WDI(country = "all", 
           indicator = indicator, 
           start = 1990, 
           end = 2016,
           extra = TRUE
           #cache = new_cache
)

source("C:/git/alexm-util/DevInit/R/P20/povcal_api2.R")

dat <- dat[c("iso3c","year","SI.POV.NAHC")]
names(dat) <- c("iso3","year","hc")
dat$hc <- dat$hc/100
dat <- dat[order(dat$year),]
dat <- dat[order(dat$iso3),]
colname <- "hc"
dat <- ddply(dat,.(iso3),function(x)
{
  naLen <- nrow(x[which(is.na(x[,colname])),])
  allLen <- nrow(x)
  valueLen <- allLen-naLen
  ival <- x[,colname]
  x[,paste("original",colname,sep="-")] <- ival 
  if(valueLen>=2)
  {
    interpVals <- na.approx(x[,colname],na.rm=FALSE,rule=2)
  }
  else if(valueLen==1){
    interpVals <- rep(sum(x[,colname],na.rm=TRUE),allLen)
  }
  else{
    interpVals <- rep(NA,allLen)
  }
  x[,colname] <- interpVals
  return(x)
}
)
names(dat) <- c("iso3","year","pl.hc","pl.hc.original")
countryMeta <- join(countryMeta,dat,by=c("iso3","year"))

newNames <- c("p20.rural"
              ,"p20.urban"
              ,"p80.rural"
              ,"p80.urban"
              ,"p80.over25.noeduc"
              ,"p80.over25.primary"
              ,"p80.over25.secondary"
              ,"p80.over25.higher"
              ,"p20.over25.noeduc"
              ,"p20.over25.primary"
              ,"p20.over25.secondary"
              ,"p20.over25.higher"
              ,"p20.male"
              ,"p20.female"
              ,"p80.male"
              ,"p80.female"
              ,"p20.male.head"
              ,"p20.female.head"
              ,"p80.male.head"
              ,"p80.female.head"
              ,"p80.unregistered"
              ,"p80.registered"
              ,"p20.unregistered"
              ,"p20.registered"
              ,"p80.notstunted"
              ,"p80.stunted"
              ,"p20.notstunted"
              ,"p20.stunted"
              ,"np20.rural"
              ,"np20.urban"
              ,"np80.rural"
              ,"np80.urban"
              ,"np80.over25.noeduc"
              ,"np80.over25.primary"
              ,"np80.over25.secondary"
              ,"np80.over25.higher"
              ,"np20.over25.noeduc"
              ,"np20.over25.primary"
              ,"np20.over25.secondary"
              ,"np20.over25.higher"
              ,"np20.male"
              ,"np20.female"
              ,"np80.male"
              ,"np80.female"
              ,"np20.male.head"
              ,"np20.female.head"
              ,"np80.male.head"
              ,"np80.female.head"
              ,"np80.unregistered"
              ,"np80.registered"
              ,"np20.unregistered"
              ,"np20.registered"
              ,"np80.notstunted"
              ,"np80.stunted"
              ,"np20.notstunted"
              ,"np20.stunted"
              ,"surveyed.pop"
              ,"surveyed.households"
              ,"surveyed.men"
              ,"surveyed.women"
)

for(i in 1:length(newNames)){
  countryMeta[[newNames[i]]] <- NA
}

filenames <- countryMeta$filename
for(i in 1:length(filenames)){
  this.filename <- filenames[i]
  message(this.filename)
  dat <- subset(data.total,filename==this.filename)
  surveyed.pop <- nrow(dat)
  surveyed.households <- length(unique(dat$household))
  under5 <- subset(dat,age<5)
  over5 <- subset(dat,age>=5)
  under15 <- subset(dat,age<15)
  over15 <- subset(dat,age>=15)
  over25 <- subset(dat,age>=25)
  women <- subset(dat,sex=="Female")
  men <- subset(dat,sex=="Male")
  surveyed.pop <- nrow(dat)
  countryMeta$surveyed.pop[which(countryMeta$filename==this.filename)] <- surveyed.pop
  surveyed.households <- nrow(unique(data.frame(dat)[c("cluster","household")]))
  countryMeta$surveyed.households[which(countryMeta$filename==this.filename)] <- surveyed.households
  surveyed.men <- nrow(men)
  countryMeta$surveyed.men[which(countryMeta$filename==this.filename)] <- surveyed.men
  surveyed.women <- nrow(women)
  countryMeta$surveyed.women[which(countryMeta$filename==this.filename)] <- surveyed.women
  if(nrow(dat)>0){
    this.pop <- subset(countryMeta,filename==this.filename)$pop.total
    this.pop.under5 <- subset(countryMeta,filename==this.filename)$female.under5 + subset(countryMeta,filename==this.filename)$male.under5
    this.pop.over5 <- this.pop - this.pop.under5
    this.pop.under15 <- this.pop.under5 + subset(countryMeta,filename==this.filename)$female.5.14 +
      subset(countryMeta,filename==this.filename)$male.5.14
    this.pop.over15 <- this.pop - this.pop.under15
    this.pop.female <- subset(countryMeta,filename==this.filename)$pop.female
    this.pop.male <- subset(countryMeta,filename==this.filename)$pop.male
    this.pop.over25.male <- subset(countryMeta,filename==this.filename)$male.25.plus
    this.pop.over25.female <- subset(countryMeta,filename==this.filename)$female.25.plus
    this.pop.over25 <- this.pop.over25.male + this.pop.over25.female
    #Urban-P20
    if(length(dat$urban[which(!is.na(dat$urban))])!=0){
      confidence.tab <- pop.confidence(dat$urban,dat$p20,dat$weights,this.pop)
      countryMeta$p80.rural[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["0","FALSE"]},error=function(e){0})
      countryMeta$p80.urban[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["1","FALSE"]},error=function(e){0})
      countryMeta$p20.rural[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["0","TRUE"]},error=function(e){0})
      countryMeta$p20.urban[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["1","TRUE"]},error=function(e){0})
    }
    #Urban-NP20
    if(length(dat$urban[which(!is.na(dat$urban))])!=0){
      confidence.tab <- pop.confidence(dat$urban,dat$np20,dat$weights,this.pop)
      countryMeta$np80.rural[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["0","FALSE"]},error=function(e){0})
      countryMeta$np80.urban[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["1","FALSE"]},error=function(e){0})
      countryMeta$np20.rural[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["0","TRUE"]},error=function(e){0})
      countryMeta$np20.urban[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["1","TRUE"]},error=function(e){0})
    }
    #Educ-P20
    if(length(over25$educ[which(!is.na(over25$educ))])!=0){
      confidence.tab <- pop.confidence(over25$educ,over25$p20,over25$weights,this.pop.over25)
      countryMeta$p80.over25.noeduc[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["No education, preschool","FALSE"]},error=function(e){0})
      countryMeta$p80.over25.primary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Primary","FALSE"]},error=function(e){0})
      countryMeta$p80.over25.secondary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Secondary","FALSE"]},error=function(e){0})
      countryMeta$p80.over25.higher[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Higher","FALSE"]},error=function(e){0})
      countryMeta$p20.over25.noeduc[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["No education, preschool","TRUE"]},error=function(e){0})
      countryMeta$p20.over25.primary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Primary","TRUE"]},error=function(e){0})
      countryMeta$p20.over25.secondary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Secondary","TRUE"]},error=function(e){0})
      countryMeta$p20.over25.higher[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Higher","TRUE"]},error=function(e){0})
    }
    #Educ-NP20
    if(length(over25$educ[which(!is.na(over25$educ))])!=0){
      confidence.tab <- pop.confidence(over25$educ,over25$np20,over25$weights,this.pop.over25)
      countryMeta$np80.over25.noeduc[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["No education, preschool","FALSE"]},error=function(e){0})
      countryMeta$np80.over25.primary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Primary","FALSE"]},error=function(e){0})
      countryMeta$np80.over25.secondary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Secondary","FALSE"]},error=function(e){0})
      countryMeta$np80.over25.higher[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Higher","FALSE"]},error=function(e){0})
      countryMeta$np20.over25.noeduc[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["No education, preschool","TRUE"]},error=function(e){0})
      countryMeta$np20.over25.primary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Primary","TRUE"]},error=function(e){0})
      countryMeta$np20.over25.secondary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Secondary","TRUE"]},error=function(e){0})
      countryMeta$np20.over25.higher[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Higher","TRUE"]},error=function(e){0})
    }
    #Sex-P20
    if(length(dat$sex[which(!is.na(dat$sex))])!=0){
      confidence.tab <- pop.confidence(dat$sex,dat$p20,dat$weights,this.pop)
      countryMeta$p80.male[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Male","FALSE"]},error=function(e){0})
      countryMeta$p80.female[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Female","FALSE"]},error=function(e){0})
      countryMeta$p20.male[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Male","TRUE"]},error=function(e){0})
      countryMeta$p20.female[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Female","TRUE"]},error=function(e){0}) 
    }
    #Sex-NP20
    if(length(dat$sex[which(!is.na(dat$sex))])!=0){
      confidence.tab <- pop.confidence(dat$sex,dat$np20,dat$weights,this.pop)
      countryMeta$np80.male[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Male","FALSE"]},error=function(e){0})
      countryMeta$np80.female[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Female","FALSE"]},error=function(e){0})
      countryMeta$np20.male[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Male","TRUE"]},error=function(e){0})
      countryMeta$np20.female[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Female","TRUE"]},error=function(e){0}) 
    }
    #Head-sex-P20
    if(length(dat$head.sex[which(!is.na(dat$head.sex))])!=0){
      confidence.tab <- pop.confidence(dat$head.sex,dat$p20,dat$weights,this.pop)
      countryMeta$p80.male.head[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Male","FALSE"]},error=function(e){0})
      countryMeta$p80.female.head[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Female","FALSE"]},error=function(e){0})
      countryMeta$p20.male.head[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Male","TRUE"]},error=function(e){0})
      countryMeta$p20.female.head[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Female","TRUE"]},error=function(e){0}) 
    }
    #Head-sex-NP20
    if(length(dat$head.sex[which(!is.na(dat$head.sex))])!=0){
      confidence.tab <- pop.confidence(dat$head.sex,dat$np20,dat$weights,this.pop)
      countryMeta$np80.male.head[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Male","FALSE"]},error=function(e){0})
      countryMeta$np80.female.head[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Female","FALSE"]},error=function(e){0})
      countryMeta$np20.male.head[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Male","TRUE"]},error=function(e){0})
      countryMeta$np20.female.head[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Female","TRUE"]},error=function(e){0}) 
    }
    #Under5 registration
    if(length(under5$birth.reg[which(!is.na(under5$birth.reg))])!=0){
      confidence.tab <- pop.confidence(under5$birth.reg,under5$p20,under5$weights,this.pop.under5)
      countryMeta$p80.unregistered[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["0","FALSE"]},error=function(e){0})
      countryMeta$p80.registered[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["1","FALSE"]},error=function(e){0})
      countryMeta$p20.unregistered[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["0","TRUE"]},error=function(e){0})
      countryMeta$p20.registered[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["1","TRUE"]},error=function(e){0})  
    }
    #Under5 registration
    if(length(under5$birth.reg[which(!is.na(under5$birth.reg))])!=0){
      confidence.tab <- pop.confidence(under5$birth.reg,under5$np20,under5$weights,this.pop.under5)
      countryMeta$np80.unregistered[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["0","FALSE"]},error=function(e){0})
      countryMeta$np80.registered[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["1","FALSE"]},error=function(e){0})
      countryMeta$np20.unregistered[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["0","TRUE"]},error=function(e){0})
      countryMeta$np20.registered[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["1","TRUE"]},error=function(e){0})  
    }
    #Under5 nutrition p20
    under5$stunted <- (under5$child.height.age <= -2) & (under5$child.height.age > -6)
    under5$stunted[which(is.na(under5$stunting))] <- NA
    if(length(under5$stunted[which(!is.na(under5$stunted))])!=0){
      confidence.tab <- pop.confidence(under5$stunted,under5$p20,under5$weights,this.pop.under5)
      countryMeta$p80.notstunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["FALSE","FALSE"]},error=function(e){0})
      countryMeta$p80.stunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["TRUE","FALSE"]},error=function(e){0})
      countryMeta$p20.notstunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["FALSE","TRUE"]},error=function(e){0})
      countryMeta$p20.stunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["TRUE","TRUE"]},error=function(e){0})  
    }
    #Under5 nutrition np20
    if(length(under5$stunted[which(!is.na(under5$stunted))])!=0){
      confidence.tab <- pop.confidence(under5$stunted,under5$np20,under5$weights,this.pop.under5)
      countryMeta$np80.notstunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["FALSE","FALSE"]},error=function(e){0})
      countryMeta$np80.stunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["TRUE","FALSE"]},error=function(e){0})
      countryMeta$np20.notstunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["FALSE","TRUE"]},error=function(e){0})
      countryMeta$np20.stunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["TRUE","TRUE"]},error=function(e){0})  
    }
  }
}

write.csv(countryMeta,"p20_np20_bycountry_tabs.csv",row.names=FALSE,na="")