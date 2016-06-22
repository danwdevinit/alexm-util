library(Hmisc)
library(plyr)
library(foreign)
library(data.table)
library(descr)
library(varhandle)

# Stop crosstab from plotting everything
options(descr.plot = FALSE)

wd <- "D:/Documents/Data/MICSmeta"
setwd(wd)
load("total_crosstabs_coded.RData")
load("D:/Documents/Data/ChinaSurvey/crosstab.RData")
load("D:/Documents/Data/BrazilSurvey/spss/crosstab.RData")
data.total <- rbind(data.total,china.data.total,fill=TRUE)
data.total <- rbind(data.total,brazil.data.total,fill=TRUE)

data.total$sex <- factor(data.total$sex,levels=c("Male","Female"))

conform <- function(complete,incomplete){
  return(
    pmax(
      incomplete[
        match(
          rownames(complete)
          ,rownames(incomplete)
        )
        ,]
      ,0
      ,na.rm=TRUE
    )
  )
}

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

####Debug####
# dat.tab <- data.table(data.total)
# dat.collapse <- dat.tab[
#   ,.(p20=weighted.mean(p20,weights,na.rm=TRUE))
#   , by=.(filename)
#   ]
# countryMeta <- join(countryMeta,dat.collapse,by="filename")
####End debug####

newNames <- c("p20.rural"
              ,"p20.urban"
              ,"p80.over15.noeduc"
              ,"p80.over15.primary"
              ,"p80.over15.secondary"
              ,"p80.over15.higher"
              ,"p20.over15.noeduc"
              ,"p20.over15.primary"
              ,"p20.over15.secondary"
              ,"p20.over15.higher"
              ,"p80.male"
              ,"p80.female"
              ,"p20.male"
              ,"p20.female"
              ,"p80.unregistered"
              ,"p80.registered"
              ,"p20.unregistered"
              ,"p20.registered"
              ,"p80.notstunted"
              ,"p80.stunted"
              ,"p20.notstunted"
              ,"p20.stunted"
)

for(i in 1:length(newNames)){
  countryMeta[[newNames[i]]] <- NA
}

filenames <- countryMeta$filename
for(i in 1:length(filenames)){
  this.filename <- filenames[i]
  message(this.filename)
  dat <- subset(data.total,filename==this.filename)
  under5 <- subset(dat,age<5)
  over5 <- subset(dat,age>=5)
  under15 <- subset(dat,age<15)
  over15 <- subset(dat,age>=15)
  women <- subset(dat,sex=="Female")
  men <- subset(dat,sex=="Male")
  if(nrow(dat)>0){
    this.pop <- subset(countryMeta,filename==this.filename)$pop.total
    this.pop.under5 <- subset(countryMeta,filename==this.filename)$female.under5 + subset(countryMeta,filename==this.filename)$male.under5
    this.pop.over5 <- this.pop - this.pop.under5
    this.pop.under15 <- this.pop.under5 + subset(countryMeta,filename==this.filename)$female.5.14 +
    subset(countryMeta,filename==this.filename)$male.5.14
    this.pop.over15 <- this.pop - this.pop.under15
    this.pop.female <- subset(countryMeta,filename==this.filename)$pop.female
    this.pop.male <- subset(countryMeta,filename==this.filename)$pop.male
    #Urban-P20
    if(length(dat$urban[which(!is.na(dat$urban))])!=0){
      confidence.tab <- pop.confidence(dat$urban,dat$p20,dat$weights,this.pop)
      countryMeta$p80.rural[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["0","FALSE"]},error=function(e){0})
      countryMeta$p80.urban[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["1","FALSE"]},error=function(e){0})
      countryMeta$p20.rural[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["0","TRUE"]},error=function(e){0})
      countryMeta$p20.urban[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["1","TRUE"]},error=function(e){0})
    }
    #Educ-P20
    if(length(over15$educ[which(!is.na(over15$educ))])!=0){
      confidence.tab <- pop.confidence(over15$educ,over15$p20,over15$weights,this.pop.over15)
      countryMeta$p80.over15.noeduc[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["No education, preschool","FALSE"]},error=function(e){0})
      countryMeta$p80.over15.primary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Primary","FALSE"]},error=function(e){0})
      countryMeta$p80.over15.secondary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Secondary","FALSE"]},error=function(e){0})
      countryMeta$p80.over15.higher[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Higher","FALSE"]},error=function(e){0})
      countryMeta$p20.over15.noeduc[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["No education, preschool","TRUE"]},error=function(e){0})
      countryMeta$p20.over15.primary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Primary","TRUE"]},error=function(e){0})
      countryMeta$p20.over15.secondary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Secondary","TRUE"]},error=function(e){0})
      countryMeta$p20.over15.higher[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Higher","TRUE"]},error=function(e){0})
    }
    #Sex-P20
    if(length(dat$sex[which(!is.na(dat$sex))])!=0){
      confidence.tab <- pop.confidence(dat$sex,dat$p20,dat$weights,this.pop)
      countryMeta$p80.male[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Male","FALSE"]},error=function(e){0})
      countryMeta$p80.female[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Female","FALSE"]},error=function(e){0})
      countryMeta$p20.male[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Male","TRUE"]},error=function(e){0})
      countryMeta$p20.female[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Female","TRUE"]},error=function(e){0}) 
    }
    #Head-sex-P20
    if(length(dat$head.sex[which(!is.na(dat$head.sex))])!=0){
      confidence.tab <- pop.confidence(dat$head.sex,dat$p20,dat$weights,this.pop)
      countryMeta$p80.male[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Male","FALSE"]},error=function(e){0})
      countryMeta$p80.female[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Female","FALSE"]},error=function(e){0})
      countryMeta$p20.male[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Male","TRUE"]},error=function(e){0})
      countryMeta$p20.female[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Female","TRUE"]},error=function(e){0}) 
    }
    #Under5 registration
    if(length(under5$birth.reg[which(!is.na(under5$birth.reg))])!=0){
      confidence.tab <- pop.confidence(under5$birth.reg,under5$p20,under5$weights,this.pop.under5)
      countryMeta$p80.unregistered[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["0","FALSE"]},error=function(e){0})
      countryMeta$p80.registered[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["1","FALSE"]},error=function(e){0})
      countryMeta$p20.unregistered[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["0","TRUE"]},error=function(e){0})
      countryMeta$p20.registered[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["1","TRUE"]},error=function(e){0})  
    }
    #Under5 nutrition
      under5$stunted <- (under5$child.height.age <= -2) & (under5$child.height.age > -6)
      under5$stunted[which(is.na(under5$stunting))] <- NA
    if(length(under5$stunted[which(!is.na(under5$stunted))])!=0){
      confidence.tab <- pop.confidence(under5$stunted,under5$p20,under5$weights,this.pop.under5)
      countryMeta$p80.notstunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["FALSE","FALSE"]},error=function(e){0})
      countryMeta$p80.stunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["TRUE","FALSE"]},error=function(e){0})
      countryMeta$p20.notstunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["FALSE","TRUE"]},error=function(e){0})
      countryMeta$p20.stunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["TRUE","TRUE"]},error=function(e){0})  
    }
  }
}

write.csv(countryMeta,"p20_urb_rural.csv",row.names=FALSE,na="")
