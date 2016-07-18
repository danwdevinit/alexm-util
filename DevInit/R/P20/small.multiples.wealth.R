####Function and setup####
library(Hmisc)
library(plyr)
library(foreign)
library(data.table)
library(ggplot2)

wd <- "D:/Documents/Data/MICSmeta/"
setwd(wd)

countryMeta <- read.csv("headcounts.csv",as.is=TRUE)

recode.urban <- function(x){
  if(is.na(x)){return(NA)}
  else if(x==0 | tolower(x)=="rural"){return("Rural")}
  else if(x==1 | tolower(x)=="urban"){return("Urban")}
  else{return(NA)}
}

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

# dir <- "D:/Documents/Data/DHSauto/ughr72dt"

####Run function####
# set our working directory, change this if using on another machine
wd <- "D:/Documents/Data/DHSauto/"
setwd(wd)

# List out all the directories in our wd, this is where our data is contained
dirs <- list.dirs(wd,full.names=TRUE)

dataList <- list()
dataList.urbrur <- list()
dataIndex <- 1

# Loop through every dir
for(i in 2:length(dirs)){
  dir <- dirs[i]
  hrBase <- basename(dir)
  if(hrBase %in% countryMeta$filename){
    message(hrBase)
    hrwd <- dir
    if(!file_test(op="-d", hrwd)){next;}
    
    iso2 <- toupper(substr(hrBase,1,2))
    phase <- substr(hrBase,5,6)
    
    prwd <- paste0("D:/Documents/Data/DHSauto/",tolower(iso2),"pr",phase,"dt/")
    if(!file_test(op="-d", prwd)){next;}
    
    pr <- read.csv(paste0(prwd,iso2,"PR",phase,"FL.csv")
                   ,na.strings="",as.is=TRUE,check.names=FALSE)
    
    iso3 <- countryMeta$iso3[which(countryMeta$filename==hrBase)]
    
    names(pr)[which(names(pr)=="hv271")] <- "wealth"
    pr$wealth <- pr$wealth/100000
    
    #Rename sample.weights var
    names(pr)[which(names(pr)=="hv005")] <- "sample.weights"
    pr$weights <- pr$sample.weights/1000000
    
    #Rename urban var
    names(pr)[which(names(pr)=="hv025")] <- "urban.rural"
    pr$urban <- sapply(pr$urban.rural,recode.urban)
    
    #Rename age var
    names(pr)[which(names(pr)=="hv105")] <- "age"
    
    #Rename sex var
    names(pr)[which(names(pr)=="hv104")] <- "sex"
    
    #Rename cluster/hh var
    names(pr)[which(names(pr)=="hv001")] <- "cluster"
    names(pr)[which(names(pr)=="hv002")] <- "household"
    
    povcalcut <- subset(countryMeta,filename==hrBase)$hc
    povcalperc <- weighted.percentile(pr$wealth,pr$weights,prob=povcalcut)
    
    weighted.wealths <- weighted.percentile(pr$wealth,pr$weights,prob=seq(0,1,length=1001))
    weighted.wealths <- weighted.wealths - povcalperc
    
    urb <- subset(pr,urban=="Urban")
    weighted.wealths.urban <- weighted.percentile(urb$wealth,urb$weights,prob=seq(0,1,length=1001))
    weighted.wealths.urban <- data.frame(weighted.wealths.urban)
    weighted.wealths.urban$urban <- "Urban"
    names(weighted.wealths.urban) <- c("wealth","urban")
    rur <- subset(pr,urban=="Rural")
    weighted.wealths.rural <- weighted.percentile(rur$wealth,rur$weights,prob=seq(0,1,length=1001))
    weighted.wealths.rural <- data.frame(weighted.wealths.rural)
    weighted.wealths.rural$urban <- "Rural"
    names(weighted.wealths.rural) <- c("wealth","urban")
    weighted.wealths.ur <- rbindlist(list(weighted.wealths.urban,weighted.wealths.rural))
    weighted.wealths.ur$wealth <- weighted.wealths.ur$wealth - povcalperc
    
    pic.file <- paste0("D:/Documents/P20_small_wealth_multiples/individual/",iso3,".jpg")
    pic.file.r <- paste0("D:/Documents/P20_small_wealth_multiples/individual/",iso3,"_urb_rur.jpg")
    r <- ggplot(weighted.wealths.ur,aes(x=wealth)) + geom_density(aes(group=urban,colour=urban,fill=urban),alpha=0.3)
    r <- r + theme_bw() + theme(legend.title=element_blank()) + labs(title=iso3,x="Adj. wealth",y="Density")
    ggsave(filename=pic.file.r,plot=r,height=5,width=8,units="in")
    d <- ggplot(data.frame(weighted.wealths),aes(x=weighted.wealths)) + geom_density(aes(fill=1),alpha=0.3)
    d <- d + theme_bw() + theme(legend.position="none") + labs(title=iso3,x="Adj. wealth",y="Density")
    ggsave(filename=pic.file,plot=d,height=5,width=8,units="in")
    
    weighted.wealths <- data.frame(weighted.wealths)
    weighted.wealths$iso3 <- iso3
    weighted.wealths.ur <- data.frame(weighted.wealths.ur)
    weighted.wealths.ur$iso3 <- iso3
    dataList[[dataIndex]] <- weighted.wealths
    dataList.urbrur[[dataIndex]] <- weighted.wealths.ur
    dataIndex <- dataIndex + 1
  }
}

setwd("D:/Documents/Data/MICSmeta")
varNames <- read.csv("mics_meta_vars_complete.csv",as.is=TRUE,na.strings="")
classes <- read.csv("global_mics_classes.csv",as.is=TRUE,na.strings="NAN")

wd <- "D:/Documents/Data/MICSauto/"
setwd(wd)

# List out all the directories in our wd, this is where our data is contained
dirs <- list.dirs(wd,full.names=TRUE)

# dir <- "D:/Documents/Data/MICSauto/Zimbabwe_MICS5_Datasets"

for(i in 2:length(dirs)){
  dir <- dirs[i]
  hrBase <- basename(dir)
  if(hrBase %in% countryMeta$filename){
    
    message(hrBase) 
    if(exists("hh")){rm(hh)}
    if(exists("hl")){rm(hl)}
    load(paste0(dir,"/","hh.RData"))
    load(paste0(dir,"/","hl.RData"))
    hh <- data.frame(hh,as.is=TRUE,check.names=FALSE)
    hl <- data.frame(hl,as.is=TRUE,check.names=FALSE)
    names(hh) <- tolower(names(hh))
    names(hl) <- tolower(names(hl))
    
    iso3 <- countryMeta$iso3[which(countryMeta$filename==hrBase)]
    
    file.varName <- subset(varNames,filename==hrBase)
    
    
    ynm.classes <- subset(classes,filename==hrBase & type=="ynm")
    urban.rural.classes <- subset(classes,filename==hrBase & type=="urban.rural")
    
    missing.vals <- subset(ynm.classes,is.na(ynm))$value
    no.vals <- subset(ynm.classes,ynm==0)$value
    yes.vals <- subset(ynm.classes,ynm==1)$value
    
    
    #Rename wealth var
    if(typeof(hh$wlthscor)=="NULL" | typeof(hh$wlthscor)=="logical" | length(hh$wlthscor[which(!is.na(hh$wlthscor))])==0){
      if(typeof(hh$wscore)=="NULL" | typeof(hh$wscore)=="logical" | length(hh$wscore[which(!is.na(hh$wscore))])==0){
        message("Wealth missing!");return(NA)
      }else{
        names(hh)[which(names(hh)=="wscore")] <- "wealth"
      }
    }else{
      names(hh)[which(names(hh)=="wlthscor")] <- "wealth"
    }
    
    #Rename sample.weights var
    names(hh)[which(names(hh)=="hhweight")] <- "weights"
    
    #Rename urban var
    names(hh)[which(names(hh)=="hh6")] <- "urban.rural"
    if(typeof(hh$urban.rural)=="NULL"){message("No urban.rural!");hh$urban.rural<-NA;urban.missing<-TRUE}else{urban.missing<-FALSE}
    
    #Rename age var
    names(hl)[which(names(hl)=="hl6")] <- "age"
    
    #Rename sex var
    names(hl)[which(names(hl)=="hl4")] <- "sex"
    
    #Rename cluster/hh var
    names(hl)[which(names(hl)=="hh1")] <- "cluster"
    names(hl)[which(names(hl)=="hh2")] <- "household"
    names(hl)[which(names(hl)=="hl1")] <- "line"
    names(hl)[which(names(hl)=="ln")] <- "line"
    names(hh)[which(names(hh)=="hh1")] <- "cluster"
    names(hh)[which(names(hh)=="hh2")] <- "household"
    
    recode.urban.rural <- function(x){
      item <- subset(urban.rural.classes,value==tolower(x))
      if(nrow(item)==0){return(NA)}
      else{item$urban[1]}
    }
    hh$urban.rural <- sapply(hh$urban.rural,recode.urban.rural)
  
    hhkeep <- c("wealth","weights","urban.rural","cluster","household")
    hhNames <- names(hh)
    namesDiff <- setdiff(hhkeep,hhNames)
    if(length(namesDiff)>0){
      for(y in 1:length(namesDiff)){
        hh[namesDiff[y]] <- NA
        message(paste("Missing variable",namesDiff[y]))
      } 
    }
    hh <- hh[hhkeep]
    hl <- join(
      hl
      ,hh
      ,by=c("cluster","household")
    )
    hl <- data.frame(hl,as.is=TRUE,check.names=FALSE)
    hl$urban <- sapply(hl$urban.rural,recode.urban)
    
    povcalcut <- subset(countryMeta,filename==hrBase)$hc
    povcalperc <- weighted.percentile(hl$wealth,hl$weights,prob=povcalcut)
    
    weighted.wealths <- weighted.percentile(hl$wealth,hl$weights,prob=seq(0,1,length=1001))
    weighted.wealths <- weighted.wealths - povcalperc
    
    urb <- subset(hl,urban=="Urban")
    weighted.wealths.urban <- weighted.percentile(urb$wealth,urb$weights,prob=seq(0,1,length=1001))
    weighted.wealths.urban <- data.frame(weighted.wealths.urban)
    weighted.wealths.urban$urban <- "Urban"
    names(weighted.wealths.urban) <- c("wealth","urban")
    rur <- subset(hl,urban=="Rural")
    weighted.wealths.rural <- weighted.percentile(rur$wealth,rur$weights,prob=seq(0,1,length=1001))
    weighted.wealths.rural <- data.frame(weighted.wealths.rural)
    weighted.wealths.rural$urban <- "Rural"
    names(weighted.wealths.rural) <- c("wealth","urban")
    weighted.wealths.ur <- rbindlist(list(weighted.wealths.urban,weighted.wealths.rural))
    weighted.wealths.ur$wealth <- weighted.wealths.ur$wealth - povcalperc
    
    pic.file <- paste0("D:/Documents/P20_small_wealth_multiples/individual/",iso3,".jpg")
    pic.file.r <- paste0("D:/Documents/P20_small_wealth_multiples/individual/",iso3,"_urb_rur.jpg")
    r <- ggplot(weighted.wealths.ur,aes(x=wealth)) + geom_density(aes(group=urban,colour=urban,fill=urban),alpha=0.3)
    r <- r + theme_bw() + theme(legend.title=element_blank()) + labs(title=iso3,x="Adj. wealth",y="Density")
    ggsave(filename=pic.file.r,plot=r,height=5,width=8,units="in")
    d <- ggplot(data.frame(weighted.wealths),aes(x=weighted.wealths)) + geom_density(aes(fill=1),alpha=0.3)
    d <- d + theme_bw() + theme(legend.position="none") + labs(title=iso3,x="Adj. wealth",y="Density")
    ggsave(filename=pic.file,plot=d,height=5,width=8,units="in")
    
    weighted.wealths <- data.frame(weighted.wealths)
    weighted.wealths$iso3 <- iso3
    weighted.wealths.ur <- data.frame(weighted.wealths.ur)
    weighted.wealths.ur$iso3 <- iso3
    dataList[[dataIndex]] <- weighted.wealths
    dataList.urbrur[[dataIndex]] <- weighted.wealths.ur
    dataIndex <- dataIndex + 1
  }
}

####China
wd <- "D:/Documents/Data/ChinaSurvey/"
setwd(wd)

load("dat2012.RData")
load("wealth.RData")

hr <- dat
ir <- famros

#Rename sample.weights var
names(hr)[which(names(hr)=="fswt_natcs12")] <- "sample.weights"
hr$weights <- hr$sample.weights/100000

#Rename age/sex var
names(ir)[which(names(ir)=="tb1b_a_p")] <- "age"
ir$age[which(ir$age<0)] <- NA
names(ir)[which(names(ir)=="tb2_a_p")] <- "sex"
ir$sex[which(ir$sex=="NA")] <- NA

#Rename urban var
names(hr)[which(names(hr)=="urban12")] <- "urban.rural"
recode.urban.rural <- function(x){
  if(is.null(x)){return(NA)}
  else if(is.na(x)){return(NA)}
  else if(tolower(x)=="urban" | x==1){return("Urban")}
  else if(tolower(x)=="rural" | x==2){return("Rural")}
  else{return(NA)}
}
hr$urban <- sapply(hr$urban.rural,recode.urban.rural)

names(ir)[which(names(ir)=="fid12")] <- "household"
names(hr)[which(names(hr)=="cid")] <- "cluster"
names(hr)[which(names(hr)=="fid12")] <- "household"

keep <- c("cluster","household","wealth","weights","urban")
hr <- hr[keep]

ir <- join(
  ir
  ,hr
  ,by=c("household")
)

hrBase <- "China"
iso3 <- "CHN"

povcalcut <- subset(countryMeta,filename=="China")$hc
povcalperc <- weighted.percentile(ir$wealth,ir$weights,prob=povcalcut)

weighted.wealths <- weighted.percentile(ir$wealth,ir$weights,prob=seq(0,1,length=1001))
weighted.wealths <- weighted.wealths - povcalperc

urb <- subset(ir,urban=="Urban")
weighted.wealths.urban <- weighted.percentile(urb$wealth,urb$weights,prob=seq(0,1,length=1001))
weighted.wealths.urban <- data.frame(weighted.wealths.urban)
weighted.wealths.urban$urban <- "Urban"
names(weighted.wealths.urban) <- c("wealth","urban")
rur <- subset(ir,urban=="Rural")
weighted.wealths.rural <- weighted.percentile(rur$wealth,rur$weights,prob=seq(0,1,length=1001))
weighted.wealths.rural <- data.frame(weighted.wealths.rural)
weighted.wealths.rural$urban <- "Rural"
names(weighted.wealths.rural) <- c("wealth","urban")
weighted.wealths.ur <- rbindlist(list(weighted.wealths.urban,weighted.wealths.rural))
weighted.wealths.ur$wealth <- weighted.wealths.ur$wealth - povcalperc

pic.file <- paste0("D:/Documents/P20_small_wealth_multiples/individual/",iso3,".jpg")
pic.file.r <- paste0("D:/Documents/P20_small_wealth_multiples/individual/",iso3,"_urb_rur.jpg")
r <- ggplot(weighted.wealths.ur,aes(x=wealth)) + geom_density(aes(group=urban,colour=urban,fill=urban),alpha=0.3)
r <- r + theme_bw() + theme(legend.title=element_blank()) + labs(title=iso3,x="Adj. wealth",y="Density")
ggsave(filename=pic.file.r,plot=r,height=5,width=8,units="in")
d <- ggplot(data.frame(weighted.wealths),aes(x=weighted.wealths)) + geom_density(aes(fill=1),alpha=0.3)
d <- d + theme_bw() + theme(legend.position="none") + labs(title=iso3,x="Adj. wealth",y="Density")
ggsave(filename=pic.file,plot=d,height=5,width=8,units="in")

weighted.wealths <- data.frame(weighted.wealths)
weighted.wealths$iso3 <- iso3
weighted.wealths.ur <- data.frame(weighted.wealths.ur)
weighted.wealths.ur$iso3 <- iso3
dataList[[dataIndex]] <- weighted.wealths
dataList.urbrur[[dataIndex]] <- weighted.wealths.ur
dataIndex <- dataIndex + 1

####Brazil
wd <- "D:/Documents/Data/BrazilSurvey/spss"
setwd(wd)

library(varhandle)

load("PNDS2006_BR_DOM_PESS.RData")
pr <- data.frame(dat,as.is=TRUE,check.names=FALSE)

load("wealth.RData")
dat <- dat[c("DOMICILIO_ID","wealth")]

pr <- pr[order(pr$DOMICILIO_ID),]
dat <- dat[order(dat$DOMICILIO_ID),]
wealth <- dat$wealth

pr <- cbind(pr,wealth)

names(pr)[which(names(pr)=="P000_NQUE")] <- "line"
names(pr)[which(names(pr)=="CD002_CONG")] <- "cluster"
names(pr)[which(names(pr)=="DOMICILIO_ID")] <- "household"

names(pr)[which(names(pr)=="CD008_SITU")] <- "urban.rural"
pr$urban <- NA
pr$urban[which(pr$urban.rural=="Urbano")] <- "Urban"
pr$urban[which(pr$urban.rural=="Rural")] <- "Rural"

names(pr)[which(names(pr)=="XP999_PESO")] <- "weights"
pr$weights <- pr$weights/10000

names(pr)[which(names(pr)=="P004_SEXO")] <- "sex"
pr$gender <- NA
pr$gender[which(pr$sex=="Masculino")] <- "Male"
pr$gender[which(pr$sex=="Feminino")] <- "Female"
pr$sex <- pr$gender
pr$gender <- NULL
names(pr)[which(names(pr)=="XP010_MELH")] <- "age"

hrBase <- "Brazil"
iso3 <- "BRA"

povcalcut <- subset(countryMeta,filename==hrBase)$hc
povcalperc <- weighted.percentile(pr$wealth,pr$weights,prob=povcalcut)

weighted.wealths <- weighted.percentile(pr$wealth,pr$weights,prob=seq(0,1,length=1001))
weighted.wealths <- weighted.wealths - povcalperc

urb <- subset(pr,urban=="Urban")
weighted.wealths.urban <- weighted.percentile(urb$wealth,urb$weights,prob=seq(0,1,length=1001))
weighted.wealths.urban <- data.frame(weighted.wealths.urban)
weighted.wealths.urban$urban <- "Urban"
names(weighted.wealths.urban) <- c("wealth","urban")
rur <- subset(pr,urban=="Rural")
weighted.wealths.rural <- weighted.percentile(rur$wealth,rur$weights,prob=seq(0,1,length=1001))
weighted.wealths.rural <- data.frame(weighted.wealths.rural)
weighted.wealths.rural$urban <- "Rural"
names(weighted.wealths.rural) <- c("wealth","urban")
weighted.wealths.ur <- rbindlist(list(weighted.wealths.urban,weighted.wealths.rural))
weighted.wealths.ur$wealth <- weighted.wealths.ur$wealth - povcalperc

pic.file <- paste0("D:/Documents/P20_small_wealth_multiples/individual/",iso3,".jpg")
pic.file.r <- paste0("D:/Documents/P20_small_wealth_multiples/individual/",iso3,"_urb_rur.jpg")
r <- ggplot(weighted.wealths.ur,aes(x=wealth)) + geom_density(aes(group=urban,colour=urban,fill=urban),alpha=0.3)
r <- r + theme_bw() + theme(legend.title=element_blank()) + labs(title=iso3,x="Adj. wealth",y="Density")
ggsave(filename=pic.file.r,plot=r,height=5,width=8,units="in")
d <- ggplot(data.frame(weighted.wealths),aes(x=weighted.wealths)) + geom_density(aes(fill=1),alpha=0.3)
d <- d + theme_bw() + theme(legend.position="none") + labs(title=iso3,x="Adj. wealth",y="Density")
ggsave(filename=pic.file,plot=d,height=5,width=8,units="in")

weighted.wealths <- data.frame(weighted.wealths)
weighted.wealths$iso3 <- iso3
weighted.wealths.ur <- data.frame(weighted.wealths.ur)
weighted.wealths.ur$iso3 <- iso3
dataList[[dataIndex]] <- weighted.wealths
dataList.urbrur[[dataIndex]] <- weighted.wealths.ur
dataIndex <- dataIndex + 1

####Aggregates

all.data <- rbindlist(dataList)
all.plot.alpha <- ggplot(all.data,aes(x=weighted.wealths)) + geom_density(aes(fill=1),alpha=0.3) + xlim(-3,3) + facet_wrap(~iso3,ncol=5) + theme_bw() + theme(legend.position="none") + labs(x="Adj. wealth",y="Density")
hcs <- countryMeta[c("iso3","hc")]
all.data <- join(all.data,hcs,by="iso3")
all.data <- all.data[order(-all.data$hc),]
all.data$iso3 <- factor(all.data$iso3,levels=unique(all.data$iso3))
all.data.urbrur <- rbindlist(dataList.urbrur)
all.urb.rur.alpha <- ggplot(all.data.urbrur,aes(x=wealth)) + geom_density(aes(group=urban,colour=urban,fill=urban),alpha=0.3) + xlim(-3,3) + facet_wrap(~iso3,ncol=5) + theme_bw() + theme(legend.title=element_blank()) + labs(x="Adj. wealth",y="Density")
all.data.urbrur <- join(all.data.urbrur,hcs,by="iso3")
all.data.urbrur <- all.data.urbrur[order(-all.data.urbrur$hc),]
all.data.urbrur$iso3 <- factor(all.data.urbrur$iso3,levels=unique(all.data.urbrur$iso3))

all.plot <- ggplot(all.data,aes(x=weighted.wealths)) + geom_density(aes(fill=1),alpha=0.3) + xlim(-3,3) + facet_wrap(~iso3,ncol=5) + theme_bw() + theme(legend.position="none") + labs(x="Adj. wealth",y="Density")
all.urb.rur <- ggplot(all.data.urbrur,aes(x=wealth)) + geom_density(aes(group=urban,colour=urban,fill=urban),alpha=0.3) + xlim(-3,3) + facet_wrap(~iso3,ncol=5) + theme_bw() + theme(legend.title=element_blank()) + labs(x="Adj. wealth",y="Density")

ggsave(filename="D:/Documents/P20_small_wealth_multiples/aggregate/all.pdf",plot=all.plot,width=8,height=30,units="in",limitsize=FALSE)
ggsave(filename="D:/Documents/P20_small_wealth_multiples/aggregate/all.urb.rur.pdf",plot=all.urb.rur,width=8,height=30,units="in",limitsize=FALSE)
ggsave(filename="D:/Documents/P20_small_wealth_multiples/aggregate/all.alpha.pdf",plot=all.plot.alpha,width=8,height=30,units="in",limitsize=FALSE)
ggsave(filename="D:/Documents/P20_small_wealth_multiples/aggregate/all.urb.rur.alpha.pdf",plot=all.urb.rur.alpha,width=8,height=30,units="in",limitsize=FALSE)
