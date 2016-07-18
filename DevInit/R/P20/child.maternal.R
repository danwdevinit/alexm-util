####Function and setup####
library(Hmisc)
library(plyr)
library(foreign)
library(data.table)
library(varhandle)

wd <- "D:/Documents/Data/MICSmeta/"
setwd(wd)

povcalcuts <- read.csv("headcounts.csv",as.is=TRUE)

povcalcuts$filename[which(povcalcuts$filename=="bfhr70dt")] <- "bfhr62dt"
povcalcuts$filename[which(povcalcuts$filename=="kehr7hdt")] <- "kehr70dt"
povcalcuts$filename[which(povcalcuts$filename=="mdhr6hdt")] <- "mdhr51dt"
povcalcuts$filename[which(povcalcuts$filename=="mwhr71dt")] <- "mwhr61dt"
povcalcuts$filename[which(povcalcuts$filename=="tzhr6adt")] <- "tzhr63dt"
povcalcuts$filename[which(povcalcuts$filename=="ughr72dt")] <- "ughr60dt"

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

wd <- "D:/Documents/Data/DHSauto/"
setwd(wd)

psum <- function(...,na.rm=TRUE) { 
  rowSums(do.call(cbind,list(...)),na.rm=na.rm)
} 

#Not necessarily most recent, but most recent with child/mother questions
latest_surveys <- c(
  "alhr50dt", "amhr61dt", "aohr61dt", "azhr52dt", "bdhr70dt", "bfhr62dt"
  ,"bjhr61dt", "bohr51dt", "buhr61dt", "cdhr61dt", "cghr60dt"
  ,"cihr61dt", "cmhr60dt", "cohr61dt", "drhr61dt", "eghr61dt"
  ,"ethr61dt", "gahr60dt", "ghhr70dt", "gmhr60dt", "gnhr61dt", "gyhr5idt"
  ,"hnhr62dt", "hthr61dt", "iahr52dt", "idhr63dt", "johr6cdt"
  ,"kehr70dt","khhr72dt", "kmhr61dt"
  #   , "kyhr61dt"
  , "lbhr6adt", "lshr71dt"
  #   ,"mbhr53dt"
  , "mdhr51dt", "mlhr6hdt", "mvhr51dt", "mwhr61dt"
  ,"mzhr62dt", "nghr6adt", "nihr61dt", "nmhr61dt"
  # , "nphr60dt"
  ,"pehr6idt","phhr61dt","pkhr61dt"
  ,"rwhr70dt","slhr61dt","snhr70dt", "sthr50dt"
  # , "szhr51dt"
  ,"tdhr70dt"
  ,"tghr61dt", "tjhr61dt", "tlhr61dt","tzhr63dt"
  # , "uahr51dt"
  ,"ughr60dt"
  # , "vnhr52dt"
  , "yehr61dt", "zmhr61dt"
  # , "zwhr62dt"
  #MICS
  ,"Afghanistan_MICS4_Datasets","Algeria_MICS4_Datasets"
  ,"Barbados_MICS4_Datasets","Belarus_MICS4_Datasets"
  ,"Belize_MICS4_Datasets","Bhutan_MICS4_Datasets"
  ,"Bosnia and Herzegovina_MICS4_Datasets","Central African Republic_MICS4_Datasets"
  ,"Costa Rica_MICS4_Datasets","Georgia MICS 2005 SPSS Datasets"
  ,"Guinea-Bissau MICS 2006 SPSS Datasets","Iraq_MICS4_Datasets","Jamaica_MICS4_Datasets"
  ,"Kazakhstan_MICS4_Datasets","Kosovo under UNSC res. 1244_MICS5_Datasets"
  ,"Kyrgyzstan MICS5 Datasets","Lao People's Democratic Republic_LSIS_Datasets"
  #   ,"Lebanon (Palestinians)_MICS4_Datasets"
  ,"Macedonia, The former Yugoslav Republic of_MICS4_Datasets","Mauritania_MICS4_Datasets"
  ,"Moldova_MICS4_Datasets","Mongolia_MICS5_Datasets","Montenegro_MICS5_Datasets"
  ,"Nepal_MICS5_Datasets"
  #   ,"Pakistan (Punjab)_MICS5_Datasets"
  ,"Serbia_MICS5_Datasets"
  #   ,"Somalia (Northeast Zone)_MICS4_Datasets"                                             
  #   ,"Somalia (Somaliland)_MICS4_Datasets"                                                 
  ,"Somalia MICS 2006 SPSS Datasets" 
  ,"South Sudan_MICS4_Datasets"
  ,"Sudan_MICS5_Datasets"
  ,"St.Lucia_MICS4_Datasets","State of Palestine_MICS5_Datasets","Suriname_MICS4_Datasets"
  ,"Swaziland_MICS4_Datasets","Syria MICS 2006 SPSS Datasets","Thailand_MICS4_Datasets"
  ,"Trinidad and Tobago MICS 2006 SPSS Datasets","Tunisia_MICS4_Datasets"
  ,"Turkmenistan_MICS3_Datasets","Ukraine_MICS4_Datasets","Uruguay_MICS4_Datasets"
  ,"Uzbekistan MICS 2006 SPSS Datasets","Vanuatu MICS 2007 SPSS Datasets","Viet Nam_MICS5_Datasets"
  ,"Zimbabwe_MICS5_Datasets"
)

# List out all the directories in our wd, this is where our data is contained
dirs <- list.dirs(wd,full.names=TRUE)

dataListwm <- list()
dataListch <- list()
dataIndex <- 1

# Loop through every dir
for(i in 2:length(dirs)){
  dir <- dirs[i]
  hrwd <- basename(dir)
  if(hrwd %in% latest_surveys){
    message(hrwd)
    
    hrBase <- basename(hrwd)
    iso2 <- toupper(substr(hrBase,1,2))
    phase <- substr(hrBase,5,6)
    
    hr <- read.csv(paste0(hrwd,"/",iso2,"HR",phase,"FL.csv")
                   ,na.strings="",as.is=TRUE,check.names=FALSE)
    
    names(hr)[which(names(hr)=="hv271")] <- "wealth"
    hr$wealth <- hr$wealth/100000
    names(hr)[which(names(hr)=="hv001")] <- "cluster"
    names(hr)[which(names(hr)=="hv002")] <- "household"
    names(hr)[which(names(hr)=="hv005")] <- "sample.weights"
    hr$weights <- hr$sample.weights/1000000
    povcalcut <- subset(povcalcuts,filename==hrBase)$hc
    povcalperc <- weighted.percentile(hr$wealth,hr$weights,prob=povcalcut)
    hr$p20 <- (hr$wealth < povcalperc)
    hrKeep <- c("cluster","household","wealth","weights","p20")
    hr <- hr[hrKeep]
    
    krwd <- paste0("D:/Documents/Data/DHSauto/",tolower(iso2),"kr",phase,"dt/")
    if(!file_test(op="-d", krwd)){message("KR WD invalid");next;}
    
    kr <- read.csv(paste0(krwd,iso2,"KR",phase,"FL.csv")
                   ,na.strings="",as.is=TRUE,check.names=FALSE)
    
    irwd <- paste0("D:/Documents/Data/DHSauto/",tolower(iso2),"ir",phase,"dt/")
    if(!file_test(op="-d", irwd)){message("IR WD invalid");}
    
    ir <- read.csv(paste0(irwd,iso2,"IR",phase,"FL.csv")
                   ,na.strings="",as.is=TRUE,check.names=FALSE)
    
    names(ir)[which(names(ir)=="v001")] <- "cluster"
    names(ir)[which(names(ir)=="v002")] <- "household"
    names(ir)[which(names(ir)=="v201")] <- "ceb"
    if(typeof(ir$v206)=="NULL" & typeof(ir$v207)=="NULL"){
      ir$cdead <- NA
    }else{
      ir$cdead <- psum(ir$v206,ir$v207,na.rm=TRUE) 
    }
    if(typeof(ir$m3a_1)=="NULL" & 
         typeof(ir$m3b_1)=="NULL" & 
         typeof(ir$m3c_1)=="NULL" & 
         typeof(ir$m3d_1)=="NULL" & 
         typeof(ir$m3e_1)=="NULL" & 
         typeof(ir$m3f_1)=="NULL"){
      ir$skilled.attendant <- NA
    }else{
      ir$skilled.attendant <- psum(
        (ir$m3a_1==1 | tolower(ir$m3a_1)=="yes")
        ,(ir$m3b_1==1 | tolower(ir$m3b_1)=="yes")
        ,(ir$m3c_1==1 | tolower(ir$m3c_1)=="yes")
        ,(ir$m3d_1==1 | tolower(ir$m3d_1)=="yes")
        ,(ir$m3e_1==1 | tolower(ir$m3e_1)=="yes")
        ,(ir$m3f_1==1 | tolower(ir$m3f_1)=="yes")
        ,na.rm=TRUE
      )>=1 
    }
    maternal.deaths <- function(df){
      maternal.deathsV <- c()
      for(i in 1:nrow(df)){
        maternal.deaths <- 0
        for(j in 1:20){
          num <- sprintf("%02d", j)
          pregVar <- paste0("mm9_",num)
          if(typeof(df[[pregVar]])!="NULL"){
            preg <- df[[pregVar]][i]
            if(!is.na(preg)){
              if(preg==2 |
                   preg==3 |
                   preg==5 |
                   preg==6 |
                   tolower(preg)=="died while pregnant" |
                   tolower(preg)=="died during delivery" |
                   tolower(preg)=="6 weeks after delivery" |
                   tolower(preg)=="2 months after delivery"
              ){
                maternal.deaths <- maternal.deaths + 1
              }
            }
          }else{
            next;
          }
        }
        maternal.deathsV <- c(maternal.deathsV,maternal.deaths)
      }
      return(maternal.deathsV)
    }
    ir$maternal.deaths <- maternal.deaths(ir)
    
    names(kr)[which(names(kr)=="v001")] <- "cluster"
    names(kr)[which(names(kr)=="v002")] <- "household"
    names(kr)[which(names(kr)=="b5")] <- "child.alive"
    names(kr)[which(names(kr)=="b7")] <- "age.at.death.months"
    if(typeof(kr$m3a)=="NULL" & 
         typeof(kr$m3b)=="NULL" & 
         typeof(kr$m3c)=="NULL" & 
         typeof(kr$m3d)=="NULL" & 
         typeof(kr$m3e)=="NULL" & 
         typeof(kr$m3f)=="NULL"){
      kr$skilled.attendant <- NA
    }else{
      kr$skilled.attendant <- psum(
        (kr$m3a==1 | tolower(kr$m3a)=="yes")
        ,(kr$m3b==1 | tolower(kr$m3b)=="yes")
        ,(kr$m3c==1 | tolower(kr$m3c)=="yes")
        ,(kr$m3d==1 | tolower(kr$m3d)=="yes")
        ,(kr$m3e==1 | tolower(kr$m3e)=="yes")
        ,(kr$m3f==1 | tolower(kr$m3f)=="yes")
        ,na.rm=TRUE
      )>=1  
    }
    
    vaccVars <- c("h2","h3","h4","h5","h6","h7","h8","h9","h10")
    
    recode.vacc.vars <- function(x){
      if(is.na(x)){
        return(NA)
      }
      if(is.factor(x)){
        str <- trimws(tolower(unfactor(x)))
      }else{
        str <- trimws(tolower(x))
      }
      
      if(is.na(str)){
        return(NA)
      }else if(str=="dk" | str==8 | str==9){
        return(NA)
      }else if(str=="0" | str=="no"){
        return(FALSE)
      }else{
        return(TRUE)
      }
    }
    
    code.any.vacc <- function(vacc.df){
      any.vaccs <- c()
      for(i in 1:nrow(vacc.df)){
        row <- vacc.df[i,]
        any.vacc <- FALSE
        na.length <- 0
        for(j in 1:8){
          vacc.var <- recode.vacc.vars(row[[j]])
          if(!is.na(vacc.var)){
            any.vacc <- any.vacc | vacc.var
          }else{
            na.length <- na.length + 1
          }
        }
        if(na.length==8){
          if(is.factor(row[[9]])){
            str <- tolower(trimws(unfactor(row[[9]])))
          }else{
            str <- tolower(trimws(row[[9]]))
          }
          if(is.na(str)){
           any.vacc <- NA 
          }else if(str=="don't know" | str==8 | str==9){
            any.vacc <- NA
          }else if(str=="no" | str==0){
            any.vacc <- FALSE
          }else if(str=="yes" | str==1){
            any.vacc <- TRUE
          }else{
            any.vacc <- NA
          }
        }
        any.vaccs <- c(any.vaccs,any.vacc)
      }
      return(any.vaccs)
    }
    
    krNames <- names(kr)
    namesDiff <- setdiff(vaccVars,krNames)
    if(length(namesDiff)>0){
      for(y in 1:length(namesDiff)){
        kr[namesDiff[y]] <- NA
        message(paste("Missing variable",namesDiff[y]))
      } 
    }
    
    kr$any.vacc <- code.any.vacc(kr[vaccVars])
    
    irKeep <- c(
      "cluster"
      ,"household"
      ,"ceb"
      ,"cdead"
      ,"skilled.attendant"
      ,"maternal.deaths"
      )
    irNames <- names(ir)
    namesDiff <- setdiff(irKeep,irNames)
    if(length(namesDiff)>0){
      for(y in 1:length(namesDiff)){
        kr[namesDiff[y]] <- NA
        message(paste("Missing variable",namesDiff[y]))
      } 
    }
    ir <- ir[irKeep]
    
    krKeep <- c(
      "cluster"
      ,"household"
      ,"skilled.attendant"
      ,"any.vacc"
    )
    krNames <- names(kr)
    namesDiff <- setdiff(krKeep,krNames)
    if(length(namesDiff)>0){
      for(y in 1:length(namesDiff)){
        kr[namesDiff[y]] <- NA
        message(paste("Missing variable",namesDiff[y]))
      } 
    }
    kr <- kr[krKeep]
    kr$filename <- hrBase
    ir$filename <- hrBase
    kr <- join(
      kr
      ,hr
      ,by=c("cluster","household")
      )
    ir <- join(
      ir
      ,hr
      ,by=c("cluster","household")
    )
    dataListch[[dataIndex]] <- kr
    dataListwm[[dataIndex]] <- ir
    dataIndex <- dataIndex + 1
  }
}


# set our working directory, change this if using on another machine
wd <- "D:/Documents/Data/MICSauto/"
setwd(wd)

varNames <- read.csv("D:/Documents/Data/MICSmeta/mics_child_vars.csv",as.is=TRUE,na.strings="")

# List out all the directories in our wd, this is where our data is contained
dirs <- list.dirs(wd,full.names=TRUE)

for(i in 2:length(dirs)){
  dir <- dirs[i]
  if(basename(dir) %in% latest_surveys){
    this.varName <- subset(varNames,filename==basename(dir))
    message(basename(dir))
    if(exists("hh")){rm(hh)}
    if(exists("wm")){rm(wm)}
    if(exists("ch")){rm(ch)}
    load(paste0(dir,"/","hh.RData"))
    load(paste0(dir,"/","wm.RData"))
    load(paste0(dir,"/","ch.RData"))
    names(hh) <- tolower(names(hh))
    names(wm) <- tolower(names(wm))
    names(ch) <- tolower(names(ch))
    ch.labs <- data.frame(names(ch)[1:length(ch)-1],attributes(ch)$variable.labels)
    names(ch.labs) <- c("name","description")
    wm.labs <- data.frame(names(wm)[1:length(wm)-1],attributes(wm)$variable.labels)
    names(wm.labs) <- c("name","description")
    hh <- data.frame(hh)
    ch <- data.frame(ch)
    wm <- data.frame(wm)
    
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
    names(hh)[which(names(hh)=="hh1")] <- "cluster"
    names(hh)[which(names(hh)=="hh2")] <- "household"
    povcalcut <- subset(povcalcuts,filename==basename(dir))$hc
    povcalperc <- weighted.percentile(hh$wealth,hh$weights,prob=povcalcut)
    
    hh$p20 <- (hh$wealth <= povcalperc)
    
    hhKeep <- c("cluster","household","wealth","weights","p20")
    hh <- hh[hhKeep]
    
    vaccVars <- subset(this.varName,match=="vacc")$var
    skilledBirthVars <- subset(this.varName,match=="attendant" &  skilled==1)$var
    unskilledBirthVars <- subset(this.varName,match=="attendant" &  skilled==0)$var
    mortality <- c("ceb","deadkids","cdead")
    names(ch)[which(names(ch)=="hh1")] <- "cluster"
    names(ch)[which(names(ch)=="hh2")] <- "household"
    names(ch)[which(names(ch)=="uf6")] <- "mother.line"
    vaccVars <- subset(this.varName,match=="vacc")$var
    names(ch)[which(names(ch)==vaccVars[1])] <- "any.vacc"
    
    
    names(wm)[which(names(wm)=="hh1")] <- "cluster"
    names(wm)[which(names(wm)=="hh2")] <- "household"
    names(wm)[which(names(wm)=="ln")] <- "mother.line"
    
    recode.birth.vars <- function(x,skilled){
      if(is.factor(x)){
        str <- trimws(tolower(unfactor(x)))
      }else{
        str <- trimws(tolower(x))
      }
      
      if(is.na(str)){
        return(NA)
      }else if(str=="" | str==9){
        return(NA)
      }else if(str=="missing"){
        return(FALSE)
      }else if(!skilled){
        return(FALSE)
      }else{
        return(TRUE)
      }
    }
    
    code.skilled.attendant <- function(attendant.df,skilled){
      skilled.attendants <- c()
      for(i in 1:nrow(attendant.df)){
        row <- attendant.df[i,]
        rowNames <- names(row)
        skilled.attendant <- FALSE
        na.length <- 0
        for(j in 1:length(row)){
          birth.var <- recode.birth.vars(row[[j]],rowNames[j] %in% skilled)
          if(!is.na(birth.var)){
            skilled.attendant <- skilled.attendant | birth.var
          }else{
            na.length <- na.length + 1
          }
        }
        if(na.length==length(row)){
          skilled.attendant <- NA
        }
        skilled.attendants <- c(skilled.attendants,skilled.attendant)
      }
      return(skilled.attendants)
    }
    wm$skilled.attendant <- code.skilled.attendant(wm[c(skilledBirthVars,unskilledBirthVars)],skilledBirthVars)
    
    recode.cdead <- function(cdead,deadkids){
      if(typeof(cdead)=="NULL"){
        return(deadkids)
      }else{
        return(cdead)
      }
    }
    wm$cdead <- recode.cdead(wm$cdead,wm$deadkids)
    wmKeep <- c(
      "cluster"
      ,"household"
      ,"mother.line"
      ,"skilled.attendant"
      ,"ceb"
      ,"cdead"
    )
    wmNames <- names(wm)
    namesDiff <- setdiff(wmKeep,wmNames)
    if(length(namesDiff)>0){
      for(y in 1:length(namesDiff)){
        wm[namesDiff[y]] <- NA
        message(paste("Missing variable",namesDiff[y]))
      } 
    }
    wm <- wm[wmKeep]

    chKeep <- c(
      "cluster"
      ,"household"
      ,"mother.line"
      ,"any.vacc"
    )
    chNames <- names(ch)
    namesDiff <- setdiff(chKeep,chNames)
    if(length(namesDiff)>0){
      for(y in 1:length(namesDiff)){
        ch[namesDiff[y]] <- NA
        message(paste("Missing variable",namesDiff[y]))
      } 
    }
    ch <- ch[chKeep]
    
    ch <- join(
      ch
      ,wm
      ,by=c("cluster","household","mother.line")
      )
    ch$filename <- basename(dir)
    wm$filename <- basename(dir)
    
    ch <- join(
      ch
      ,hh
      ,by=c("cluster","household")
    )
    wm <- join(
      wm
      ,hh
      ,by=c("cluster","household")
    )
    
    dataListwm[[dataIndex]] <- wm
    dataListch[[dataIndex]] <- ch
    dataIndex <- dataIndex + 1
  }
}

wd <- "D:/Documents/Data/MICSmeta"
setwd(wd)
maternal.health <- rbindlist(dataListwm,fill=TRUE)
child.health <- rbindlist(dataListch,fill=TRUE)

save(child.health,maternal.health,file="child.maternal.RData")
