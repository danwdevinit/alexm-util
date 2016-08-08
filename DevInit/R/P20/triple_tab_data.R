####Function and setup####
library(Hmisc)
library(plyr)
library(foreign)
library(data.table)
library(WDI)
library(varhandle)
require(zoo)

wd <- "D:/Documents/Data/MICSmeta/"
setwd(wd)

povcalcuts <- read.csv("headcounts.csv",as.is=TRUE)

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
povcalcuts <- join(povcalcuts,dat,by=c("iso3","year"))

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

####Run function####
# set our working directory, change this if using on another machine
wd <- "D:/Documents/Data/DHSauto/"
setwd(wd)

# List out all the directories in our wd, this is where our data is contained
dirs <- list.dirs(wd,full.names=TRUE)

dataList <- list()
dataIndex <- 1

# Loop through every dir
for(i in 2:length(dirs)){
  dir <- dirs[i]
  # Pull some coded info out of the dir name
  country <- tolower(substr(basename(dir),1,2))
  recode <- tolower(substr(basename(dir),3,4))
  phase <- as.integer(substr(basename(dir),5,5))
  # For this analysis, we're only interested in individual member recodes, or "hr"
  if(basename(dir) %in% povcalcuts$filename){
    message(basename(dir))
    hrwd <- dir
    if(!file_test(op="-d", hrwd)){next;}
    
    hrBase <- basename(hrwd)
    iso2 <- toupper(substr(hrBase,1,2))
    phase <- substr(hrBase,5,6)
    
    prwd <- paste0("D:/Documents/Data/DHSauto/",tolower(iso2),"pr",phase,"dt/")
    if(!file_test(op="-d", prwd)){next;}
    
    pr <- read.csv(paste0(prwd,iso2,"PR",phase,"FL.csv")
                   ,na.strings="",as.is=TRUE,check.names=FALSE)
    
    names(pr)[which(names(pr)=="hv271")] <- "wealth"
    pr$wealth <- pr$wealth/100000
    
    #Rename sample.weights var
    names(pr)[which(names(pr)=="hv005")] <- "sample.weights"
    pr$weights <- pr$sample.weights/1000000
    
    #Rename urban var
    names(pr)[which(names(pr)=="hv025")] <- "urban.rural"
    
    #Rename educ var
    names(pr)[which(names(pr)=="hv109")] <- "educ"
    recode.educ <- function(x){
      if(is.na(x)){return(NA)}
      else if(tolower(x)=="dk" | tolower(x)=="don't know" | tolower(x)=="missing" | x==8 | x==9){return(NA)}
      else if(x==0 | x==1 | tolower(x)=="no education, preschool" | tolower(x)=="no education" | tolower(x)=="incomplete primary"){return("No education, preschool")}
      else if(x==2 | x==3 | tolower(x)=="complete primary" | tolower(x)=="incomplete secondary"){return("Primary")}
      else if(x==4 | tolower(x)=="complete secondary"){return("Secondary")}
      else if(x==5 | tolower(x)=="higher"){return("Higher")}
      else{return(NA)}
    }
    pr$educ <- sapply(pr$educ,recode.educ)
    
    #Rename age var
    names(pr)[which(names(pr)=="hv105")] <- "age"
    
    #Rename sex var
    names(pr)[which(names(pr)=="hv104")] <- "sex"
    
    #Rename cluster/hh var
    names(pr)[which(names(pr)=="hv001")] <- "cluster"
    names(pr)[which(names(pr)=="hv002")] <- "household"
    names(pr)[which(names(pr)=="hvidx")] <- "line"
    names(pr)[which(names(pr)=="hv112")] <- "mother.line"
    pr$mother.line[which(pr$mother.line==99)] <- NA
    
    #Head vars
    names(pr)[which(names(pr)=="hv219")] <- "head.sex"
    names(pr)[which(names(pr)=="hv220")] <- "head.age"
    
    #reg?
    names(pr)[which(names(pr)=="hv140")] <- "birth.cert"
    
    #nutrition
    names(pr)[which(names(pr)=="ha40")] <- "woman.bmi"
    if(typeof(pr$woman.bmi)!="NULL"){
      pr$woman.bmi <- pr$woman.bmi/100 
    }else{
      pr$woman.bmi <- NA
    }
    names(pr)[which(names(pr)=="hb40")] <- "man.bmi"
    if(typeof(pr$man.bmi)!="NULL"){
      pr$man.bmi <- pr$man.bmi/100 
    }else{
      pr$man.bmi <- NA
    }
    names(pr)[which(names(pr)=="hc1")] <- "age.months"
    names(pr)[which(names(pr)=="hc2")] <- "weight.kg"
    names(pr)[which(names(pr)=="hc3")] <- "height.cm"
    names(pr)[which(names(pr)=="hc15")] <- "standing.lying"
    names(pr)[which(names(pr)=="hc5")] <- "child.height.age"
    if(typeof(pr$child.height.age)=="NULL"){
      pr$child.height.age <- NA
    }else{
      pr$child.height.age <- pr$child.height.age/100
    }
    pr$child.weights <- pr$weights
    
    povcalcut <- subset(povcalcuts,filename==hrBase)$hc
    np20cut <- 0.2
    nplcut <- subset(povcalcuts,filename==hrBase)$pl.hc
    cuts <- c(povcalcut,np20cut,nplcut)
    povperc <- weighted.percentile(pr$wealth,pr$weights,prob=cuts)
    
    pr$p20 <- (pr$wealth < povperc[1])
    pr$np20 <- (pr$wealth < povperc[2])
    pr$npl <- (pr$wealth < povperc[3])
    
    mothers <- unique(pr[c("cluster","household","line","woman.bmi")])
    mothers <- mothers[complete.cases(mothers),]
    names(mothers) <- c("cluster","household","mother.line","mother.bmi")
    pr <- join(
      pr
      ,mothers
      ,by=c("cluster","household","mother.line")
    )
    
    keep <- c("wealth","weights","urban.rural","educ","age","sex","cluster","household","head.sex","head.age","p20"
              ,"birth.cert","birth.reg","age.months","weight.kg","height.cm","standing.lying","child.height.age"
              ,"woman.bmi","man.bmi","child.weights","mother.bmi","np20","npl"
    )
    prNames <- names(pr)
    namesDiff <- setdiff(keep,prNames)
    if(length(namesDiff)>0){
      for(y in 1:length(namesDiff)){
        pr[namesDiff[y]] <- NA
        message(paste("Missing variable",namesDiff[y]))
      } 
    }
    data <- pr[keep]
    data$filename <- hrBase
    dataList[[dataIndex]] <- data
    dataIndex <- dataIndex + 1
  }
}

setwd("D:/Documents/Data/MICSmeta")
varNames <- read.csv("mics_meta_vars_complete.csv",as.is=TRUE,na.strings="")
classes <- read.csv("global_mics_classes.csv",as.is=TRUE,na.strings="NAN")

wd <- "D:/Documents/Data/MICSauto/"
setwd(wd)

dataList <- list()
dataIndex <- 1

# List out all the directories in our wd, this is where our data is contained
dirs <- list.dirs(wd,full.names=TRUE)

# dir <- "D:/Documents/Data/MICSauto/Somalia MICS 2006 SPSS Datasets"
# dir <- "D:/Documents/Data/MICSauto/Algeria_MICS4_Datasets"

for(i in 2:length(dirs)){
  dir <- dirs[i]
  hrBase <- basename(dir)
  if(hrBase %in% povcalcuts$filename){
    
    message(hrBase) 
    if(exists("hh")){rm(hh)}
    if(exists("hl")){rm(hl)}
    if(exists("ch")){rm(ch)}
    if(exists("wm")){rm(wm)}
    load(paste0(dir,"/","hh.RData"))
    load(paste0(dir,"/","hl.RData"))
    load(paste0(dir,"/","ch.RData"))
    load(paste0(dir,"/","wm.RData"))
    hh <- data.frame(hh,as.is=TRUE,check.names=FALSE)
    hl <- data.frame(hl,as.is=TRUE,check.names=FALSE)
    ch <- data.frame(ch,as.is=TRUE,check.names=FALSE)
    wm <- data.frame(wm,as.is=TRUE,check.names=FALSE)
    names(hh) <- tolower(names(hh))
    names(hl) <- tolower(names(hl))
    names(ch) <- tolower(names(ch))
    names(wm) <- tolower(names(wm))
    
    file.varName <- subset(varNames,filename==hrBase)
    
    attendedVar <- subset(file.varName,match=="attended")$varName
    gradeVar <- subset(file.varName,match=="grade")$varName
    schoolVar <- subset(file.varName,match=="school")$varName
    
    ynm.classes <- subset(classes,filename==hrBase & type=="ynm")
    attended.classes <- subset(classes,filename==hrBase & type=="attended")
    urban.rural.classes <- subset(classes,filename==hrBase & type=="urban.rural")
    school.classes <- subset(classes,filename==hrBase & type=="school")
    
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
    
    #Rename educ var
    names(hl)[which(names(hl)==attendedVar)] <- "attended"
    names(hl)[which(names(hl)==schoolVar)] <- "school"
    names(hl)[which(names(hl)==gradeVar)] <- "grade"
    
    #Rename age var
    names(hl)[which(names(hl)=="hl6")] <- "age"
    
    #Rename sex var
    names(hl)[which(names(hl)=="hl4")] <- "sex"
    
    #Rename head var
    hl$head <- tolower(substr(hl$hl3,1,4)) %in% c("chef","head")
    
    #Rename child vars
    names(ch)[which(names(ch)=="br1")] <- "birth.cert"
    names(ch)[which(names(ch)=="br2")] <- "birth.reg"
    names(ch)[which(names(ch)=="cage")] <- "age.months"
    names(ch)[which(names(ch)=="chweight")] <- "child.weights"
    names(ch)[which(names(ch)=="an3")] <- "weight.kg"
    names(ch)[which(names(ch)=="an4a")] <- "standing.lying"
    names(ch)[which(names(ch)=="haz2")] <- "child.height.age"
    
    #code female bmi
    if(typeof(wm$anw4)!="NULL" & typeof(wm$anw5)!="NULL"){
      wm$anw4[which(wm$anw4==99.9)] <- NA
      wm$anw5[which(wm$anw5==999.9)] <- NA
      wm$anw5 <- wm$anw5/100
      wm$woman.bmi <- wm$anw4/(wm$anw5*wm$anw5) 
    }
    
    #Rename cluster/hh var
    names(hl)[which(names(hl)=="hh1")] <- "cluster"
    names(hl)[which(names(hl)=="hh2")] <- "household"
    names(hl)[which(names(hl)=="hl1")] <- "line"
    names(hl)[which(names(hl)=="ln")] <- "line"
    names(hh)[which(names(hh)=="hh1")] <- "cluster"
    names(hh)[which(names(hh)=="hh2")] <- "household"
    names(ch)[which(names(ch)=="hh1")] <- "cluster"
    names(ch)[which(names(ch)=="hh2")] <- "household"
    names(ch)[which(names(ch)=="ln")] <- "line"
    names(ch)[which(names(ch)=="uf6")] <- "mother.line"
    names(wm)[which(names(wm)=="hh1")] <- "cluster"
    names(wm)[which(names(wm)=="hh2")] <- "household"
    names(wm)[which(names(wm)=="ln")] <- "line"
    
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
              if(grade>=5 & grade<7){
                educ <- 1
              }else if(grade>=7 & grade<9){
                educ <- 2
              }else if(grade>9){
                educ <- 3
              }else{
                educ <- 0
              }
            }
          }else{
            #missing attended, but not level
            if(is.na(grade)){
              #has level, but not grade
              if(school %in% preschool.level | school %in% none.level){
                educ <- 0
              }else if(school %in% primary.level){
                educ <- 1
              }else if(school %in% secondary.level){
                educ <- 2
              }else if(school %in% higher.level){
                educ <- 3
              }else{
                educ <- NA
              }
            }else{
              #missing attended and level, but not grade
              if(grade>=5 & grade<7){
                educ <- 1
              }else if(grade>=7 & grade<9){
                educ <- 2
              }else if(grade>9){
                educ <- 3
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
              if(grade>=5 & grade<7){
                educ <- 1
              }else if(grade>=7 & grade<9){
                educ <- 2
              }else if(grade>9){
                educ <- 3
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
              educ <- 0
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
            educ <- 2
          } else if(school %in% higher.level){
            #(in)complete higher
            educ <- 3
          }else if(grade>=5 & grade<7){
            educ <- 1
          }else if(grade>=7 & grade<9){
            educ <- 2
          }else if(grade>9){
            educ <- 3
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
    
    hl$educ <- recode.educ(hl$attended,hl$school,hl$grade)
    
    head <- subset(hl,head==1)
    names(head)[which(names(head)=="sex")] <- "head.sex"
    names(head)[which(names(head)=="age")] <- "head.age"
    keep <- c("cluster","household","head.sex","head.age")
    head <- head[keep]
    hh <- join(
      hh
      ,head
      ,by=c("cluster","household")
    )
    
    recode.urban.rural <- function(x){
      item <- subset(urban.rural.classes,value==tolower(x))
      if(nrow(item)==0){return(NA)}
      else{item$urban[1]}
    }
    hh$urban.rural <- sapply(hh$urban.rural,recode.urban.rural)
    
    povcalcut <- subset(povcalcuts,filename==hrBase)$hc
    np20cut <- 0.2
    nplcut <- subset(povcalcuts,filename==hrBase)$pl.hc
    cuts <- c(povcalcut,np20cut,nplcut)
    povperc <- weighted.percentile(hh$wealth,hh$weights,prob=cuts)
    
    hh$p20 <- (hh$wealth < povperc[1])
    hh$np20 <- (hh$wealth < povperc[2])
    hh$npl <- (hh$wealth < povperc[3])
    
    wmkeep <- c("household","cluster","line","woman.bmi")
    wmNames <- names(wm)
    namesDiff <- setdiff(wmkeep,wmNames)
    if(length(namesDiff)>0){
      for(y in 1:length(namesDiff)){
        wm[namesDiff[y]] <- NA
        message(paste("Missing variable",namesDiff[y]))
      } 
    }
    wm <- wm[wmkeep]
    
    hl <- join(
      hl
      ,wm
      ,by=c("cluster","household","line")
    )
    
    names(wm) <- c("household","cluster","mother.line","mother.bmi")
    
    ch <- join(
      ch
      ,wm
      ,by=c("cluster","household","mother.line")
    )
    
    chkeep <- c("household","cluster","line","birth.cert","birth.reg","age.months","child.weights","weight.kg","standing.lying"
                ,"child.height.age","mother.bmi")
    chNames <- names(ch)
    namesDiff <- setdiff(chkeep,chNames)
    if(length(namesDiff)>0){
      for(y in 1:length(namesDiff)){
        ch[namesDiff[y]] <- NA
        message(paste("Missing variable",namesDiff[y]))
      } 
    }
    ch <- ch[chkeep]
    
    hl <- join(
      hl
      ,ch
      ,by=c("cluster","household","line")
    )
    
    
    hhkeep <- c("wealth","weights","urban.rural","cluster","household","head.sex","head.age","p20","np20","npl")
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
    keep <- c("wealth","weights","urban.rural","educ","age","sex","cluster","household","head.sex","head.age","p20"
              ,"birth.cert","birth.reg","age.months","weight.kg","height.cm","standing.lying","child.height.age"
              ,"woman.bmi","man.bmi","child.weights","mother.bmi","np20","npl"
    )
    hlNames <- names(hl)
    namesDiff <- setdiff(keep,hlNames)
    if(length(namesDiff)>0){
      for(y in 1:length(namesDiff)){
        hl[namesDiff[y]] <- NA
        message(paste("Missing variable",namesDiff[y]))
      } 
    }
    hl <- hl[keep]
    hl$filename <- hrBase
    dataList[[dataIndex]] <- hl
    dataIndex <- dataIndex + 1
  }
}

wd <- "D:/Documents/Data/MICSmeta"
setwd(wd)
micsMetaData <- rbindlist(dataList)

data.total <- rbind(metaData,micsMetaData)

recode.urban <- function(x){
  if(is.na(x)){return(NA)}
  else if(x==0 | tolower(x)=="rural"){return(0)}
  else if(x==1 | tolower(x)=="urban"){return(1)}
  else{return(NA)}
}
data.total$urban <- sapply(data.total$urban.rural,recode.urban)

recode.educ <- function(x){
  if(is.na(x)){return(NA)}
  else if(tolower(x)=="dk" | tolower(x)=="don't know"){return(NA)}
  else if(x==0 | tolower(x)=="no education, preschool"){return("No education, preschool")}
  else if(x==1 | tolower(x)=="primary"){return("Primary")}
  else if(x==2 | tolower(x)=="secondary"){return("Secondary")}
  else if(x==3 | tolower(x)=="higher"){return("Higher")}
  else{return(NA)}
}
data.total$educ <- sapply(data.total$educ,recode.educ)
data.total$educ <- factor(data.total$educ
                          ,levels = c("No education, preschool","Primary","Secondary","Higher")
)

codeAgeCat <- function(x){
  startAge <- 0
  ageDiff <- 4
  endAge <- 4
  if(is.na(x)){
    return("missing")
  }
  while(startAge<95){
    endAge <- startAge+ageDiff
    if(x>=startAge & x<=endAge){
      return(
        paste0(startAge,"-",endAge)  
      )
    }
    startAge <- endAge + 1
  }
  if(x>=95){
    return("95+")
  }
  return("missing")
}

data.total$ageCategory <- vapply(data.total$age,codeAgeCat,character(1))
data.total$ageCategory <- factor(data.total$ageCategory,
                                 levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34"
                                            ,"35-39","40-44","45-49","50-54","55-59","60-64"
                                            ,"65-69","70-74","75-79","80-84","85-89","90-94"
                                            ,"95+","missing")                          
)

data.total$head.ageCategory <- vapply(data.total$head.age,codeAgeCat,character(1))
data.total$head.ageCategory <- factor(data.total$head.ageCategory,
                                      levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34"
                                                 ,"35-39","40-44","45-49","50-54","55-59","60-64"
                                                 ,"65-69","70-74","75-79","80-84","85-89","90-94"
                                                 ,"95+","missing")                          
)

sex.missing = c(NA,"missing",9)
sex.male = c(1,"male","masculin","hombre")
sex.female = c(2, "female","feminin","mujer")
data.total$sex[which(tolower(data.total$sex) %in% sex.missing)] <- NA
data.total$sex[which(tolower(data.total$sex) %in% sex.male)] <- "Male"
data.total$sex[which(tolower(data.total$sex) %in% sex.female)] <- "Female"
data.total$head.sex[which(tolower(data.total$head.sex) %in% sex.missing)] <- NA
data.total$head.sex[which(tolower(data.total$head.sex) %in% sex.male)] <- "Male"
data.total$head.sex[which(tolower(data.total$head.sex) %in% sex.female)] <- "Female"

#0 - neither certificate or registered
#1 - has certificate
#2 - registered
#8 - dk
birth.cert.missing <- c(NA,"dk","don't know",8,9,"missing","nsp","manquant","no sabe")
birth.cert.no <- c("registered",0,2,"neither certificate or registered","no","non","has only hospital card")
birth.cert.yes <- setdiff(unique(tolower(data.total$birth.cert)),c(birth.cert.no,birth.cert.missing))

birth.reg.missing <- c(NA,"dk","missing","nsp","manquant")
birth.reg.no <- c("no","non")
birth.reg.yes <- c("yes","oui","sí")
#count registrations if birth.cert var reveals it to be so
birth.cert.registered <- c(2,"registered","has only hospital card",birth.cert.yes)
birth.cert.not.registered <- c(0,"neither certificate or registered","no","non")
data.total$birth.reg.coded <- unfactor(data.total$birth.reg)
data.total$birth.reg.coded[which(is.na(data.total$birth.reg.coded) & tolower(data.total$birth.cert) %in% birth.cert.registered)] <- "Yes"
data.total$birth.reg.coded[which(is.na(data.total$birth.reg.coded) & tolower(data.total$birth.cert) %in% birth.cert.not.registered)] <- "No"
data.total$birth.reg.coded[which(is.na(data.total$birth.reg.coded) & grepl("visto",data.total$birth.cert))] <- "Yes"

data.total$birth.reg.coded[which(tolower(data.total$birth.reg.coded) %in% birth.reg.missing)] <- NA
data.total$birth.reg.coded[which(tolower(data.total$birth.reg.coded) %in% birth.reg.no)] <- 0
data.total$birth.reg.coded[which(tolower(data.total$birth.reg.coded) %in% birth.reg.yes)] <- 1
data.total$birth.reg.coded[which(substr(data.total$birth.reg.coded,1,1)=="S")] <- 1

data.total$birth.reg <- data.total$birth.reg.coded

data.total$birth.cert <- unfactor(data.total$birth.cert)
data.total$birth.cert[which(tolower(data.total$birth.cert) %in% birth.cert.missing)] <- NA
data.total$birth.cert[which(tolower(data.total$birth.cert) %in% birth.cert.no)] <- 0
data.total$birth.cert[which(tolower(data.total$birth.cert) %in% birth.cert.yes)] <- 1
data.total$birth.cert[which(grepl("visto",data.total$birth.cert))] <- 1

data.total$woman.bmi[which(data.total$woman.bmi>80)] <- NA
data.total$woman.bmi.class <- NA
data.total$woman.bmi.class[which(data.total$woman.bmi<16)] <- "Severe thinness"
data.total$woman.bmi.class[which(data.total$woman.bmi>=16 & data.total$woman.bmi<17)] <- "Moderate thinness"
data.total$woman.bmi.class[which(data.total$woman.bmi>=17 & data.total$woman.bmi<18.5)] <- "Mild thinness"
data.total$woman.bmi.class[which(data.total$woman.bmi>=18.5 & data.total$woman.bmi<25)] <- "Normal range"
data.total$woman.bmi.class[which(data.total$woman.bmi>=25 & data.total$woman.bmi<30)] <- "Pre-obese"
data.total$woman.bmi.class[which(data.total$woman.bmi>=30 & data.total$woman.bmi<35)] <- "Obese class I"
data.total$woman.bmi.class[which(data.total$woman.bmi>=35 & data.total$woman.bmi<40)] <- "Obese class II"
data.total$woman.bmi.class[which(data.total$woman.bmi>=40)] <- "Obese class III"

data.total$woman.bmi.class <- factor(data.total$woman.bmi.class
                                     ,levels=c(
                                       "Severe thinness"
                                       ,"Moderate thinness"
                                       ,"Mild thinness"
                                       ,"Normal range"
                                       ,"Pre-obese"
                                       ,"Obese class I"
                                       ,"Obese class II"
                                       ,"Obese class III"
                                     ))

data.total$man.bmi[which(data.total$man.bmi>80)] <- NA
data.total$man.bmi.class <- NA
data.total$man.bmi.class[which(data.total$man.bmi<16)] <- "Severe thinness"
data.total$man.bmi.class[which(data.total$man.bmi>=16 & data.total$man.bmi<17)] <- "Moderate thinness"
data.total$man.bmi.class[which(data.total$man.bmi>=17 & data.total$man.bmi<18.5)] <- "Mild thinness"
data.total$man.bmi.class[which(data.total$man.bmi>=18.5 & data.total$man.bmi<25)] <- "Normal range"
data.total$man.bmi.class[which(data.total$man.bmi>=25 & data.total$man.bmi<30)] <- "Pre-obese"
data.total$man.bmi.class[which(data.total$man.bmi>=30 & data.total$man.bmi<35)] <- "Obese class I"
data.total$man.bmi.class[which(data.total$man.bmi>=35 & data.total$man.bmi<40)] <- "Obese class II"
data.total$man.bmi.class[which(data.total$man.bmi>=40)] <- "Obese class III"

data.total$man.bmi.class <- factor(data.total$man.bmi.class
                                   ,levels=c(
                                     "Severe thinness"
                                     ,"Moderate thinness"
                                     ,"Mild thinness"
                                     ,"Normal range"
                                     ,"Pre-obese"
                                     ,"Obese class I"
                                     ,"Obese class II"
                                     ,"Obese class III"
                                   ))

data.total$mother.bmi[which(data.total$mother.bmi>80)] <- NA
data.total$mother.bmi.class <- NA
data.total$mother.bmi.class[which(data.total$mother.bmi<16)] <- "Severe thinness"
data.total$mother.bmi.class[which(data.total$mother.bmi>=16 & data.total$mother.bmi<17)] <- "Moderate thinness"
data.total$mother.bmi.class[which(data.total$mother.bmi>=17 & data.total$mother.bmi<18.5)] <- "Mild thinness"
data.total$mother.bmi.class[which(data.total$mother.bmi>=18.5 & data.total$mother.bmi<25)] <- "Normal range"
data.total$mother.bmi.class[which(data.total$mother.bmi>=25 & data.total$mother.bmi<30)] <- "Pre-obese"
data.total$mother.bmi.class[which(data.total$mother.bmi>=30 & data.total$mother.bmi<35)] <- "Obese class I"
data.total$mother.bmi.class[which(data.total$mother.bmi>=35 & data.total$mother.bmi<40)] <- "Obese class II"
data.total$mother.bmi.class[which(data.total$mother.bmi>=40)] <- "Obese class III"

data.total$mother.bmi.class <- factor(data.total$mother.bmi.class
                                      ,levels=c(
                                        "Severe thinness"
                                        ,"Moderate thinness"
                                        ,"Mild thinness"
                                        ,"Normal range"
                                        ,"Pre-obese"
                                        ,"Obese class I"
                                        ,"Obese class II"
                                        ,"Obese class III"
                                      ))

data.total$child.height.age[which(data.total$child.height.age>80)] <- NA
data.total$stunting <- NA
# data.total$stunting[which(data.total$child.height.age<= (-6))] <- "Implausibly low"
data.total$stunting[which(data.total$child.height.age > (-6) & data.total$child.height.age<= (-3))] <- "Severely stunted"
data.total$stunting[which(data.total$child.height.age > (-3) & data.total$child.height.age<= (-2))] <- "Stunted, but not severely"
data.total$stunting[which(data.total$child.height.age > (-2) & data.total$child.height.age< (6))] <- "Not stunted"
# data.total$stunting[which(data.total$child.height.age>= (6))] <- "Implausibly high"

data.total$stunting <- factor(data.total$stunting
                              ,levels=c(
                                "Implausibly low"
                                ,"Severely stunted"
                                ,"Stunted, but not severely"
                                ,"Not stunted"
                                ,"Implausibly high"
                              ))

wd <- "D:/Documents/Data/BrazilSurvey/spss"
setwd(wd)

load("PNDS2006_BR_DOM_PESS.RData")
pr <- data.frame(dat,as.is=TRUE,check.names=FALSE)
load("PNDS2006_BR_FILHOS.RData")
ch <- data.frame(dat,as.is=TRUE,check.names=FALSE)

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
pr$urban[which(pr$urban.rural=="Urbano")] <- 1
pr$urban[which(pr$urban.rural=="Rural")] <- 0

names(pr)[which(names(pr)=="XP999_PESO")] <- "weights"
pr$weights <- pr$weights/10000

names(pr)[which(names(pr)=="P004_SEXO")] <- "sex"
pr$gender <- NA
pr$gender[which(pr$sex=="Masculino")] <- "Male"
pr$gender[which(pr$sex=="Feminino")] <- "Female"
pr$sex <- pr$gender
pr$gender <- NULL
names(pr)[which(names(pr)=="XP010_MELH")] <- "age"
names(pr)[which(names(pr)=="P011B_GRAU")] <- "educ"

names(pr)[which(names(pr)=="P003_PARE")] <- "head"
head <- subset(pr,head=="Responsável")
names(head)[which(names(head)=="age")] <- "head.age"
names(head)[which(names(head)=="sex")] <- "head.sex"
keep <- c("household","head.age","head.sex")
head <- head[keep]

pr <- join(
  pr
  ,head
  ,by="household"
)

povcalcut <- subset(povcalcuts,filename=="Brazil")$hc
np20cut <- 0.2
nplcut <- subset(povcalcuts,filename=="Brazil")$pl.hc
cuts <- c(povcalcut,np20cut,nplcut)
povperc <- weighted.percentile(pr$wealth,pr$weights,prob=cuts)

pr$p20 <- (pr$wealth < povperc[1])
pr$np20 <- (pr$wealth < povperc[2])
pr$npl <- (pr$wealth < povperc[3])

codeAgeCat <- function(x){
  startAge <- 0
  ageDiff <- 4
  endAge <- 4
  if(is.na(x)){
    return("missing")
  }
  while(startAge<95){
    endAge <- startAge+ageDiff
    if(x>=startAge & x<=endAge){
      return(
        paste0(startAge,"-",endAge)  
      )
    }
    startAge <- endAge + 1
  }
  if(x>=95){
    return("95+")
  }
  return("missing")
}

pr$ageCategory <- vapply(pr$age,codeAgeCat,character(1))
pr$ageCategory <- factor(pr$ageCategory,
                         levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34"
                                    ,"35-39","40-44","45-49","50-54","55-59","60-64"
                                    ,"65-69","70-74","75-79","80-84","85-89","90-94"
                                    ,"95+","missing")                          
)

pr$head.ageCategory <- vapply(pr$head.age,codeAgeCat,character(1))
pr$head.ageCategory <- factor(pr$head.ageCategory,
                              levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34"
                                         ,"35-39","40-44","45-49","50-54","55-59","60-64"
                                         ,"65-69","70-74","75-79","80-84","85-89","90-94"
                                         ,"95+","missing")                          
)

names(ch)[which(names(ch)=="M241_LINH")] <- "ch.line"
names(ch)[which(names(ch)=="M241Z_NQUE")] <- "line"
names(ch)[which(names(ch)=="MULHER_ID")] <- "mother.id"
ch$household <- substr(ch$mother.id,1,nchar(ch$mother.id)-2)
names(ch)[which(names(ch)=="CM002_CONG")] <- "cluster"
names(ch)[which(names(ch)=="XF110_PESO")] <- "weight.kg"
ch$weight.kg[which(ch$weight.kg>=998)] <- NA
names(ch)[which(names(ch)=="XF120_ALTU")] <- "height.cm"
ch$height.cm[which(ch$height.cm>=998)] <- NA
names(ch)[which(names(ch)=="XF100_MESE")] <- "age.months"
ch$height.cm[which(ch$height.cm>=998)] <- NA
names(ch)[which(names(ch)=="M847_DEIT")] <- "standing.lying"
names(ch)[which(names(ch)=="XF310_INDI")] <- "child.height.age"
names(ch)[which(names(ch)=="XF320_INDI")] <- "child.weight.age"

chKeep <- c("line","cluster","household","weight.kg"
            ,"height.cm","age.months","standing.lying"
            ,"child.height.age","child.weight.age")
ch <- ch[chKeep]

pr <- cbind(pr,ch[match(pr$line,ch$line),])

pr$stunting <- NA
# pr$stunting[which(pr$child.height.age<= (-6))] <- "Implausibly low"
pr$stunting[which(pr$child.height.age > (-6) & pr$child.height.age<= (-3))] <- "Severely stunted"
pr$stunting[which(pr$child.height.age > (-3) & pr$child.height.age<= (-2))] <- "Stunted, but not severely"
pr$stunting[which(pr$child.height.age > (-2) & pr$child.height.age< (6))] <- "Not stunted"
# pr$stunting[which(pr$child.height.age>= (6))] <- "Implausibly high"

pr$stunting <- factor(pr$stunting
                      ,levels=c(
                        "Implausibly low"
                        ,"Severely stunted"
                        ,"Stunted, but not severely"
                        ,"Not stunted"
                        ,"Implausibly high"
                      ))

recode.educ <- function(x){
  if(is.na(x)){return(NA)}
  else if(x=="Sem resposta" | x=="Não sabe"){return(NA)}
  else if(x=="Nenhum" | x=="Creche (não seriado)" | x=="Pré-escola (não seriado)"){return("No education, preschool")}
  else if(x=="CA / Alfab adultos (não seriado)" | x=="EJA (não seriado)" | x=="Ensino fundamental (seriado)" | x=="Crianças especiais"){return("Primary")}
  else if(x=="Supletivo ensino fundamental" | x=="Ensino médio"){return("Secondary")}
  else{return("Higher")}
}
pr$educ <- sapply(pr$educ,recode.educ)
pr$educ <- factor(pr$educ
                  ,levels = c("No education, preschool","Primary","Secondary","Higher")
)

keep <- c("wealth","weights","urban.rural","urban","educ","age","sex","cluster","household","head.sex","head.age","p20"
          ,"birth.cert","birth.reg","age.months","weight.kg","height.cm","standing.lying","child.height.age","child.weight.age"
          ,"woman.bmi","man.bmi","ageCategory","head.ageCategory","stunting","npl","np20"
)
prNames <- names(pr)
namesDiff <- setdiff(keep,prNames)
if(length(namesDiff)>0){
  for(y in 1:length(namesDiff)){
    pr[namesDiff[y]] <- NA
    message(paste("Missing variable",namesDiff[y]))
  } 
}
data <- pr[keep]
data$filename <- "Brazil"

brazil.data.total <- data
data.total <- rbind(brazil.data.total,data.total)

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

#Registration
names(ir)[which(names(ir)=="qa301_a12_p")] <- "birth.reg.raw"
registered <- c("Agricultural","non-agricultural","Non-Chinese nationality")
nonregistered <- c("NA","Unknown","No registration")
ir$birth.reg <- NA
ir$birth.reg[which(ir$birth.reg.raw %in% registered)] <- 1
ir$birth.reg[which(ir$birth.reg.raw %in% nonregistered)] <- 0

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
zscores$standing.lying <- NA
zscoreKeep <- c("cluster","household","pid","weight.kg","height.cm","age.months","standing.lying","zlen","zwei")
zscores <- zscores[zscoreKeep]
names(zscores)[which(names(zscores)=="zlen")] <- "child.height.age"
names(zscores)[which(names(zscores)=="zwei")] <- "child.weight.age"

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
np20cut <- 0.2
nplcut <- subset(povcalcuts,filename=="China")$pl.hc
cuts <- c(povcalcut,np20cut,nplcut)
povperc <- weighted.percentile(ir$wealth,ir$weights,prob=cuts)

ir$p20 <- (ir$wealth < povperc[1])
ir$np20 <- (ir$wealth < povperc[2])
ir$npl <- (ir$wealth < povperc[3])

ir <- join(
  ir
  ,zscores
  ,by=c("cluster","household","pid")
)

keep <- c("wealth","weights","urban","educ","age","sex","cluster","household","head.sex","head.age","p20"
          ,"birth.cert","birth.reg","age.months","weight.kg","height.cm","standing.lying","child.height.age"
          ,"woman.bmi","man.bmi","np20","npl"
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

codeAgeCat <- function(x){
  startAge <- 0
  ageDiff <- 4
  endAge <- 4
  if(is.na(x)){
    return("missing")
  }
  while(startAge<95){
    endAge <- startAge+ageDiff
    if(x>=startAge & x<=endAge){
      return(
        paste0(startAge,"-",endAge)  
      )
    }
    startAge <- endAge + 1
  }
  if(x>=95){
    return("95+")
  }
  return("missing")
}

ir$ageCategory <- vapply(ir$age,codeAgeCat,character(1))
ir$ageCategory <- factor(ir$ageCategory,
                         levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34"
                                    ,"35-39","40-44","45-49","50-54","55-59","60-64"
                                    ,"65-69","70-74","75-79","80-84","85-89","90-94"
                                    ,"95+","missing")                          
)

ir$head.ageCategory <- vapply(ir$head.age,codeAgeCat,character(1))
ir$head.ageCategory <- factor(ir$head.ageCategory,
                              levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34"
                                         ,"35-39","40-44","45-49","50-54","55-59","60-64"
                                         ,"65-69","70-74","75-79","80-84","85-89","90-94"
                                         ,"95+","missing")                          
)

#Not really stunting, do we want to just call this "nutrition"?
ir$stunting <- NA
# ir$stunting[which(ir$child.height.age<= (-6))] <- "Implausibly low"
ir$stunting[which(ir$child.height.age > (-6) & ir$child.height.age<= (-3))] <- "Severely stunted"
ir$stunting[which(ir$child.height.age > (-3) & ir$child.height.age<= (-2))] <- "Stunted, but not severely"
ir$stunting[which(ir$child.height.age > (-2) & ir$child.height.age< (6))] <- "Not stunted"
# ir$stunting[which(ir$child.height.age>= (6))] <- "Implausibly high"

ir$stunting <- factor(ir$stunting
                      ,levels=c(
                        "Implausibly low"
                        ,"Severely stunted"
                        ,"Stunted, but not severely"
                        ,"Not stunted"
                        ,"Implausibly high"
                      ))
ir$filename <- "China"
china.data.total <- ir
data.total <- rbind(china.data.total,data.total)

setwd("D:/Documents/Data/MICSmeta/")
load("child.maternal.RData")
setnames(child.health,"skilled.attendant","ch.skilled.attendant")
child.health <- child.health[c("filename","cluster","household","ch.skilled.attendant","any.vacc")]
maternal.health <- maternal.health[c("filename","cluster","household","ceb","cdead","skilled.attendant","maternal.deaths")]
data.total <- join(data.total,child.health,by=c("filename","cluster","household"))
data.total <- join(data.total,maternal.health,by=c("filename","cluster","household"))
save(data.total,file="total_triple.RData")