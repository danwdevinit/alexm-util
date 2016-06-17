####Function and setup####
library(Hmisc)
library(plyr)
library(foreign)
library(data.table)

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

# dir <- "D:/Documents/Data/DHSauto/rwhr61dt"

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
    names(pr)[which(names(pr)=="hv106")] <- "educ"
    
    #Rename age var
    names(pr)[which(names(pr)=="hv105")] <- "age"
    
    #Rename sex var
    names(pr)[which(names(pr)=="hv104")] <- "sex"
    
    #Rename cluster/hh var
    names(pr)[which(names(pr)=="hv001")] <- "cluster"
    names(pr)[which(names(pr)=="hv002")] <- "household"
    
    #Head vars
    names(pr)[which(names(pr)=="hv219")] <- "head.sex"
    names(pr)[which(names(pr)=="hv220")] <- "head.age"
    
    #reg?
    names(pr)[which(names(pr)=="hv140")] <- "birth.cert"
    
    #nutrition
    names(pr)[which(names(pr)=="ha40")] <- "woman.bmi"
    if(typeof(pr$woman.bmi)!="NULL"){
      pr$woman.bmi <- pr$woman.bmi/100 
    }
    names(pr)[which(names(pr)=="hb40")] <- "man.bmi"
    if(typeof(pr$man.bmi)!="NULL"){
      pr$man.bmi <- pr$man.bmi/100 
    }
    names(pr)[which(names(pr)=="hc1")] <- "age.months"
    names(pr)[which(names(pr)=="hc2")] <- "weight.kg"
    names(pr)[which(names(pr)=="hc3")] <- "height.cm"
    names(pr)[which(names(pr)=="hc15")] <- "standing.lying"
    names(pr)[which(names(pr)=="hc6")] <- "child.height.age"
    pr$child.weights <- pr$weights
    
    povcalcut <- subset(povcalcuts,filename==hrBase)$hc
    povcalperc <- weighted.percentile(pr$wealth,pr$weights,prob=povcalcut)
    
    pr$p20 <- (pr$wealth <= povcalperc)
    
    keep <- c("wealth","weights","urban.rural","educ","age","sex","cluster","household","head.sex","head.age","p20"
              ,"birth.cert","birth.reg","age.months","weight.kg","height.cm","standing.lying","child.height.age"
              ,"woman.bmi","man.bmi","child.weights"
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

wd <- "D:/Documents/Data/DHSmeta"
setwd(wd)
metaData <- rbindlist(dataList)
# save(metaData,file="DHS_pr_crosstabs.RData")

setwd("D:/Documents/Data/MICSmeta")
varNames <- read.csv("mics_meta_vars_complete.csv",as.is=TRUE,na.strings="")
classes <- read.csv("global_mics_classes.csv",as.is=TRUE,na.strings="NAN")

wd <- "D:/Documents/Data/MICSauto/"
setwd(wd)

dataList <- list()
dataIndex <- 1

# List out all the directories in our wd, this is where our data is contained
dirs <- list.dirs(wd,full.names=TRUE)

# dir <- "D:/Documents/Data/MICSauto/Afghanistan_MICS4_Datasets"
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
    povcalperc <- weighted.percentile(hh$wealth,hh$weights,prob=povcalcut)
    
    hh$p20 <- (hh$wealth <= povcalperc)
    
    chkeep <- c("household","cluster","line","birth.cert","birth.reg","age.months","child.weights","weight.kg","standing.lying"
                ,"child.height.age")
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
    
    hhkeep <- c("wealth","weights","urban.rural","cluster","household","head.sex","head.age","p20")
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
              ,"woman.bmi","man.bmi","child.weights"
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
# save(micsMetaData,file="MICS_hl_crosstabs.RData")

data.total <- rbind(metaData,micsMetaData)
# save(data.total,file="total_crosstabs.RData")
