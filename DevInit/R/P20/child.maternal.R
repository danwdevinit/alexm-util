####Function and setup####
library(Hmisc)
library(plyr)
library(foreign)
library(data.table)

wd <- "D:/Documents/Data/DHSauto/"
setwd(wd)

#Not necessarily most recent, but most recent with child/mother questions
latest_surveys <- c(
  "alhr50dt", "amhr61dt", "aohr61dt", "azhr52dt", "bdhr70dt", "bfhr62dt"
  ,"bjhr61dt", "bohr51dt", "buhr61dt", "cdhr61dt", "cghr60dt"
  ,"cihr61dt", "cmhr60dt", "cohr61dt", "drhr61dt", "eghr61dt"
  ,"ethr61dt", "gahr60dt", "ghhr70dt", "gmhr60dt", "gnhr61dt", "gyhr5idt"
  ,"hnhr62dt", "hthr61dt", "iahr52dt", "idhr63dt", "johr6cdt"
  ,"kehr70dt","khhr72dt", "kmhr61dt"
  #   , "kyhr61dt"
  , "lbhr6adt", "lshr61dt"
  #   ,"mbhr53dt"
  , "mdhr51dt", "mlhr6hdt", "mvhr51dt", "mwhr61dt"
  ,"mzhr62dt", "nghr6adt", "nihr61dt", "nmhr61dt"
  # , "nphr60dt"
  ,"pehr6idt","phhr61dt","pkhr61dt"
  ,"rwhr70dt","slhr61dt","snhr70dt", "sthr50dt"
  # , "szhr51dt"
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
  ,"Chad_MICS4_Datasets","Costa Rica_MICS4_Datasets","Georgia MICS 2005 SPSS Datasets"
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

dataList <- list()
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
    
    krwd <- paste0("D:/Documents/Data/DHSauto/",tolower(iso2),"kr",phase,"dt/")
    if(!file_test(op="-d", krwd)){message("KR WD invalid");next;}
    
    kr <- read.csv(paste0(krwd,iso2,"KR",phase,"FL.csv")
                   ,na.strings="",as.is=TRUE,check.names=FALSE)
    
    names(kr)[which(names(kr)=="v001")] <- "cluster"
    names(kr)[which(names(kr)=="v002")] <- "household"
    names(kr)[which(names(kr)=="b5")] <- "child.alive"
    names(kr)[which(names(kr)=="b7")] <- "age.at.death.months"
    names(kr)[which(names(kr)=="m3h")] <- "trained.birth.attendant"
    vaccVars <- c("h2","h3","h4","h5","h6","h7","h8","h9","h10")
    krKeep <- c(
      "child.alive"
      ,"age.at.death.months"
      ,"trained.birth.attendant"
      ,vaccVars
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
    dataList[[dataIndex]] <- kr
    dataIndex <- dataIndex + 1
  }
}

wd <- "D:/Documents/Data/DHSmeta"
setwd(wd)
metaData <- rbindlist(dataList,fill=TRUE)
dhs.child.maternal <- metaData

# set our working directory, change this if using on another machine
wd <- "D:/Documents/Data/MICSauto/"
setwd(wd)

varNames <- read.csv("D:/Documents/Data/MICSmeta/mics_child_vars.csv",as.is=TRUE,na.strings="")

# List out all the directories in our wd, this is where our data is contained
dirs <- list.dirs(wd,full.names=TRUE)

dataList <- list()
dataIndex <- 1

for(i in 2:length(dirs)){
  dir <- dirs[i]
  if(basename(dir) %in% latest_surveys){
    this.varName <- subset(varNames,filename==basename(dir))
    message(basename(dir))
    if(exists("wm")){rm(wm)}
    if(exists("ch")){rm(ch)}
    load(paste0(dir,"/","wm.RData"))
    load(paste0(dir,"/","ch.RData"))
    names(wm) <- tolower(names(wm))
    names(ch) <- tolower(names(ch))
    ch.labs <- data.frame(names(ch)[1:length(ch)-1],attributes(ch)$variable.labels)
    names(ch.labs) <- c("name","description")
    wm.labs <- data.frame(names(wm)[1:length(wm)-1],attributes(wm)$variable.labels)
    names(wm.labs) <- c("name","description")
    ch <- data.frame(ch)
    wm <- data.frame(wm)
    vaccVars <- subset(this.varName,match=="vacc")$var
    birthVars <- subset(this.varName,match=="attendant" &  skilled==1)$var
    mortality <- c("ceb","deadkids","cdead")
    wmKeep <- c(
      birthVars
      ,mortality
    )
    names(ch)[which(names(ch)=="v001")] <- "cluster"
    names(ch)[which(names(ch)=="v002")] <- "household"
    vaccVars <- subset(this.varName,match=="vacc")$var
    names(ch)[which(names(ch)==vaccVars[1])] <- "any.vacc"
    
    
    names(wm)[which(names(wm)=="v001")] <- "cluster"
    names(wm)[which(names(wm)=="v002")] <- "household"
    dataList[[dataIndex]] <- data.frame(percentList)
    dataIndex <- dataIndex + 1
  }
}

wd <- "D:/Documents/Data/MICSmeta"
setwd(wd)
mics.meta <- rbindlist(dataList,fill=TRUE)
mics.child.maternal <- mics.meta
save(dhs.child.maternal,mics.child.maternal,file="child.maternal.RData")
