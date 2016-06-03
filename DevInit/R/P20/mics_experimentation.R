####Setup####
wd <- "D:/Documents/Data/MICSmeta/"
setwd(wd)

specific.results <- read.csv("missing.wide.csv",as.is=TRUE,na.strings="")

wd <- "D:/Documents/Data/MICSauto/"
setwd(wd)

library(foreign)
library(plyr)
library(data.table)

dirs <- list.dirs(wd,full.names=TRUE)

#narrow search
# usefulVarshh = list(
#   "toilets" = c("type of toilet")
#   ,"share.toilets" = c("facility shared")
#   ,"tv" = c("television")
#   ,"phone" = c("phone")
#   ,"fridge" = c("refrig","refrid")
#   ,"car" = c("car or truc")
# )
# 
# usefulVarshl = list(
#   "attended" = c("ever attended"),
#   "school" = c("highest level"),
#   "grade" = c("highest grade")
# )

#wide search
usefulVarshh = list(
  "toilets" = c("toilet","banheiro","baño","facility","ws7","ws8")
  ,"share.toilets" = c("toilet","banheiro","baño","facility","share","ws8","ws9")
  ,"tv" = c("tv","telev","télév","flat","screen","nv",)
  ,"phone" = c("phone","telep","telef","télép","teléf","fon")
  ,"fridge" = c("frig","frid","gele","gela")
  ,"car" = c("car","truc","coch","cami","voit")
)

usefulVarshl = list(
  "attended" = c("ed2","ed3"),
  "school" = c("ed3","ed4"),
  "grade" = c("ed3","ed4")
)

parseMICS <- function(hh,hl){
  missing.results <- subset(specific.results,filename==hh$filename & is.na(value))$match
  matches <- c()
  varNames <- c()
  varLabels <- c()
  hh.var.lab <- attributes(hh)$variable.labels
  for(i in 1:length(hh.var.lab)){
    var <- names(hh.var.lab[i])
    lab <- hh.var.lab[[i]]
    lab <- paste(var,lab,sep=" : ")
    for(j in 1:length(usefulVarshh)){
      usefulVar <- names(usefulVarshh[j])
      usefulLabs <- usefulVarshh[[j]]
      useful <- sum(sapply(usefulLabs,grepl,x=lab,ignore.case=TRUE),na.rm=TRUE)>0
      unuseful <- FALSE
      if(useful & usefulVar %in% missing.results){
        matches <- c(matches,usefulVar)
        varNames <- tolower(c(varNames,var))
        varLabels <- c(varLabels,lab)
      }
    }
  }
  hl.var.lab <- attributes(hl)$variable.labels
  for(i in 1:length(hl.var.lab)){
    var <- names(hl.var.lab[i])
    lab <- hl.var.lab[[i]]
    lab <- paste(var,lab,sep=" : ")
    for(j in 1:length(usefulVarshl)){
      usefulVar <- names(usefulVarshl[j])
      usefulLabs <- usefulVarshl[[j]]
      useful <- sum(sapply(usefulLabs,grepl,x=lab,ignore.case=TRUE),na.rm=TRUE)>0
      unuseful <- FALSE
      if(useful & usefulVar %in% missing.results){
        matches <- c(matches,usefulVar)
        varNames <- tolower(c(varNames,var))
        varLabels <- c(varLabels,lab)
      }
    }
  }
  df <- data.frame(matches,varNames,varLabels)
  if(length(df)==0){
    df[1,1]=NA
    df[1,2]=NA
    df[1,3]=NA
  }
  names(df) <- c("match","varName","varLabel")
  df$filename <- hh$filename
  return(df)
}

####Run####

dataList <- list()
dataIndex <- 1

for(i in 2:length(dirs)){
  dir <- dirs[i]
  message(basename(dir))
  if(exists("hh")){rm(hh)}
  if(exists("hl")){rm(hl)}
  load(paste0(dir,"/","hh.RData"))
  load(paste0(dir,"/","hl.RData"))
  names(hh) <- tolower(names(hh))
  names(hl) <- tolower(names(hl))
  if(typeof(hh$hh1)!="NULL"){
    vars <- parseMICS(hh,hl)
    dataList[[dataIndex]] <- vars
    dataIndex <- dataIndex + 1 
  }
}

metaData <- rbindlist(dataList,fill=TRUE)
wd <- "D:/Documents/Data/MICSmeta/"
setwd(wd)
write.csv(metaData,"mics_meta_vars_wide.csv",row.names=FALSE,na="")
