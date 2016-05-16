library(plyr)
library(Hmisc)
library(foreign)
library(ggplot2)
library(data.table)
library(descr)
library(MonetDB.R)

wd <- "D:/Documents/Data/DHSauto/"
setwd(wd)

monetNames <- function(df){
  x <- names(df)
  x <- tolower(x)
  x <- gsub(".","_",x,fixed=TRUE)
  return(x)
}

dbType <- function(conn,df){
  res <- c()
  for(i in 1:length(df)){
    res <- c(res,dbDataType(conn,df[,i]))
  }
  return(res)
}

conn <- dbConnect(dbDriver("MonetDB"),"monetdb://localhost/dhs")

dirs <- list.dirs(wd,full.names=FALSE)

# metaNames <- c()
# metaTypes <- c()
# 
# for(i in 2:length(dirs)){
#   dir <- dirs[i]
#   country <- tolower(substr(dir,1,2))
#   recode <- tolower(substr(dir,3,4))
#   phase <- as.integer(substr(dir,5,5))
#   if(recode=="pr"){
#     dtaPath <- list.files(paste0(wd,dir), pattern="*.dta",ignore.case=TRUE)[1]
#     dta <- read.dta(paste0(wd,dir,"/",dtaPath))
#     dta <- dta[, colSums(!is.na(dta)) != 0]
#     names <- names(dta)
#     if("hv000" %in% names){
#       message(dta$hv000[1])
#       metaNames <- c(metaNames,names)
#       metaTypes <- c(metaTypes,dbType(conn,dta))
#     }
#   }
# }
# 
# dbTypedf <- data.frame(metaNames,metaTypes,stringsAsFactors = FALSE)
# uniqueTypes <- unique(dbTypedf)
# dupedNames <- uniqueTypes[duplicated(uniqueTypes$metaNames),]$metaNames
# uniqueTypes <- transform(uniqueTypes,hasDup = metaNames %in% dupedNames)
# dups <- subset(uniqueTypes,hasDup==TRUE)
# dups <- dups[order(dups$metaNames),]
# 
# typeDefault <- function(vec){
#   if("STRING" %in% vec){
#     return("STRING")
#   }else if("DOUBLE PRECISION" %in% vec){
#     return("DOUBLE PRECISION")
#   }else{
#     return("INTEGER")
#   }
# }
# 
# dups <- ddply(dups,.(metaNames),summarise,metaTypes=typeDefault(metaTypes))
# 
# #Some vars are both string and integer. Default to string type now and sort out later
# uniqueTypes <- ddply(uniqueTypes,.(metaNames),summarise,metaTypes=typeDefault(metaTypes))

setwd("D:/Documents/Data/DHSmeta")
# write.csv(uniqueTypes,"pr_uniqueTypes.csv",row.names=FALSE)
# write.csv(dups,"pr_dups.csv",row.names=FALSE)
uniqueTypes <- read.csv("pr_uniqueTypes.csv",as.is=TRUE,na.strings="")
uniqueTypes <- rbind(uniqueTypes,c("filename","STRING"))
metaNames <- uniqueTypes$metaNames
# dups <- read.csv("pr_dups.csv",as.is=TRUE,na.strings="")
message(length(metaNames))
setwd(wd)

tableExists <- nrow(dbGetQuery(conn,"Select name FROM tables WHERE name like 'pr';"))>=1

if(tableExists){
  dbSendUpdate(conn,"DROP TABLE pr;")
  tableExists <- FALSE
}

for(i in 2:length(dirs)){
  dir <- dirs[i]
  country <- tolower(substr(dir,1,2))
  recode <- tolower(substr(dir,3,4))
  phase <- as.integer(substr(dir,5,5))
  if(recode=="pr"){
    dtaPath <- list.files(paste0(wd,dir), pattern="*.dta",ignore.case=TRUE)[1]
    pr <- read.dta(paste0(wd,dir,"/",dtaPath))
    pr$filename <- dir
    names <- names(pr)
    if("hv000" %in% names){
      message(paste("Reading",pr$hv000[1]))
      #Force datatype
      for(b in 1:length(names)){
        varname <- names[b]
        correctType <- uniqueTypes[which(uniqueTypes$metaNames==varname),]$metaTypes
        thisType <- dbDataType(conn,pr[varname])
        if(length(correctType)>0){
          if(thisType != correctType){
            if(correctType=="STRING"){
              pr[varname] <- as.character(unlist(pr[varname]))
            }else if(correctType=="DOUBLE PRECISION"){
              pr[varname] <- as.double(unlist(pr[varname]))
            }else if(correctType=="INTEGER"){
              pr[varname] <- as.integer(unlist(pr[varname]))
            }
          } 
        }
      }
      #Force conformity
      if(length(setdiff(metaNames,names))>0){
        for(y in 1:length(setdiff(metaNames,names))){
          pr[setdiff(metaNames,names)[y]] <- NA
        } 
      }
      #Force order/trunc any useless cols
      pr <- pr[metaNames]
      
      #Force formatting
      names(pr) <- monetNames(pr)
      
      if(!tableExists){
        create <- sprintf("CREATE TABLE pr ( %s )", 
          paste0(sprintf('"%s" %s', uniqueTypes$metaNames, 
          uniqueTypes$metaTypes), collapse=","))
        dbSendUpdate(conn,create)
        tableExists <- TRUE
      }
      
      message(paste("Writing",pr$hv000[1]))
      file <- "D:/Documents/Data/dhs_temp.csv"
      write.table(pr,file,na="",sep=",",row.names=FALSE,col.names=FALSE,fileEncoding="utf8")
      dbSendUpdate(conn
        ,paste0(
          "COPY INTO pr FROM '"
          ,file
          ,"' USING DELIMITERS ',','\n','\"' NULL as ''"
        )           
      )
      message("Write successful")
    }
  }
}

# Close connection 
dbDisconnect(conn)
