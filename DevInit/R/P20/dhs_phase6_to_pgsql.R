library(plyr)
library(Hmisc)
library(foreign)
library(ggplot2)
library(data.table)
library(descr)
library(RPostgreSQL)

wd <- "D:/Documents/Data/DHSauto/"
setwd(wd)

dirs <- list.dirs(wd,full.names=FALSE)

# metaNames <- c()
# 
# for(i in 2:length(dirs)){
#   dir <- dirs[i]
#   country <- tolower(substr(dir,1,2))
#   recode <- tolower(substr(dir,3,4))
#   phase <- as.integer(substr(dir,5,5))
#   if(recode=="pr"){
#     dtaPath <- list.files(paste0(wd,dir), pattern="*.dta",ignore.case=TRUE)[1]
#     dta <- read.dta(paste0(wd,dir,"/",dtaPath))
#     names <- names(dta)
#     if("hv000" %in% names){
#       message(dta$hv000[1])
#       metaNames <- unique(c(metaNames,names))
#     }
#   }
# }

setwd("D:/Documents/Data")
# write.csv(metaNames,"pr_metanames.csv",row.names=FALSE)
metaNames <- read.csv("pr_metanames.csv",as.is=TRUE,na.strings="")
metaNames <- metaNames$x
message(length(metaNames))
setwd(wd)

db <- "dhs"
host <- "localhost"
username <- "postgres"
port <- 5432
# password <- ""

options(sqldf.RPostgreSQL.user = username, 
        sqldf.RPostgreSQL.password = password,
        sqldf.RPostgreSQL.dbname = db,
        sqldf.RPostgreSQL.host = host, 
        sqldf.RPostgreSQL.port = port)

# Establish connection to PoststgreSQL using RPostgreSQL
drv <- dbDriver("PostgreSQL")

# Full version of connection setting
con <- dbConnect(drv, dbname=db,host=host,port=port,user=username,password=password )

# overwrite=TRUE will change both data and table structure
# When row.name=TRUE then column named row.names will be added to the table
sqldf("drop table if exists pr;")

for(i in 2:length(dirs)){
  dir <- dirs[i]
  country <- tolower(substr(dir,1,2))
  recode <- tolower(substr(dir,3,4))
  phase <- as.integer(substr(dir,5,5))
  if(recode=="pr"){
    dtaPath <- list.files(paste0(wd,dir), pattern="*.dta",ignore.case=TRUE)[1]
    pr <- read.dta(paste0(wd,dir,"/",dtaPath))
    names <- names(pr)
    if("hv000" %in% names){
      message(pr$hv000[1])
      #Force conformity
      if(length(setdiff(metaNames,names))>0){
        for(y in 1:length(setdiff(metaNames,names))){
          pr[setdiff(metaNames,names)[y]] <- NA
        } 
      }
      dbWriteTable(con, "pr", value=pr,append=TRUE, row.names=FALSE)
    }
  }
}

# Close PostgreSQL connection 
dbDisconnect(con)