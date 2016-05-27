# wd <- "D:/Documents/Data/MICSauto/"
# setwd(wd)

# filenames <- list.files(wd, pattern="*.zip",ignore.case=TRUE)
# 
# for(i in 1:length(filenames)){
#   zip <- filenames[i]
#   unzip(zip, files = NULL, list = FALSE, overwrite = TRUE,
#         junkpaths = FALSE, unzip = "internal",
#         setTimes = FALSE)
# }

# dirs <- list.dirs(wd,full.names=FALSE)

# #Look for valid folders
# for(i in 2:length(dirs)){
#   dir <- dirs[i]
#   files <- list.files(paste0(wd,dir), pattern="*",full.names=TRUE,recursive=TRUE)
#   for(j in 1:length(files)){
#     file <- files[j]
#     if(!length(file)==0){
#       isSav <- grepl(".sav",file,ignore.case=TRUE)
#       if(isSav){
#         if(dir!=basename(dir)){
#           file.copy(file,strsplit(dir,"/")[[1]][1])
#           file.remove(file)
#         }
#       }
#       else{
#         dirToCreate <- paste0("D:/Documents/Data/MICSmeta/readmes/",basename(dir))
#         if(!file_test(op="-d", dirToCreate)){
#           dir.create(dirToCreate)
#         }
#         file.copy(file,paste0(dirToCreate,"/"))
#         file.remove(file)
#       } 
#     }
#   }
#   files <- list.files(paste0(wd,dir), pattern="*",full.names=TRUE,recursive=TRUE)
#   if(sum(grepl(".sav",files,ignore.case=TRUE))==0){
#     unlink(dir,recursive=TRUE)
#   }
# }

###Test sav structure

# dirs <- list.dirs(wd,full.names=TRUE)
# 
# for(i in 2:length(dirs)){
#   dir <- dirs[i]
#   files <- list.files(dir, pattern="*.sav",full.names=FALSE,ignore.case=TRUE)
#   if(sum(grepl("hh",files,ignore.case=TRUE))!=1){
#     message(dir) 
#   }
# }
# 
# for(i in 2:length(dirs)){
#   dir <- dirs[i]
#   files <- list.files(dir, pattern="*.sav",full.names=FALSE,ignore.case=TRUE)
#   if(sum(grepl("hl",files,ignore.case=TRUE))!=1){
#    message(dir) 
#   }
# }
# 
# for(i in 2:length(dirs)){
#   dir <- dirs[i]
#   files <- list.files(dir, pattern="*.sav",full.names=FALSE,ignore.case=TRUE)
#   if(sum(grepl("wm",files,ignore.case=TRUE))!=1){
#     message(dir) 
#   }
# }
# 
# for(i in 2:length(dirs)){
#   dir <- dirs[i]
#   files <- list.files(dir, pattern="*.sav",full.names=FALSE,ignore.case=TRUE)
#   if(sum(grepl("ch",files,ignore.case=TRUE))!=1){
#     message(dir) 
#   }
# }

###Let's write some CSVs

wd <- "D:/Documents/Data/MICSauto/"
setwd(wd)

library(foreign)
library(plyr)
library(data.table)

# HHdataList <- list()
# HLdataList <- list()
# WMdataList <- list()
# CHdataList <- list()
# dataIndex <- 1

dirs <- list.dirs(wd,full.names=TRUE)

for(i in 2:length(dirs)){
  dir <- dirs[i]
  message(basename(dir))
  files <- list.files(dir, pattern="*.sav",full.names=FALSE,ignore.case=TRUE)
  csvs <- list.files(dir, pattern="*.csv",full.names=FALSE,ignore.case=TRUE)
#   csvs <- c()
  if(!("hh.csv" %in% csvs)){
    hhName <- files[grepl("hh",files,ignore.case=TRUE)]
    hh <- read.spss(paste0(dir,"/",hhName),to.data.frame=TRUE,trim_values=TRUE,trim.factor.names=TRUE)
    hh <- data.frame(lapply(hh,function(x){gsub("\032","",x)}),stringsAsFactors=FALSE,check.names=FALSE)
    hh$filename <- basename(dir)
    names(hh) <- tolower(names(hh))
    write.csv(hh,paste0(dir,"/","hh.csv"),row.names=FALSE,na="")
  }else{
    hh <- read.csv(paste0(dir,"/hh.csv"),as.is=TRUE,na.strings="",check.names=FALSE)
  }
#   if(!("hl.csv" %in% csvs)){
#     hlName <- files[grepl("hl",files,ignore.case=TRUE)]
#     hl <- read.spss(paste0(dir,"/",hlName),to.data.frame=TRUE,trim_values=TRUE,trim.factor.names=TRUE)
#     hl <- data.frame(lapply(hl,function(x){gsub("\032","",x)}),stringsAsFactors=FALSE,check.names=FALSE)
#     hl$filename <- basename(dir)
#     names(hl) <- tolower(names(hl))
#     write.csv(hl,paste0(dir,"/","hl.csv"),row.names=FALSE,na="")
#   }else{
#     hl <- read.csv(paste0(dir,"/hl.csv"),as.is=TRUE,na.strings="",check.names=FALSE)
#   }
#   if(!("wm.csv" %in% csvs)){
#     wmName <- files[grepl("wm",files,ignore.case=TRUE)]
#     wm <- read.spss(paste0(dir,"/",wmName),to.data.frame=TRUE,trim_values=TRUE,trim.factor.names=TRUE)
#     wm <- data.frame(lapply(wm,function(x){gsub("\032","",x)}),stringsAsFactors=FALSE,check.names=FALSE)
#     wm$filename <- basename(dir)
#     names(wm) <- tolower(names(wm))
#     write.csv(wm,paste0(dir,"/","wm.csv"),row.names=FALSE,na="")
#   }else{
#     wm <- read.csv(paste0(dir,"/wm.csv"),as.is=TRUE,na.strings="",check.names=FALSE)
#   }
#   if(!("ch.csv" %in% csvs)){
#     chName <- files[grepl("ch",files,ignore.case=TRUE)]
#     ch <- read.spss(paste0(dir,"/",chName),to.data.frame=TRUE,trim_values=TRUE,trim.factor.names=TRUE)
#     ch <- data.frame(lapply(ch,function(x){gsub("\032","",x)}),stringsAsFactors=FALSE,check.names=FALSE)
#     ch$filename <- basename(dir)
#     names(ch) <- tolower(names(ch))
#     write.csv(ch,paste0(dir,"/","ch.csv"),row.names=FALSE,na="")
#   }else{
#     ch <- read.csv(paste0(dir,"/ch.csv"),as.is=TRUE,na.strings="",check.names=FALSE)
#   }
#   HHdataList[[dataIndex]] <- hh
#   HLdataList[[dataIndex]] <- hl
#   WMdataList[[dataIndex]] <- wm
#   CHdataList[[dataIndex]] <- ch
#   dataIndex <- dataIndex + 1
}

wd <- "D:/Documents/Data/MICSmeta/"
setwd(wd)

# hh.all <- rbindlist(HHdataList,fill=TRUE)
# write.csv(hh.all,"hh.all.csv",row.names=FALSE,na="")

# hl.all <- rbindlist(HLdataList,fill=TRUE)
# write.csv(hl.all,"hl.all.csv",row.names=FALSE,na="")
# 
# wm.all <- rbindlist(WMdataList,fill=TRUE)
# write.csv(wm.all,"wm.all.csv",row.names=FALSE,na="")
# 
# ch.all <- rbindlist(CHdataList,fill=TRUE)
# write.csv(ch.all,"ch.all.csv",row.names=FALSE,na="")

###Debug
# describe(hh.all$ws1)
# missing.water <- subset(hh.all,is.na(ws1))
# missing.water <- data.frame(missing.water)
# missing.water <- missing.water[, colSums(!is.na(missing.water)) != 0]
# names(missing.water)[grepl("pipe",missing.water,ignore.case=TRUE)]
# names(missing.water)[grepl("water",missing.water,ignore.case=TRUE)]
# names(missing.water)[grepl("protected",missing.water,ignore.case=TRUE)]
# names(missing.water)[grepl("river",missing.water,ignore.case=TRUE)]
# count(missing.water$filename)[order(-count(missing.water$filename)$freq),]
