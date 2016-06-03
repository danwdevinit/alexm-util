wd <- "D:/Documents/Data/MICSauto/"
setwd(wd)

filenames <- list.files(wd, pattern="*.zip",ignore.case=TRUE)

for(i in 1:length(filenames)){
  zip <- filenames[i]
  unzip(zip, files = NULL, list = FALSE, overwrite = TRUE,
        junkpaths = FALSE, unzip = "internal",
        setTimes = FALSE)
}

dirs <- list.dirs(wd,full.names=FALSE)

#Look for valid folders
for(i in 2:length(dirs)){
  dir <- dirs[i]
  files <- list.files(paste0(wd,dir), pattern="*",full.names=TRUE,recursive=TRUE)
  for(j in 1:length(files)){
    file <- files[j]
    if(!length(file)==0){
      isSav <- grepl(".sav",file,ignore.case=TRUE)
      if(isSav){
        if(dir!=basename(dir)){
          file.copy(file,strsplit(dir,"/")[[1]][1])
          file.remove(file)
        }
      }
      else{
        dirToCreate <- paste0("D:/Documents/Data/MICSmeta/readmes/",basename(dir))
        if(!file_test(op="-d", dirToCreate)){
          dir.create(dirToCreate)
        }
        file.copy(file,paste0(dirToCreate,"/"))
        file.remove(file)
      } 
    }
  }
  files <- list.files(paste0(wd,dir), pattern="*",full.names=TRUE,recursive=TRUE)
  if(sum(grepl(".sav",files,ignore.case=TRUE))==0){
    unlink(dir,recursive=TRUE)
  }
}

###Test sav structure

dirs <- list.dirs(wd,full.names=TRUE)

for(i in 2:length(dirs)){
  dir <- dirs[i]
  files <- list.files(dir, pattern="*.sav",full.names=FALSE,ignore.case=TRUE)
  if(sum(grepl("hh",files,ignore.case=TRUE))!=1){
    message(dir) 
  }
}

for(i in 2:length(dirs)){
  dir <- dirs[i]
  files <- list.files(dir, pattern="*.sav",full.names=FALSE,ignore.case=TRUE)
  if(sum(grepl("hl",files,ignore.case=TRUE))!=1){
   message(dir) 
  }
}

for(i in 2:length(dirs)){
  dir <- dirs[i]
  files <- list.files(dir, pattern="*.sav",full.names=FALSE,ignore.case=TRUE)
  if(sum(grepl("wm",files,ignore.case=TRUE))!=1){
    message(dir) 
  }
}

for(i in 2:length(dirs)){
  dir <- dirs[i]
  files <- list.files(dir, pattern="*.sav",full.names=FALSE,ignore.case=TRUE)
  if(sum(grepl("ch",files,ignore.case=TRUE))!=1){
    message(dir) 
  }
}

###Let's write some CSVs

wd <- "D:/Documents/Data/MICSauto/"
setwd(wd)

library(foreign)
library(plyr)
library(data.table)


dirs <- list.dirs(wd,full.names=TRUE)

for(i in 2:length(dirs)){
  dir <- dirs[i]
  message(basename(dir))
  files <- list.files(dir, pattern="*.sav",full.names=FALSE,ignore.case=TRUE)
  rDatas <- list.files(dir, pattern="*.RData",full.names=FALSE,ignore.case=TRUE)
#   csvs <- list.files(dir, pattern="*.csv",full.names=TRUE,ignore.case=TRUE)
#   for(j in 1:length(csvs)){
#     csv <- csvs
#     file.remove(csv)
#   }
#   rDatas <- c()
  if(!("hh.RData" %in% rDatas)){
    hhName <- files[grepl("hh",files,ignore.case=TRUE)]
    hh <- read.spss(paste0(dir,"/",hhName),trim_values=TRUE,trim.factor.names=TRUE)
    hh$filename <- basename(dir)
    save(hh,file=paste0(dir,"/","hh.RData"))
  }else{
#     load(paste0(dir,"/","hh.RData"))
  }
  if(!("hl.RData" %in% rDatas)){
    hlName <- files[grepl("hl",files,ignore.case=TRUE)]
    hl <- read.spss(paste0(dir,"/",hlName),trim_values=TRUE,trim.factor.names=TRUE)
    hl$filename <- basename(dir)
    save(hl,file=paste0(dir,"/","hl.RData"))
  }else{
#     load(paste0(dir,"/","hl.RData"))
  }
  if(!("wm.RData" %in% rDatas)){
    wmName <- files[grepl("wm",files,ignore.case=TRUE)]
    wm <- read.spss(paste0(dir,"/",wmName),trim_values=TRUE,trim.factor.names=TRUE)
    wm$filename <- basename(dir)
    save(wm,file=paste0(dir,"/","wm.RData"))
  }else{
#     load(paste0(dir,"/","wm.RData"))
  }
  if(!("ch.RData" %in% rDatas)){
    chName <- files[grepl("ch",files,ignore.case=TRUE)]
    ch <- read.spss(paste0(dir,"/",chName),trim_values=TRUE,trim.factor.names=TRUE)
    ch$filename <- basename(dir)
    save(ch,file=paste0(dir,"/","ch.RData"))
  }else{
#     load(paste0(dir,"/","ch.RData"))
  }
}

wd <- "D:/Documents/Data/MICSmeta/"
setwd(wd)
