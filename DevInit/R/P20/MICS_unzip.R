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
  if(sum(grepl("hh",files))!=1){
    message(dir)
  }
}

