path <- "S:/Projects/Programme resources/Data/GHA calcs and analyses/April 2015/Datasets - do not edit/CRS/Raw/"
wd <- "S:/Projects/Programme resources/Data/GHA calcs and analyses/April 2015/Datasets - do not edit/CRS/"
setwd(wd)

#List all files in country-year
filenames <- list.files(path, pattern="*-enc.txt", full.names=TRUE)

#Iterate through files, reading them in
for (i in 1:length(filenames))
{
  basename = substr(basename(filenames[i]), 1, nchar(basename(filenames[i])) - 8)
  message("Reading file: ", basename)
  dat <- read.table(
    filenames[i]
    ,header = TRUE
    ,sep = "|"
    ,na.strings=""
    ,encoding="UTF-8"
    ,quote=""
    ,as.is=T
    ,fill=TRUE
    ,comment.char=""
    ,allowEscapes = TRUE
  )
  #Get rid of Byte Order Mark
  names(dat)[1] <- "Year"
  #Write
  message("Writing file: ", basename)
  #Open UTF8 connection
  con<-file(paste0(basename,".csv"),encoding="utf8")
  write.csv(dat,con,na="",row.names=FALSE)
}
