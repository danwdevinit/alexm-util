#install.packages("xlsx")
#install.packages("rJava")
#Add jvm.dll to your PATH, w/ 64 bit java
options(java.parameters = "-Xmx8000m")
library(rJava)
library(xlsx)

#Garbage collection call for memory
jgc <- function()
{
  .jcall("java/lang/System", method = "gc")
}    

path <- "S:/Projects/Programme resources/Data/GHA calcs and analyses/April 2015/Datasets - do not edit/CRS/Raw/"
wd <- "S:/Projects/Programme resources/Data/GHA calcs and analyses/April 2015/Datasets - do not edit/CRS/"

#List all files in country-year
filenames <- list.files(path, pattern="*-enc.txt", full.names=TRUE)

#Iterate through files, reading them in
for (i in 1:length(filenames))
{
  jgc()
  message("Adding data frame", i)
  basename = substr(basename(filenames[i]), 1, nchar(basename(filenames[i])) - 4)
  dat <- read.table(
    filenames[i]
    ,header = TRUE
    ,sep = "|"
    ,fileEncoding="latin1"
    ,na.strings=NULL
    ,quote=""
    ,as.is=T
    ,fill=TRUE
  )
  #Write xlsx
  file <- paste0(basename,".xlsx")
  wb <- createWorkbook()
  sheet <- createSheet(wb,sheetName="crs data")
  addDataFrame(dat,sheet,row.names=FALSE)
  saveWorkbook(wb, file)
}
