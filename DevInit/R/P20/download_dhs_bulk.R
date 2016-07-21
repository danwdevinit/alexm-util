library(curl)
library(devtools)
library(foreign)
library(varhandle)

wd <- "D:/Documents/Data/DHS gps data"
setwd(wd)

# url <- "https://dhsprogram.com/data/download/urlslist_87786.txt"
# 
# links <- readLines(curl(url))
# 
# for(i in 1:length(links)){
#   link <- links[i]
#   filename <- substr(basename(link),31,42)
#   if(filename!=""){
#     download.file(link,filename,mode="wb")
#     unzip(filename)
#   }
# }

# zips <- list.files(pattern="*.zip")
# 
# for(i in 1:length(zips)){
#   zip <- zips[i]
#   unzip(zip)
# }

metaDat <- read.dbf("D:/Documents/Data/sdr_subnational_data_2016-07-20/shps/sdr_subnational_data_quickstats.dbf")

ccs <- unique(metaDat[c("DHSCC","SVYYEAR")])$DHSCC
years <- unique(metaDat[c("DHSCC","SVYYEAR")])$SVYYEAR
ccyears <- paste0(ccs,years)
dbfs <- list.files(pattern="*.dbf")
for(i in 1:length(dbfs)){
  dbf <- dbfs[i]
  zip <- paste0(substr(dbf,1,nchar(dbf)-4),".zip")
  dat <- read.dbf(dbf)
  cc <- unfactor(dat$DHSCC[1])
  year <- dat$DHSYEAR[1]
  ccyear <- paste0(cc,year)
  if(ccyear %in% ccyears){
    file.copy(zip,paste0("D:/Documents/Data/sdr_subnational_data_2016-07-20/clusters"))
  }
}