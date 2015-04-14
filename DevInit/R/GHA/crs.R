#Add package 'httr'
library(httr)

#Set the working directory
wd <- "C:/users/alexm/Documents/Rwork"
setwd(wd)

#Define URL
url <- "https://stats.oecd.org/FileView2.aspx?IDFile=21ee64b2-51c0-4f81-bd1c-ee3c24391084"
#GET url
response <- GET(url)
#Write tempfile
zip <- tempfile()
writeBin(content(response, as = "raw"), zip)
#Find file path
path <- unzip(zip, list = TRUE)$Name
#Unzip it
unzip(zip, exdir = ".")
#Read it

dat <- read.table(
  path
  ,header = TRUE
  ,sep = "|"
  ,fileEncoding="UTF-16"
  ,na.strings=NULL
  ,quote=""
  ,as.is=T
  ,fill=TRUE
  ,encoding="UTF-8 BOM"
  ,nrow=80000
)
