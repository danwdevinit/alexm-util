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
#Change encoding
#http://stackoverflow.com/questions/28979857/is-there-a-sed-type-package-in-r-for-removing-embedded-nuls
#Read it

dat <- read.csv(
  path
  ,header = TRUE
  ,sep = "|"
  ,fileEncoding="UTF-16LE"
  ,na.strings=""
  ,quote=""
  ,stringsAsFactors=FALSE
  ,skipNul=TRUE
)
