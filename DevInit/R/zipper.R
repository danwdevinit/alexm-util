#install.packages("utils")
library(utils)
wd <- "C:/git/digital-platform/user-data"
setwd(wd)

filenames <- list.files(wd, pattern="/*", full.names=FALSE)

for(i in 1:length(filenames)){
  files <- dir(filenames[i],full.names=TRUE)
  zip(zipfile = filenames[i],files=files)
}