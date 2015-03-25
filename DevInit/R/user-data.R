#install.packages('reshape')
library(reshape)

wd <- "C:/git/digital-platform/user-data"
setwd(wd)
filenames <- list.files("C:/git/digital-platform/country-year", pattern="*.csv", full.names=TRUE)

for (i in 1:length(filenames))
{
  data <- read.csv(filenames[i], header = TRUE,sep=",",na.strings="",check.names=FALSE)
  names <- colnames(data)
  basename = substr(basename(filenames[i]), 1, nchar(basename(filenames[i])) - 4)
  fwd = paste(wd,basename,sep="/")
  dir.create(fwd)
  setwd(fwd)
  if("id" %in% names & "year" %in% names)  {
    wdata <- reshape(data,idvar="id",timevar="year",direction="wide")
    write.csv(wdata,paste(basename,"-wide",".csv",sep=""),row.names=FALSE,na="")
  }
  write.csv(data,paste(basename,".csv",sep=""),row.names=FALSE,na="")
  setwd(wd)
}
