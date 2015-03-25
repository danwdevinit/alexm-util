#install.packages('reshape')
library(reshape)

wd <- "C:/git/digital-platform/user-data"
setwd(wd)
filenames <- list.files("C:/git/digital-platform/country-year", pattern="*.csv", full.names=TRUE)

for (i in 1:length(filenames))
{
  data <- read.csv(filenames[i], header = TRUE,sep=",",na.strings="",check.names=FALSE)
  names <- colnames(data)
  if((length(names)==3 & "id" %in% names & "year" %in% names & "value" %in% names) | (length(names)==4 & "id" %in% names & "year" %in% names & "value" %in% names & "original-value" %in% names))  {
    basename = substr(basename(filenames[i]), 1, nchar(basename(filenames[i])) - 4)
    fwd = paste(wd,basename,sep="/")
    dir.create(fwd)
    data <- data[order(data$id,data$year),]
    data <- reshape(data,idvar="id",timevar="year",direction="wide")
    setwd(fwd)
    write.csv(data,paste(basename,"-wide",".csv",sep=""),row.names=FALSE,na="")
    setwd(wd)
  }
}
