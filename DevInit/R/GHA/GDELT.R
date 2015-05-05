wd <-"C:/Users/alexm/Documents/RWork/GDELT/"
setwd(wd)
colnames <- read.csv("CSV.header.dailyupdates.csv", header = FALSE,
                     sep="\t",colClasses="character")
date <- character(30)
protestCount <- numeric(30)
reportCount <- numeric(30)
for(i in 1:30){
  yesterday <- format(Sys.Date()-i, "%Y%m%d")
  yestwd <- paste(wd,yesterday,sep="/")
  message(yesterday)
  if(file.exists(yestwd)){
    setwd(yestwd)
    filename <- paste(yesterday,".export.CSV",sep="")
  }
  else{
    dir.create(paste(wd,yesterday,sep="/"))
    setwd(yestwd)
    filename <- paste(yesterday,".export.CSV",sep="")
    zipname <- paste("http://data.gdeltproject.org/events/",
                     filename,".zip",sep="")
    zip <- tempfile()
    download.file(zipname,zip)
    unzip(zip, files = NULL, list = FALSE, overwrite = TRUE,
          junkpaths = FALSE, exdir = ".", unzip = "internal",
          setTimes = FALSE)
  }
  data <- read.csv(filename, header = FALSE,
                   sep="\t",colClasses="character",
                   col.names = colnames)
  bdi <- subset(data,(Actor1CountryCode=="BDI" | Actor2CountryCode=="BDI"))
  protests <- subset(bdi,EventRootCode=="14")
  date[i]<- yesterday
  protestCount[i] <- nrow(protests)
  reportCount[i] <- nrow(bdi)
}

df <- data.frame(date,protestCount,reportCount)
