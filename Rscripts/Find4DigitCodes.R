setwd("~/R/Work/GDELT")
colnames <- read.csv("CSV.header.dailyupdates.csv", header = FALSE,
                     sep="\t",colClasses="character")
afrcountries <- read.csv("africanCountries.csv",header=FALSE)
fourdigitcodes <- data.frame()
for(j in 1:62)
{
  yesterday <- format(Sys.Date()-j, "%Y%m%d")
  filename <- paste(yesterday,".export.CSV",sep="")
  zipname <- paste("http://gdeltproject.org/data/dailyupdates/",
                   filename,".zip",sep="")
  zip <- tempfile()
  download.file(zipname,zip)
  csv <- unzip(zip, files = NULL, list = FALSE, overwrite = TRUE,
        junkpaths = FALSE, exdir = ".", unzip = "internal",
        setTimes = FALSE)
  data <- read.csv(filename, header = FALSE,
                   sep="\t",colClasses="character",
                   col.names = colnames)
  unlink(zip)
  file.remove(filename)
  d1 <- data.frame()
  for(i in 1:nrow(afrcountries))
  {
    d1 <- rbind(d1,data[which(data$Actor1CountryCode==afrcountries[i,1]|data$Actor2CountryCode==afrcountries[i,1]),])
  }
  
  d2 <- d1[which(d1$EventRootCode=="14"),]
  d3 <- d2[which(nchar(d2$EventCode)>3),]
  fourdigitcodes <- rbind(fourdigitcodes,d3)
}
write.csv(fourdigitcodes,"fourdigitcodes.csv")