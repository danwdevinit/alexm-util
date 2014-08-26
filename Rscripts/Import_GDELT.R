setwd("~/R/Work/GDELT")
colnames <- read.csv("CSV.header.dailyupdates.csv", header = FALSE,
                     sep="\t",colClasses="character")
afrcountries <- read.csv("africanCountries.csv",header=FALSE)
yesterday <- format(Sys.Date()-1, "%Y%m%d")
dir.create(paste("~/R/Work/GDELT",yesterday,sep="/"))
setwd(paste("~/R/Work/GDELT",yesterday,sep="/"))
filename <- paste(yesterday,".export.CSV",sep="")
zipname <- paste("http://gdeltproject.org/data/dailyupdates/",
                 filename,".zip",sep="")
zip <- tempfile()
download.file(zipname,zip)
unzip(zip, files = NULL, list = FALSE, overwrite = TRUE,
      junkpaths = FALSE, exdir = ".", unzip = "internal",
      setTimes = FALSE)
data <- read.csv(filename, header = FALSE,
                 sep="\t",colClasses="character",
                 col.names = colnames)
d1 <- data.frame()
for(i in 1:nrow(afrcountries))
{
  d1 <- rbind(d1,data[which(data$Actor1CountryCode==afrcountries[i,1]|data$Actor2CountryCode==afrcountries[i,1]),])
  print(afrcountries[i,1])
}

d2 <- d1[which(d1$EventRootCode=="14"),]
#Graph
library(ggplot2)
library(scales)

for(i in 1:nrow(d2))
{
  d2$ActorActor[i] <- 0
  if(d2$Actor1CountryCode[i] %in% afrcountries[,1]){d2$ActorActor[i] <- d2$Actor1CountryCode[i]}
  if(d2$Actor2CountryCode[i] %in% afrcountries[,1] & d2$Actor2CountryCode[i]!=d2$ActorActor[i]){
    if(nchar(d2$ActorActor[i])>2) d2$ActorActor[i] <- paste(d2$ActorActor[i],d2$Actor2CountryCode[i],sep="-") else
    d2$ActorActor[i] <-d2$Actor2CountryCode[i]
  }
}
atitle <- format(Sys.Date()-1, "%B %d, %Y")
a <- ggplot(d2, aes(EventCode, fill=ActorActor) ) +
  geom_histogram(aes( y = ..count..))+
  xlab("Code") +
  ylab("Count") +
  ggtitle(paste("Protest Event Codes in African Countries -",atitle))
ggsave(a, file = "protests.jpg",dpi = 1000)
names(d2)[names(d2) == "ActionGeo_Lat"] <- "lat"
names(d2)[names(d2) == "ActionGeo_Long"] <- "long"
keep <- c("Actor1Name","Actor1CountryCode",
          "Actor2Name","Actor2CountryCode",
          "EventCode","lat","long")
write.csv(d2[keep],"protests.csv")
