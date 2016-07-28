library(rCharts)
library(plyr)

wd <-"D:/Documents/RWork/GDELT/"
setwd(wd)
colnames <- read.csv("CSV.header.dailyupdates.csv", header = FALSE,
                     sep="\t",colClasses="character")

dataList <- list()
dataIndex <- 1
for(i in 1:100){
  yesterday <- format(Sys.Date()-i, "%Y%m%d")
  yestwd <- paste(wd,yesterday,sep="/")
  message(paste(i,yesterday))
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
  refugees.idps <- data[which(data$Actor1Code=="REF" | data$Actor2Code=="REF"),]
  migrants <- data[which(grepl("migrant",data$Actor1Name,ignore.case=TRUE) | grepl("migrant",data$Actor2Name,ignore.case=TRUE)),]
  refIdpMig <- rbind(refugees.idps,migrants)
  dataList[[dataIndex]] <- refIdpMig
  dataIndex <- dataIndex + 1
}

reports <- rbindlist(dataList)

reports$AvgTone <- as.double(reports$AvgTone)
reports$tag <- NA
reports$tag[which(reports$Actor1Code=="REF" | reports$Actor2Code=="REF")] <- "Refugees and IDPs"
reports$tag[which(is.na(reports$tag))] <- "Migrants and immigrants"

reports$positive <- reports$AvgTone >= 0

save(reports,file=paste0("D:/Documents/Data/GDELT/ref reports ",Sys.Date(),".RData"))

reports.tab <- data.table(reports)
# df <- reports.tab[,.(
#   count=sum(!is.na(GLOBALEVENTID))
#   ,avgTone = mean(AvgTone,na.rm=TRUE)
#   ),by=.(SQLDATE,tag,ActionGeo_CountryCode)
#   ]
df <- reports.tab[,.(
  count=sum(!is.na(GLOBALEVENTID))
  ,avgTone = mean(AvgTone,na.rm=TRUE)
),by=.(tag,ActionGeo_CountryCode,positive)
]

write.csv(df,"D:/Documents/Data/GDELT/refugees and migrants.csv",row.names=FALSE,na="")
write.csv(subset(reports,tag=="Refugees and IDPs"),"D:/Documents/Data/GDELT/refugee reports.csv")
write.csv(subset(reports,tag=="Migrants and immigrants"),"D:/Documents/Data/GDELT/migrant reports.csv")

write.csv(subset(df,tag=="Refugees and IDPs" & positive==TRUE),"D:/Documents/Data/GDELT/pos-refugee.csv",row.names=FALSE,na="")
write.csv(subset(df,tag=="Refugees and IDPs" & positive==FALSE),"D:/Documents/Data/GDELT/neg-refugee.csv",row.names=FALSE,na="")
write.csv(subset(df,tag=="Migrants and immigrants" & positive==TRUE),"D:/Documents/Data/GDELT/pos-migrants.csv",row.names=FALSE,na="")
write.csv(subset(df,tag=="Migrants and immigrants" & positive==FALSE),"D:/Documents/Data/GDELT/neg-migrants.csv",row.names=FALSE,na="")