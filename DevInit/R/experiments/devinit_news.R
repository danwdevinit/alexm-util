#install.packages("rvest")
library(plyr)
library(rvest)

wd <-"C:/Users/alexm/Documents/RWork/GDELT/"
setwd(wd)

#Download####
colnames <- read.csv("CSV.header.dailyupdates.csv", header = FALSE,
                     sep="\t",colClasses="character")
for(i in 1:1){
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
  if(exists("reports")){
    reports <- rbind(data,reports)
  }
  else{
    reports <- data
  }
}

#Analyze####
setwd("C:/git/alexm-util/DevInit/R/experiments")
#Subset?
reports <- reports[sample(1:nrow(reports),300,replace=FALSE),]
texts <- character(nrow(reports))
for(i in 1:nrow(reports)){
  sourceurl <- reports[i,58]
  sheet <- tryCatch({
    page <- html(sourceurl)
    text <- page %>%
      html_node("body") %>%
      html_text()
    text <- gsub("[[:space:]]", " ", text)
    texts[i] <- text
  },warning = function(war){
    message(paste(war,"on index",i))
    message("\n")
  },error = function(err){
    message(paste(err,"on index",i))
    message("\n")
    texts[i] <- NA
  })
}
reports[59] <- texts
names(reports)[59] <- "text"

containsDI <- function(textVector){
  results <- logical(length(textVector))
  for(i in 1:length(textVector)){
    text <- textVector[i]
    results[i] <- 
      grepl("Development Initiatives",text,ignore.case=TRUE)
      #| grepl(" DI ",text,ignore.case=TRUE)
  }
  return(results)
}

di <- subset(reports,containsDI(text))
