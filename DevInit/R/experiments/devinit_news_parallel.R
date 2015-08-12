#install.packages("rvest")
#install.packages("foreach")
#install.packages("doParallel")
library(plyr)
library(rvest)
library("parallel")
library("foreach")
library("doParallel")

if(exists("reports")){
  rm(reports)
}

cl <- makeCluster(detectCores() - 1, outfile="C:/git/alexm-util/DevInit/R/experiments/outfile.txt")
registerDoParallel(cl, cores = detectCores() - 1)

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
#reports <- reports[sample(1:nrow(reports),1000,replace=FALSE),]
texts <- foreach(i=1:nrow(reports),.packages=c("rvest")
                 ,.combine=rbind) %dopar% {
  sourceurl <- reports[i,58]
  sheet <- tryCatch({
    page <- html(sourceurl)
    text <- page %>%
      html_node("body") %>%
      html_text()
    text <- gsub("[[:space:]]", " ", text)
    if((i %% 100)==0){message(i)}
    text
  },warning = function(war){
  },error = function(err){
    NA
  })
}
stopCluster(cl)
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
