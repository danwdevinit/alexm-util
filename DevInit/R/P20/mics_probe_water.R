####Function and setup####
library(Hmisc)
library(plyr)
library(foreign)
library(data.table)
####Run function####
# set our working directory, change this if using on another machine
wd <- "D:/Documents/Data/MICSauto/"
setwd(wd)

# List out all the directories in our wd, this is where our data is contained
dirs <- list.dirs(wd,full.names=TRUE)

filenames <- c()
ws1s <- c()
ws2s <- c()
ws3s <- c()

# Loop through every dir
for(i in 2:length(dirs)){
  dir <- dirs[i]
  message(basename(dir))
  hh <- read.csv(paste0(dir,"/hh.csv"),as.is=TRUE,na.strings="",check.names=FALSE)
  filenames <- c(filenames,basename(dir))
  ws1s <- c(ws1s
            ,sum(
              grepl("bore",hh$ws1,ignore.case=TRUE)
              ,grepl("river",hh$ws1,ignore.case=TRUE)
              ,grepl("dam",hh$ws1,ignore.case=TRUE)
              ,grepl("pipe",hh$ws1,ignore.case=TRUE)
              ,grepl("tap",hh$ws1,ignore.case=TRUE)
              ,grepl("lagoa",hh$ws1,ignore.case=TRUE)
              ,grepl("cistern",hh$ws1,ignore.case=TRUE)
              ,grepl("surface",hh$ws1,ignore.case=TRUE)
              ,grepl("protege",hh$ws1,ignore.case=TRUE)
              ,grepl("protect",hh$ws1,ignore.case=TRUE)
              ,grepl("pompe",hh$ws1,ignore.case=TRUE)
              ,grepl("tube",hh$ws1,ignore.case=TRUE)
            )>0
  )
  ws2s <- c(ws2s
            ,sum(
              grepl("bore",hh$ws2,ignore.case=TRUE)
              ,grepl("river",hh$ws2,ignore.case=TRUE)
              ,grepl("dam",hh$ws2,ignore.case=TRUE)
              ,grepl("pipe",hh$ws2,ignore.case=TRUE)
              ,grepl("tap",hh$ws2,ignore.case=TRUE)
              ,grepl("lagoa",hh$ws2,ignore.case=TRUE)
              ,grepl("cistern",hh$ws2,ignore.case=TRUE)
              ,grepl("surface",hh$ws2,ignore.case=TRUE)
              ,grepl("protege",hh$ws2,ignore.case=TRUE)
              ,grepl("protect",hh$ws2,ignore.case=TRUE)
              ,grepl("pompe",hh$ws2,ignore.case=TRUE)
              ,grepl("tube",hh$ws2,ignore.case=TRUE)
            )>0
  )
  ws3s <- c(ws3s
            ,sum(
              grepl("bore",hh$ws3,ignore.case=TRUE)
              ,grepl("river",hh$ws3,ignore.case=TRUE)
              ,grepl("dam",hh$ws3,ignore.case=TRUE)
              ,grepl("pipe",hh$ws3,ignore.case=TRUE)
              ,grepl("tap",hh$ws3,ignore.case=TRUE)
              ,grepl("lagoa",hh$ws3,ignore.case=TRUE)
              ,grepl("cistern",hh$ws3,ignore.case=TRUE)
              ,grepl("surface",hh$ws3,ignore.case=TRUE)
              ,grepl("protege",hh$ws3,ignore.case=TRUE)
              ,grepl("protect",hh$ws3,ignore.case=TRUE)
              ,grepl("pompe",hh$ws3,ignore.case=TRUE)
              ,grepl("tube",hh$ws3,ignore.case=TRUE)
            )>0
  )
}

df <- data.frame(filenames,ws1s,ws2s,ws3s)
# df[which(df$filenames=="Comoros 2000 MICS_Datasets"),]$ws3s<-TRUE

zeroes <- subset(df,ws1s==FALSE & ws2s==FALSE & ws3s==FALSE)
insufficient <- c(
  "Algeria_MICS4_Datasets"
  ,"Indonesia MICS2 2000_Datasets"
  ,"Philippines 1999 MICS_Datasets"
)
zeroes <- subset(zeroes,!(filenames %in% insufficient))

nrow(zeroes)

notws1 <- subset(df,ws1s==FALSE)

#Conclusion, all use WS1 or ws1