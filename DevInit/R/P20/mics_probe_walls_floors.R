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
#Floor
hc3s <- c()
#Wall
hc5s <- c()
#Floor
hi8s <- c()
#Test for phase?
hh1s <- c()

# Loop through every dir
for(i in 2:length(dirs)){
  dir <- dirs[i]
  message(basename(dir))
  hh <- read.csv(paste0(dir,"/hh.csv"),as.is=TRUE,na.strings="",check.names=FALSE)
  filenames <- c(filenames,basename(dir))
  hc3s <- c(hc3s
            ,sum(
              grepl("mud",hh$hc3,ignore.case=TRUE)
              ,grepl("dirt",hh$hc3,ignore.case=TRUE)
              ,grepl("wood",hh$hc3,ignore.case=TRUE)
              ,grepl("terr",hh$hc3,ignore.case=TRUE)
              ,grepl("cemen",hh$hc3,ignore.case=TRUE)
              ,grepl("cimen",hh$hc3,ignore.case=TRUE)
              ,grepl("metal",hh$hc3,ignore.case=TRUE)
              ,grepl("adobe",hh$hc3,ignore.case=TRUE)
            )>0
  )
  hc5s <- c(hc5s
            ,sum(
              grepl("mud",hh$hc5,ignore.case=TRUE)
              ,grepl("dirt",hh$hc5,ignore.case=TRUE)
              ,grepl("wood",hh$hc5,ignore.case=TRUE)
              ,grepl("terr",hh$hc5,ignore.case=TRUE)
              ,grepl("cemen",hh$hc5,ignore.case=TRUE)
              ,grepl("cimen",hh$hc5,ignore.case=TRUE)
              ,grepl("metal",hh$hc5,ignore.case=TRUE)
              ,grepl("adobe",hh$hc5,ignore.case=TRUE)
            )>0
  )
  hi8s <- c(hi8s
            ,sum(
              grepl("mud",hh$hi8,ignore.case=TRUE)
              ,grepl("dirt",hh$hi8,ignore.case=TRUE)
              ,grepl("wood",hh$hi8,ignore.case=TRUE)
              ,grepl("terr",hh$hi8,ignore.case=TRUE)
              ,grepl("cemen",hh$hi8,ignore.case=TRUE)
              ,grepl("cimen",hh$hi8,ignore.case=TRUE)
              ,grepl("metal",hh$hi8,ignore.case=TRUE)
              ,grepl("adobe",hh$hi8,ignore.case=TRUE)
            )>0
  )
  hh1s <- c(hh1s,typeof(hh$hh1))
}

df <- data.frame(filenames,hc3s,hc5s,hi8s,hh1s)
df[which(df$filenames=="Algeria_MICS4_Datasets"),]$hc5s<-TRUE

zeroes <- subset(df,(hc3s==FALSE | hc5s==FALSE) & hh1s!="NULL")
insufficient <- c(
  "Argentina_MICS4_Datasets"
  ,"State of Palestine_MICS4_Datasets"
  ,"Yemen MICS 2006 SPSS Datasets"
  ,"Cuba MICS 2006 SPSS Datasets"
  ,"Cuba_MICS4_Datasets"
  ,"Cuba_MICS5_Datasets"
  ,"Jamaica MICS 2005 SPSS Datasets"
)
zeroes <- subset(zeroes,!(filenames %in% insufficient))

nrow(zeroes)

df <- df[order(df$hh1s),]

#Argentina_MICS4_Datasets is missing HC5, but has HC3
# Likewise with State of Palestine_MICS4_Datasets
# likewise with Yemen MICS 2006 SPSS Datasets

# Algeria_MICS4_Datasets, hh$hc5
# Motte de Terres 13
# Toub ou Terre Séchées 14
# Roseaux avec boue 21
# Pierre avec boue 22
# Pierres avec Chaux/Ciment 32
# Briques 33
# Planches de bois 36
# Parpaing 37
# Autre (à préciser)......... 96