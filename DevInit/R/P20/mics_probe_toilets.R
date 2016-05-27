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
ws8s <- c()
ws7s <- c()
ws3s <- c()
#Test for phase?
hh1s <- c()

# Loop through every dir
for(i in 2:length(dirs)){
  dir <- dirs[i]
  message(basename(dir))
  hh <- read.csv(paste0(dir,"/hh.csv"),as.is=TRUE,na.strings="",check.names=FALSE)
  filenames <- c(filenames,basename(dir))
  ws8s <- c(ws8s
            ,sum(
              grepl("flush",hh$ws8,ignore.case=TRUE)
              ,grepl("latr",hh$ws8,ignore.case=TRUE)
              ,grepl("sept",hh$ws8,ignore.case=TRUE)
              ,grepl("toil",hh$ws8,ignore.case=TRUE)
              ,grepl("compost",hh$ws8,ignore.case=TRUE)
              ,grepl("letri",hh$ws8,ignore.case=TRUE)
              )>0
            )
  ws7s <- c(ws7s
            ,sum(
              grepl("flush",hh$ws7,ignore.case=TRUE)
              ,grepl("latr",hh$ws7,ignore.case=TRUE)
              ,grepl("sept",hh$ws7,ignore.case=TRUE)
              ,grepl("toil",hh$ws7,ignore.case=TRUE)
              ,grepl("compost",hh$ws7,ignore.case=TRUE)
              ,grepl("letri",hh$ws7,ignore.case=TRUE)
              )>0
            )
  ws3s <- c(ws3s
            ,sum(
              grepl("flush",hh$ws3,ignore.case=TRUE)
              ,grepl("latr",hh$ws3,ignore.case=TRUE)
              ,grepl("sept",hh$ws3,ignore.case=TRUE)
              ,grepl("toil",hh$ws3,ignore.case=TRUE)
              ,grepl("compost",hh$ws3,ignore.case=TRUE)
              ,grepl("letri",hh$ws3,ignore.case=TRUE)
            )>0
  )
  hh1s <- c(hh1s,typeof(hh$hh1))
}

df <- data.frame(filenames,ws3s,ws8s,ws7s,hh1s)
df[which(df$filenames=="Comoros 2000 MICS_Datasets"),]$ws3s<-TRUE
df[which(df$filenames=="Guyana 2000 MICS_Datasets"),]$ws3s<-TRUE
df[which(df$filenames=="Moldova MICS2 2000_Datasets"),]$ws3s<-TRUE
df[which(df$filenames=="Swaziland 2000 MICS_Datasets"),]$ws3s<-TRUE
zeroes <- subset(df,ws8s==FALSE & ws7s==FALSE & ws3s==FALSE)
insufficient <- c(
  "Algeria_MICS4_Datasets"
  ,"Argentina_MICS4_Datasets"
  ,"Indonesia MICS2 2000_Datasets"
  ,"Philippines 1999 MICS_Datasets"
  ,"Trinidad and Tobago 2000 MICS_Datasets"
  )
zeroes <- subset(zeroes,!(filenames %in% insufficient))

nrow(zeroes)

code.toiletsVar <- function(ws3s,ws7s,ws8s){
  toiletsVar <- c()
  for(i in 1:length(ws3s)){
    ws3 <- ws3s[i]
    ws7 <- ws7s[i]
    ws8 <- ws8s[i]
    if(sum(ws3,ws7,ws8)>1){
      message("More than one??")
    }else if(sum(ws3,ws7,ws8)==0){
      toiletsVar <- c(toiletsVar,NA)
    }else if(sum(ws3,ws7,ws8)==1){
      if(ws3){
        toiletsVar <- c(toiletsVar,"ws3")
      }else if(ws7){
        toiletsVar <- c(toiletsVar,"ws7")
      }else if(ws8){
        toiletsVar <- c(toiletsVar,"ws8")
      }
    }
  }
  return(toiletsVar)
}

df$toiletsVar <- code.toiletsVar(df$ws3s,df$ws7s,df$ws8s)
keep <- c("filenames","toiletsVar","hh1s")
df<-df[keep]
names(df) <- c("filename","toiletsVar","ID type")

setwd("D:/Documents/Data/MICSmeta/")
write.csv(df,"toiletsVars.csv",na="",row.names=FALSE)

# Algeria_MICS4_Datasets
# Argentina_MICS4_Datasets
# Indonesia MICS2 2000_Datasets
#Insufficient data ^^^^

#Comoros 2000 MICS_Datasets, hh$ws3
#Chasse d'eau avec ou avec septique ......................... 1
# Latrines a evacuation ......... ....................... 2
# Latrines ameliorees a ventilation (VIP) ....... 3
# Latrines traditionnelles ............................... 4
# Trou ouvert ....................................... ......... 5
# Sceau ........................................................ 6
# Autre (a preciser) .............. .. ........................ 7       

#Guyana 2000 MICS_Datasets, hh$ws3
# Flush to sewage system or septic tank .....01
# Pour flush latrine (water seal type)............02
# Improved pit latrine (e.g., VIP) ..................03
# Traditional pit latrine..................................04
# Open pit ....................................................05
# Other (specify) ..........................................06
# No facilities or bush or field.......................88

#Moldova MICS2 2000_Datasets, hh$ws3
# No codebook available and no metadata in .sav... unique vals are 1:7,97,98,NA 
# Additional vars
# "ectoilfl" - Toilet -flush to sewage system or septic tank, ws3==1
# "ectoilpo" - Toilet -pour flush latrine/ improved pit latrine, ws3==2,3
# "ectoiltr" - Toilet -traditional pit latrine, ws3==4
# "ectoilot"- Toilet -other, ws3=5,6,7,97,98
#From MICS2 questionnaire
# Flush to sewage system or septic tank  1
# Pour flush latrine (water seal type)	2
# Improved pit latrine (e.g., VIP)	3
# Traditional pit latrine	4
# Open pit	5
# Bucket	6
# Other (specify)	 7
# 
# No facilities or bush or field	8

#Swaziland 2000 MICS_Datasets, hh$ws3
# Flush to sewage system or septic tank .......1
# Pour flush latrine (water seal type).........2
# Improved pit latrine (e.g., VIP).............3
# Traditional pit latrine......................4
# Other locally used method of disposal**......5
# No facilities or bush or field...............6
# Other........................................7

