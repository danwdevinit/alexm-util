library(Hmisc)
library(plyr)
library(foreign)
library(data.table)
library(varhandle)

wd <- "D:/Documents/Data/MICSmeta/"
setwd(wd)

povcalcuts <- read.csv("headcounts.csv",as.is=TRUE)

weighted.percentile <- function(x,w,prob,na.rm=TRUE){
  df <- data.frame(x,w)
  if(na.rm){
    df <- df[which(complete.cases(df)),]
  }
  #Sort
  df <- df[order(df$x),]
  sumw <- sum(df$w)
  df$cumsumw <- cumsum(df$w)
  #For each percentile
  cutList <- c()
  cutNames <-c()
  for(i in 1:length(prob)){
    p <- prob[i]
    pStr <- paste0(round(p*100,digits=2),"%")
    sumwp <- sumw*p
    df$above.prob <- df$cumsumw>=sumwp
    thisCut <- df$x[which(df$above.prob==TRUE)[1]]
    cutList <- c(cutList,thisCut)
    cutNames <- c(cutNames,pStr)
  }
  names(cutList) <- cutNames
  return(cutList)
}

# Import some libraries
library(plyr)
library(Hmisc)
library(foreign)
library(ggplot2)
library(data.table)
library(descr)

# set our working directory, change this if using on another machine
wd <- "D:/Documents/Data/DHSauto/"
setwd(wd)

# Stop crosstab from plotting everything
options(descr.plot = FALSE)

# A while-looped formula that generates age categories in 5 year chunks
codeAgeCat <- function(x){
  startAge <- 0
  ageDiff <- 4
  endAge <- 4
  if(is.na(x)){
    return("missing")
  }
  while(startAge<95){
    endAge <- startAge+ageDiff
    if(x>=startAge & x<=endAge){
      return(
        paste0(startAge,"-",endAge)  
      )
    }
    startAge <- endAge + 1
  }
  if(x>=95){
    return("95+")
  }
  return("missing")
}

# List out all the directories in our wd, this is where our data is contained
dirs <- list.dirs(wd,full.names=FALSE)

data <- list()
dataIndex <- 1
# Define relevant variables
keep <- c(
  "v000"
  ,"v001"
  ,"v002"
  ,"v005"
  ,"v007"
  ,"v012"
  ,"v014"
  ,"v106"
  ,"v102"
  ,"v191"
  ,"d005"
  ,"d101a"
  ,"d101b"
  ,"d101c"
  ,"d101d"
  ,"d101e"
  ,"d101f"
  ,"d101g"
  ,"d101h"
  ,"d101i"
  ,"d101j"
  ,"d102"
  ,"d103a"
  ,"d103b"
  ,"d103c"
  ,"d103d"
  ,"d103e"
  ,"d103f"
  ,"d104"
  ,"d105a"
  ,"d105b"
  ,"d105c"
  ,"d105d"
  ,"d105e"
  ,"d105f"
  ,"d105g"
  ,"d105h"
  ,"d105i"
  ,"d105j"
  ,"d105k"
  ,"d105l"
  ,"d105m"
  ,"d105n"
  ,"d106"
  ,"d107"
  ,"d108"
  ,"d109"
  ,"d110a"
  ,"d110b"
  ,"d110c"
  ,"d110d"
  ,"d110e"
  ,"d110f"
  ,"d110g"
  ,"d110h"
  ,"d111"
  ,"d112"
  ,"d112a"
  ,"d113"
  ,"d114"
  ,"d115b"
  ,"d115c"
  ,"d115d"
  ,"d115e"
  ,"d115f"
  ,"d115g"
  ,"d115h"
  ,"d115i"
  ,"d115j"
  ,"d115k"
  ,"d115l"
  ,"d115m"
  ,"d115n"
  ,"d115o"
  ,"d115p"
  ,"d115q"
  ,"d115r"
  ,"d115s"
  ,"d115t"
  ,"d115u"
  ,"d115v"
  ,"d115w"
  ,"d115x"
  ,"d115y"
  ,"d115xa"
  ,"d115xb"
  ,"d115xc"
  ,"d115xd"
  ,"d115xe"
  ,"d115xf"
  ,"d115xg"
  ,"d115xh"
  ,"d115xi"
  ,"d115xj"
  ,"d115xk"
  ,"d116"
  ,"d117a"
  ,"d118a"
  ,"d118b"
  ,"d118c"
  ,"d118d"
  ,"d118e"
  ,"d118f"
  ,"d118g"
  ,"d118h"
  ,"d118i"
  ,"d118j"
  ,"d118k"
  ,"d118l"
  ,"d118m"
  ,"d118n"
  ,"d118o"
  ,"d118p"
  ,"d118q"
  ,"d118r"
  ,"d118s"
  ,"d118t"
  ,"d118u"
  ,"d118v"
  ,"d118w"
  ,"d118x"
  ,"d118y"
  ,"d118xa"
  ,"d118xb"
  ,"d118xc"
  ,"d118xd"
  ,"d118xe"
  ,"d118xf"
  ,"d118xg"
  ,"d118xh"
  ,"d118xi"
  ,"d118xj"
  ,"d118xk"
  ,"d119a"
  ,"d119b"
  ,"d119c"
  ,"d119d"
  ,"d119e"
  ,"d119f"
  ,"d119g"
  ,"d119h"
  ,"d119i"
  ,"d119j"
  ,"d119k"
  ,"d119l"
  ,"d119m"
  ,"d119n"
  ,"d119o"
  ,"d119p"
  ,"d119q"
  ,"d119r"
  ,"d119s"
  ,"d119t"
  ,"d119u"
  ,"d119v"
  ,"d119w"
  ,"d119x"
  ,"d119y"
  ,"d119xa"
  ,"d119xb"
  ,"d119xc"
  ,"d119xd"
  ,"d119xe"
  ,"d119xf"
  ,"d119xg"
  ,"d119xh"
  ,"d119xi"
  ,"d119xj"
  ,"d119xk"
  ,"d120"
  ,"d121"
  ,"d122a"
  ,"d122b"
  ,"d122c"
  ,"d123"
  ,"d124"
  ,"d125"
  ,"d126"
  ,"d127"
  ,"d128"
  ,"d129"
  ,"d130a"
  ,"d130b"
)
# And the real names of the relevant variables
metaNames <- c(
  "country.phase"
  ,"cluster"
  ,"hhid"
  ,"sample.weight"
  ,"year"
  ,"age"
  ,"age.completeness"
  ,"educ"
  ,"urban"
  ,"wealth"
  ,"d005"
  ,"d101a"
  ,"d101b"
  ,"d101c"
  ,"d101d"
  ,"d101e"
  ,"d101f"
  ,"d101g"
  ,"d101h"
  ,"d101i"
  ,"d101j"
  ,"d102"
  ,"d103a"
  ,"d103b"
  ,"d103c"
  ,"d103d"
  ,"d103e"
  ,"d103f"
  ,"d104"
  ,"d105a"
  ,"d105b"
  ,"d105c"
  ,"d105d"
  ,"d105e"
  ,"d105f"
  ,"d105g"
  ,"d105h"
  ,"d105i"
  ,"d105j"
  ,"d105k"
  ,"d105l"
  ,"d105m"
  ,"d105n"
  ,"d106"
  ,"d107"
  ,"d108"
  ,"d109"
  ,"d110a"
  ,"d110b"
  ,"d110c"
  ,"d110d"
  ,"d110e"
  ,"d110f"
  ,"d110g"
  ,"d110h"
  ,"d111"
  ,"d112"
  ,"d112a"
  ,"d113"
  ,"d114"
  ,"d115b"
  ,"d115c"
  ,"d115d"
  ,"d115e"
  ,"d115f"
  ,"d115g"
  ,"d115h"
  ,"d115i"
  ,"d115j"
  ,"d115k"
  ,"d115l"
  ,"d115m"
  ,"d115n"
  ,"d115o"
  ,"d115p"
  ,"d115q"
  ,"d115r"
  ,"d115s"
  ,"d115t"
  ,"d115u"
  ,"d115v"
  ,"d115w"
  ,"d115x"
  ,"d115y"
  ,"d115xa"
  ,"d115xb"
  ,"d115xc"
  ,"d115xd"
  ,"d115xe"
  ,"d115xf"
  ,"d115xg"
  ,"d115xh"
  ,"d115xi"
  ,"d115xj"
  ,"d115xk"
  ,"d116"
  ,"d117a"
  ,"d118a"
  ,"d118b"
  ,"d118c"
  ,"d118d"
  ,"d118e"
  ,"d118f"
  ,"d118g"
  ,"d118h"
  ,"d118i"
  ,"d118j"
  ,"d118k"
  ,"d118l"
  ,"d118m"
  ,"d118n"
  ,"d118o"
  ,"d118p"
  ,"d118q"
  ,"d118r"
  ,"d118s"
  ,"d118t"
  ,"d118u"
  ,"d118v"
  ,"d118w"
  ,"d118x"
  ,"d118y"
  ,"d118xa"
  ,"d118xb"
  ,"d118xc"
  ,"d118xd"
  ,"d118xe"
  ,"d118xf"
  ,"d118xg"
  ,"d118xh"
  ,"d118xi"
  ,"d118xj"
  ,"d118xk"
  ,"d119a"
  ,"d119b"
  ,"d119c"
  ,"d119d"
  ,"d119e"
  ,"d119f"
  ,"d119g"
  ,"d119h"
  ,"d119i"
  ,"d119j"
  ,"d119k"
  ,"d119l"
  ,"d119m"
  ,"d119n"
  ,"d119o"
  ,"d119p"
  ,"d119q"
  ,"d119r"
  ,"d119s"
  ,"d119t"
  ,"d119u"
  ,"d119v"
  ,"d119w"
  ,"d119x"
  ,"d119y"
  ,"d119xa"
  ,"d119xb"
  ,"d119xc"
  ,"d119xd"
  ,"d119xe"
  ,"d119xf"
  ,"d119xg"
  ,"d119xh"
  ,"d119xi"
  ,"d119xj"
  ,"d119xk"
  ,"d120"
  ,"d121"
  ,"d122a"
  ,"d122b"
  ,"d122c"
  ,"d123"
  ,"d124"
  ,"d125"
  ,"d126"
  ,"d127"
  ,"d128"
  ,"d129"
  ,"d130a"
  ,"d130b"
)

# Loop through every dir
for(i in 2:length(dirs)){
  dir <- dirs[i]
  # Pull some coded info out of the dir name
  country <- tolower(substr(basename(dir),1,2))
  recode <- tolower(substr(basename(dir),3,4))
  phase <- as.integer(substr(basename(dir),5,5))
  # For this analysis, we're only interested in individual member recodes, or "hr"
  if(basename(dir) %in% povcalcuts$filename){
    message(basename(dir))
    hrwd <- dir
    if(!file_test(op="-d", hrwd)){next;}
    
    hrBase <- basename(hrwd)
    iso2 <- toupper(substr(hrBase,1,2))
    phase <- substr(hrBase,5,6)
    
    irwd <- paste0("D:/Documents/Data/DHSauto/",tolower(iso2),"ir",phase,"dt/")
    if(!file_test(op="-d", irwd)){next;}
    
    pr <- read.csv(paste0(irwd,iso2,"IR",phase,"FL.csv")
                   ,na.strings="",as.is=TRUE,check.names=FALSE)
    prNames <- names(pr)
    # Sanity check for a common variable, plus checking phase from filename
    if("v000" %in% prNames & "d005" %in% prNames){
      #Force conformity
      if(length(setdiff(keep,prNames))>0){
        for(y in 1:length(setdiff(keep,prNames))){
          pr[setdiff(keep,prNames)[y]] <- NA
        } 
      }
      # Message for a progress measure
      message(hrBase)
      #
      # Filter our set
      pr <- pr[keep]
      # Rename the resultant vars
      names(pr) <- metaNames
      pr$weights <- pr$sample.weight/1000000
      pr$wealth <- pr$wealth/100000
      
      povcalcut <- subset(povcalcuts,filename==hrBase)$hc
      povcalperc <- weighted.percentile(pr$wealth,pr$weights,prob=povcalcut)
      
      pr$p20 <- (pr$wealth < povcalperc)
      pr$filename <- hrBase
      data[[dataIndex]] <- pr
      dataIndex <- dataIndex + 1        
    }
  }
}

pr <- rbindlist(data,fill=TRUE)

# Some recoding work to standardize things
pr$urban <- tolower(pr$urban)
pr$urban <- factor(pr$urban
                   ,levels = c("urban","rural")
)
pr$educ[which(pr$educ==0)] <- "no education, preschool"
pr$educ[which(pr$educ==1)] <- "primary"
pr$educ[which(pr$educ==2)] <- "secondary"
pr$educ[which(pr$educ==3)] <- "higher"
pr$educ[which(pr$educ==8)] <- NA
pr$educ[which(pr$educ==9)] <- NA
pr$educ <- tolower(pr$educ)
pr$educ[which(pr$educ=="dk")] <- NA
pr$educ[which(pr$educ=="don't know")] <- NA
pr$educ <- factor(pr$educ
                  ,levels = c("no education, preschool","primary","secondary","higher")
)

pr$ageCategory <- vapply(pr$age,codeAgeCat,character(1))
pr$ageCategory <- factor(pr$ageCategory,
                         levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34"
                                    ,"35-39","40-44","45-49","50-54","55-59","60-64"
                                    ,"65-69","70-74","75-79","80-84","85-89","90-94"
                                    ,"95+","missing")                          
)
# Silly sample-weight recode here. Guide said divide by 1 million
pr$d005 <- pr$d005/1000000

#Recode some DV vars
pr$violence <- pr$d106
pr$violence[which(pr$violence==9)] <- NA
pr$violence[which(pr$violence=="no")] <- 0
pr$violence[which(pr$violence=="yes")] <- 1
pr$violence[which(pr$violence=="yes (d105a-d)")] <- 1
pr$severe.violence <- pr$d107
pr$severe.violence[which(pr$severe.violence==9)] <- NA
pr$severe.violence[which(pr$severe.violence=="no")] <- 0
pr$severe.violence[which(pr$severe.violence=="yes")] <- 1
pr$severe.violence[which(pr$severe.violence=="yes (d105e-g)")] <- 1
pr$sexual.violence <- pr$d108
pr$sexual.violence[which(pr$sexual.violence==9)] <- NA
pr$sexual.violence[which(pr$sexual.violence=="no")] <- 0
pr$sexual.violence[which(pr$sexual.violence=="yes")] <- 1
pr$sexual.violence[which(pr$sexual.violence=="yes (d105h-i)")] <- 1

#Subset for just DV?
setwd("D:/Documents/Data/DHSmeta")
pr <- subset(pr,!is.na(d005))
save(pr,file="dv.RData")