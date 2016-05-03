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
  "hv001"
  ,"hv002"
  ,"hv000"
  ,"hv005"
  ,"hv025"
  ,"hv007"
  ,"hv104"
  ,"hv105"
  ,"hv106"
  ,"hv270"
)
# And the real names of the relevant variables
metaNames <- c(
  "cluster"
  ,"household"
  ,"country.phase"
  ,"sample.weight"
  ,"urban"
  ,"year"
  ,"sex"
  ,"age"
  ,"educ"
  ,"wealth"
)

# Loop through every dir
for(i in 2:length(dirs)){
  dir <- dirs[i]
  # Pull some coded info out of the dir name
  country <- tolower(substr(dir,1,2))
  recode <- tolower(substr(dir,3,4))
  phase <- as.integer(substr(dir,5,5))
  # For this analysis, we're only interested in household member recodes, or "pr"
  if(recode=="pr"){
    # Find the .dta and read it in
    dtaPath <- list.files(paste0(wd,dir), pattern="*.dta",ignore.case=TRUE)[1]
    pr <- read.dta(paste0(wd,dir,"/",dtaPath))
    names <- names(pr)
    # Sanity check for a common variable, plus checking phase from filename
    if("hv000" %in% names & phase>=6){
      # Message for a progress measure
      message(pr$hv000[1])
      # Filter our set
      pr <- pr[keep]
      # Rename the resultant vars
      names(pr) <- metaNames
      # Some recoding work to standardize things
      pr$urban <- tolower(pr$urban)
      pr$urban <- factor(pr$urban
                         ,levels = c("urban","rural")
      )
      pr$sex[which(pr$sex==1)] <- "male"
      pr$sex[which(pr$sex==2)] <- "female"
      pr$sex[which(pr$sex==9)] <- NA
      pr$sex <- tolower(pr$sex)
      pr$sex <- factor(pr$sex
                       ,levels = c("male","female")
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
      pr$wealth <- tolower(pr$wealth)
      pr$wealth <- factor(pr$wealth
                          ,levels = c("poorest","poorer","middle","richer","richest")
      )
      
      
      
      pr$ageCategory <- vapply(pr$age,codeAgeCat,character(1))
      pr$ageCategory <- factor(pr$ageCategory,
                               levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34"
                                          ,"35-39","40-44","45-49","50-54","55-59","60-64"
                                          ,"65-69","70-74","75-79","80-84","85-89","90-94"
                                          ,"95+","missing")                          
      )
      # Silly sample-weight recode here. Guide said divide by 1 million
      pr$sample.weight <- pr$sample.weight/1000000
      
      # Precautions against whole columns of NA
      hasWeight <- length(pr$sample.weight[which(!is.na(pr$sample.weight))])>0
      hasUrban <- length(pr$urban[which(!is.na(pr$urban))])>0
      hasSex <- length(pr$sex[which(!is.na(pr$sex))])>0
      hasAge <- length(pr$age[which(!is.na(pr$age))])>0
      hasEduc <- length(pr$educ[which(!is.na(pr$educ))])>0
      hasWealth <- length(pr$wealth[which(!is.na(pr$wealth))])>0
      
      pivot <- ddply(pr,.(cluster,household,country.phase,urban,year,wealth),summarise
                     ,members=sum(!is.na(sex))
                     ,women=sum(sex=="female",na.rm=TRUE)
                     ,men=sum(sex=="male",na.rm=TRUE)
                     ,youth=sum(age<15,na.rm=TRUE)
                     ,elderly=sum(age>49,na.rm=TRUE)
                     ,target.age=sum(age>=15 & age <=49, na.rm=TRUE)
                     ,youth.women=sum(age<15 & sex=="female",na.rm=TRUE)
                     ,youth.men=sum(age<15 & sex=="male",na.rm=TRUE)
                     ,elderly.women=sum(age>49 & sex=="female",na.rm=TRUE)
                     ,elderly.men=sum(age>49 & sex=="male",na.rm=TRUE)
                     ,target.age.women=sum(age>=15 & age <=49 & sex=="female", na.rm=TRUE)
                     ,target.age.men=sum(age>=15 & age <=49 & sex=="male", na.rm=TRUE)
                     ,hh.weight=sum(sample.weight,na.rm=TRUE)
                     )
      
      data[[dataIndex]] <- pivot
      
      dataIndex <- dataIndex + 1
    }
  }
}

pr <- rbindlist(data,fill=TRUE)
write.csv(pr,"dhs_hh_min.csv",na="",row.names=FALSE)

# Write to XLSX
setwd("D:/Documents/Data/")
library(openxlsx)

# Create workbook
wb <- createWorkbook("Households_Disagg")
addWorksheet(wb,"hh disagg")
writeData(wb,sheet="hh disagg",pr,colNames=TRUE,rowNames=FALSE)

saveWorkbook(wb, "Households_Disagg.xlsx", overwrite = TRUE)

pr <- transform(pr,percentElderly=elderly/members,percentFemale=women/members)

plot(percentElderly~wealth,data=pr)
plot(wealth~percentElderly,data=pr)

plot(percentFemale~wealth,data=pr)
plot(wealth~percentFemale,data=pr)

plot(count(pr$wealth[which(pr$percentFemale==1)]),main="Frequency of wealth indexes for all female households")
plot(count(pr$wealth[which(pr$percentFemale==0)]),main="Frequency of wealth indexes for all male households")