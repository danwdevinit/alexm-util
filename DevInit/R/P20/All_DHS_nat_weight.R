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

# Set up some empty lists to catch our crosstabs later
dataIndex <- 1
ageUrbanList <- list()
ageSexList <- list()
ageWealthList <- list()
ageEducList <- list()
sexUrbanList <- list()
sexWealthList <- list()
sexEducList <- list()
urbanWealthList <- list()
urbanEducList <- list()
wealthEducList <- list()

# Define relevant variables
keep <- c(
  "hv000"
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
  "country.phase"
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
      
      # Create some crosstabs here, and store their data in the previously empty lists
      ageSexList[[dataIndex]] <- crosstab(pr$ageCategory,pr$sex,weight=pr$sample.weight,drop.levels=FALSE)$tab
      if(hasUrban){
        sexUrbanList[[dataIndex]] <- crosstab(pr$sex,pr$urban,weight=pr$sample.weight,drop.levels=FALSE)$tab
        ageUrbanList[[dataIndex]] <- crosstab(pr$ageCategory,pr$urban,weight=pr$sample.weight,drop.levels=FALSE)$tab
      }
      if(hasEduc){
        ageEducList[[dataIndex]] <- crosstab(pr$ageCategory,pr$educ,weight=pr$sample.weight,drop.levels=FALSE)$tab
        sexEducList[[dataIndex]] <- crosstab(pr$sex,pr$educ,weight=pr$sample.weight,drop.levels=FALSE)$tab
        if(hasUrban){
          urbanEducList[[dataIndex]] <- crosstab(pr$urban,pr$educ,weight=pr$sample.weight,drop.levels=FALSE)$tab
        }
        if(hasWealth){
          wealthEducList[[dataIndex]] <- crosstab(pr$wealth,pr$educ,weight=pr$sample.weight,drop.levels=FALSE)$tab
        }
      }
      if(hasWealth){
        ageWealthList[[dataIndex]] <- crosstab(pr$ageCategory,pr$wealth,weight=pr$sample.weight,drop.levels=FALSE)$tab
        sexWealthList[[dataIndex]] <- crosstab(pr$sex,pr$wealth,weight=pr$sample.weight,drop.levels=FALSE)$tab
        if(hasUrban){
          urbanWealthList[[dataIndex]] <- crosstab(pr$urban,pr$wealth,weight=pr$sample.weight,drop.levels=FALSE)$tab
        }
      }
      dataIndex <- dataIndex + 1
    }
  }
}

# Combine crosstabs here, filtering out any null indexes due to skipping
crossTabs <- list()
crossTabs[["ageUrban"]] <- Reduce("+",Filter(Negate(is.null),ageUrbanList))
crossTabs[["ageSex"]] <- Reduce("+",Filter(Negate(is.null),ageSexList))
crossTabs[["ageWealth"]] <- Reduce("+",Filter(Negate(is.null),ageWealthList))
crossTabs[["ageEduc"]] <- Reduce("+",Filter(Negate(is.null),ageEducList))
crossTabs[["sexUrban"]] <- Reduce("+",Filter(Negate(is.null),sexUrbanList))
crossTabs[["sexWealth"]] <- Reduce("+",Filter(Negate(is.null),sexWealthList))
crossTabs[["sexEduc"]] <- Reduce("+",Filter(Negate(is.null),sexEducList))
crossTabs[["urbanWealth"]] <- Reduce("+",Filter(Negate(is.null),urbanWealthList))
crossTabs[["urbanEduc"]] <- Reduce("+",Filter(Negate(is.null),urbanEducList))
crossTabs[["wealthEduc"]] <- Reduce("+",Filter(Negate(is.null),wealthEducList))

# Write to XLSX
setwd("D:/Documents/Data/")
library(openxlsx)

# Create workbook
wb <- createWorkbook("DHS_crosstabs")

crossNames <- names(crossTabs)
for(i in 1:length(crossNames)){
  crossName <- crossNames[i]
  crossTab <- crossTabs[[i]]
  addWorksheet(wb,crossName)
  writeData(wb,sheet=crossName,crossTab,colNames=TRUE,rowNames=TRUE)
}

saveWorkbook(wb, "DHS_crosstabs_weighted.xlsx", overwrite = TRUE)