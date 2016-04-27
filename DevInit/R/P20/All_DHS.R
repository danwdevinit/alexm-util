library(plyr)
library(Hmisc)
library(foreign)
library(ggplot2)
library(data.table)
library(descr)

wd <- "D:/Documents/Data/DHSauto/"
setwd(wd)

# filenames <- list.files(wd, pattern="*.zip",ignore.case=TRUE)
# 
# 
# for(i in 1:length(filenames)){
#   zip <- filenames[i]
#   unzip(zip, files = NULL, list = FALSE, overwrite = TRUE,
#         junkpaths = FALSE, unzip = "internal",
#         setTimes = FALSE)
# }

dirs <- list.dirs(wd,full.names=FALSE)

data <- list()
dataIndex <- 1
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

for(i in 2:length(dirs)){
  dir <- dirs[i]
  country <- tolower(substr(dir,1,2))
  recode <- tolower(substr(dir,3,4))
  phase <- as.integer(substr(dir,5,5))
  if(recode=="pr"){
    dtaPath <- list.files(paste0(wd,dir), pattern="*.dta",ignore.case=TRUE)[1]
    dta <- read.dta(paste0(wd,dir,"/",dtaPath))
    names <- names(dta)
    if("hv000" %in% names & phase>=6){
          message(dta$hv000[1])
          data[[dataIndex]] <- dta[keep]
          dataIndex <- dataIndex + 1
    }
  }
}

pr <- rbindlist(data,fill=TRUE)
names(pr) <- metaNames
pr$urban <- tolower(pr$urban)
pr$sex[which(pr$sex==1)] <- "male"
pr$sex[which(pr$sex==2)] <- "female"
pr$sex[which(pr$sex==9)] <- NA
pr$sex <- tolower(pr$sex)
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

pr$ageCategory <- vapply(pr$age,codeAgeCat,character(1))
pr$ageCategory <- factor(pr$ageCategory,
                         levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34"
                                    ,"35-39","40-44","45-49","50-54","55-59","60-64"
                                    ,"65-69","70-74","75-79","80-84","85-89","90-94"
                                    ,"95+","missing")                          
)
pr$sample.weight <- pr$sample.weight/1000000

setwd("D:/Documents/Data/")
write.csv(pr,"dhs_pr_min.csv",na="",row.names=FALSE)
crossTabs <- list()
crossTabs[["ageUrban"]] <- crosstab(pr$ageCategory,pr$urban,weight=pr$sample.weight)$tab
crossTabs[["ageSex"]] <- crosstab(pr$ageCategory,pr$sex,weight=pr$sample.weight)$tab
crossTabs[["ageWealth"]] <- crosstab(pr$ageCategory,pr$wealth,weight=pr$sample.weight)$tab
crossTabs[["ageEduc"]] <- crosstab(pr$ageCategory,pr$educ,weight=pr$sample.weight)$tab
crossTabs[["sexUrban"]] <- crosstab(pr$sex,pr$urban,weight=pr$sample.weight)$tab
crossTabs[["sexWealth"]] <- crosstab(pr$sex,pr$wealth,weight=pr$sample.weight)$tab
crossTabs[["sexEduc"]] <- crosstab(pr$sex,pr$educ,weight=pr$sample.weight)$tab
crossTabs[["urbanWealth"]] <- crosstab(pr$urban,pr$wealth,weight=pr$sample.weight)$tab
crossTabs[["urbanEduc"]] <- crosstab(pr$urban,pr$educ,weight=pr$sample.weight)$tab
crossTabs[["wealthEduc"]] <- crosstab(pr$wealth,pr$educ,weight=pr$sample.weight)$tab

library(openxlsx)

#Create workbook
wb <- createWorkbook("DHS_crosstabs")

crossNames <- names(crossTabs)
for(i in 1:length(crossNames)){
  crossName <- crossNames[i]
  crossTab <- crossTabs[[i]]
  addWorksheet(wb,crossName)
  writeData(wb,sheet=crossName,crossTab,colNames=TRUE,rowNames=TRUE)
}

saveWorkbook(wb, "DHS_crosstabs_weighted.xlsx", overwrite = TRUE)