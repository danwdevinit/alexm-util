library(Hmisc)
library(data.table)
library(foreign)
library(descr)
library(plyr)

setwd("D:/Documents/Data/DHSmeta")

cwi <- read.csv("global_cwi.csv",na.strings="",as.is=TRUE)

# cwi <- read.csv("global_ccwi.csv",na.strings="",as.is=TRUE)

# setnames(cwi,"ccwi","cwi")

cwi$weights <- cwi$sample.weights/1000000

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

cwi$phase <- substr(cwi$filename,5,5)

cwi.table <- data.table(cwi)

cwi.collapse <- cwi.table[
  ,.(mean.cwi=weighted.mean(cwi,weights,na.rm=TRUE)
     ,median.cwi=weighted.percentile(cwi,weights,.5)
     ,filename=max(filename))
  , by=.(iso2,phase)]

isos <- unique(cwi.collapse$iso2)

latest_surveys <- c()
for(i in 1:length(isos)){
  iso <- isos[i]
  sub <- subset(cwi.collapse,iso2==iso)
  sub <- sub[order(-sub$phase)]
  latest <- sub$filename[1]
  latest_surveys <- c(latest_surveys,latest)
}

cwi <- subset(cwi,filename %in% latest_surveys)

quints <- weighted.percentile(cwi$cwi,cwi$weights,prob=seq(0,1,length=6))

for(i in 2:length(quints)){
  quint <- quints[i]
  quintName <- paste0("quint.",(i-1)*20)
  cwi[[quintName]] <- (cwi$cwi <= quint)
}

decs <- weighted.percentile(cwi$cwi,cwi$weights,prob=seq(0,1,length=11))
cwi$dec.50 <- (cwi$cwi <= decs[6])

write.csv(cwi,"cwi_percentiles.csv",na="",row.names=FALSE)

latest_surveys_pr <- gsub("hr","pr",latest_surveys)

wd <- "D:/Documents/Data/DHSauto/"
setwd(wd)

dirs <- list.dirs(wd,full.names=FALSE)

data <- list()
dataIndex <- 1
keep <- c(
  "hv000"
  ,"hv001"
  ,"hv002"
  ,"hv005"
  ,"hv025"
  ,"hv007"
  ,"hv104"
  ,"hv105"
  ,"hv106"
)
metaNames <- c(
  "country.phase"
  ,"cluster"
  ,"household"
  ,"sample.weight"
  ,"urban"
  ,"year"
  ,"sex"
  ,"age"
  ,"educ"
)

for(i in 2:length(dirs)){
  dir <- dirs[i]
  if(dir %in% latest_surveys_pr){
    dtaPath <- list.files(paste0(wd,dir), pattern="*.dta",ignore.case=TRUE)[1]
    dta <- read.dta(paste0(wd,dir,"/",dtaPath))
    message(dta$hv000[1])
    pr <- dta[keep]
    names(pr) <- metaNames
    filename <- paste0(substr(dir,1,2),"hr",substr(dir,5,6),"dt")
    pr$filename <- filename
    pr <- join(
      pr
      ,cwi
      ,by=c("cluster","household","filename")
    )
    data[[dataIndex]] <- pr
    dataIndex <- dataIndex + 1
  }
}

pr <- rbindlist(data,fill=TRUE)
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

setwd("D:/Documents/Data/DHSmeta")
write.csv(pr,"dhs_pr_cwi.csv",na="",row.names=FALSE)
# pr <- read.csv("dhs_pr_cwi.csv",na.strings="",as.is=TRUE)
# Stop crosstab from plotting everything
options(descr.plot = FALSE)
crossTabs <- list()
crossTabs[["agep.20"]] <- crosstab(pr$ageCategory,pr$quint.20,weight=pr$sample.weight)$tab
crossTabs[["educp.20"]] <- crosstab(pr$educ,pr$quint.20,weight=pr$sample.weight)$tab
crossTabs[["sexp.20"]] <- crosstab(pr$sex,pr$quint.20,weight=pr$sample.weight)$tab
crossTabs[["urbanp.20"]] <- crosstab(pr$urban,pr$quint.20,weight=pr$sample.weight)$tab

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

saveWorkbook(wb, "DHS_CWI_crosstabs_weighted_ccwi.xlsx", overwrite = TRUE)