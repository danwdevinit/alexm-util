library(Hmisc)
library(plyr)
library(foreign)
library(data.table)
library(descr)
library(varhandle)
library(WDI)

# Stop crosstab from plotting everything
options(descr.plot = FALSE)

wd <- "D:/Documents/Data/MICSmeta"
setwd(wd)
load("total_crosstabs_coded.RData")
load("D:/Documents/Data/ChinaSurvey/crosstab.RData")
data.total <- rbind(data.total,china.data.total,fill=TRUE)

pop.confidence <- function(x,y,w,pop){
  ct <- crosstab(x,y,weight=w,prop.t=TRUE,drop.levels=FALSE)
  props <- ct$prop.tbl
  cv <- sd(w,na.rm=TRUE)/mean(w,na.rm=TRUE)
  deft <- cv*cv+1
  n <- ct$total.n
  SEs <- sqrt(((1-(n/pop))/n)*(pop/(pop-1))*(props*(1-props)))
  corrected.SEs <- SEs*deft
  low.end <- (props-(2*corrected.SEs))*pop
  low.end <- pmax(low.end,0)
  middle.end <- props*pop
  high.end <- (props+(2*corrected.SEs))*pop
  high.end <- pmin(high.end,pop)
  return(
    list(
       low = low.end
       ,middle = middle.end
       ,high = high.end
      )
    )
}

countryMeta <- read.csv("headcounts.csv",as.is=TRUE)

indicator <- "SP.POP.TOTL"

pop <- WDI(country = "all", 
           indicator = indicator, 
           start = 2000, 
           end = 2015,
           extra = TRUE
)

keep <- c("iso3c","year","SP.POP.TOTL")
pop <- pop[keep]
names(pop) <- c("iso3","year","pop")

countryMeta <- join(
  countryMeta
  ,pop
  ,by=c("iso3","year")
  )

countryMeta[which(countryMeta$iso3=="KEN"),]$pop <- 44863583
countryMeta[which(countryMeta$iso3=="RWA"),]$pop <- 11341544
countryMeta[which(countryMeta$iso3=="TMP"),]$pop <- 1048367
countryMeta[which(countryMeta$iso2=="XK"),]$pop <- 1823149

####Debug####
# dat.tab <- data.table(data.total)
# dat.collapse <- dat.tab[
#   ,.(p20=weighted.mean(p20,weights,na.rm=TRUE))
#   , by=.(filename)
#   ]
# countryMeta <- join(countryMeta,dat.collapse,by="filename")
####End debug####

crossTabs <- list()

filenames <- countryMeta$filename
for(i in 1:length(filenames)){
  this.filename <- filenames[i]
  message(this.filename)
  dat <- subset(data.total,filename==this.filename)
  if(nrow(dat)>0){
    this.pop <- subset(countryMeta,filename==this.filename)$pop
    #Urban-P20
    if(length(dat$urban[which(!is.na(dat$urban))])!=0){
      confidence.tab <- pop.confidence(dat$urban,dat$p20,dat$weights,this.pop)
      if(is.null(crossTabs[["urbanp20"]])){
        crossTabs[["urbanp20"]] <- confidence.tab
      }else{
        crossTabs[["urbanp20"]]$low <- crossTabs[["urbanp20"]]$low + confidence.tab$low
        crossTabs[["urbanp20"]]$middle <- crossTabs[["urbanp20"]]$middle + confidence.tab$middle
        crossTabs[["urbanp20"]]$high <- crossTabs[["urbanp20"]]$high + confidence.tab$high
      }  
    }
    #Educ-P20
    if(length(dat$educ[which(!is.na(dat$educ))])!=0){
      confidence.tab <- pop.confidence(dat$educ,dat$p20,dat$weights,this.pop)
      if(is.null(crossTabs[["educp20"]])){
        crossTabs[["educp20"]] <- confidence.tab
      }else{
        crossTabs[["educp20"]]$low <- crossTabs[["educp20"]]$low + confidence.tab$low
        crossTabs[["educp20"]]$middle <- crossTabs[["educp20"]]$middle + confidence.tab$middle
        crossTabs[["educp20"]]$high <- crossTabs[["educp20"]]$high + confidence.tab$high
      }  
    }
    #Age-P20
    if(length(dat$ageCategory[which(!is.na(dat$ageCategory))])!=0){
      confidence.tab <- pop.confidence(dat$ageCategory,dat$p20,dat$weights,this.pop)
      if(is.null(crossTabs[["ageCategoryp20"]])){
        crossTabs[["ageCategoryp20"]] <- confidence.tab
      }else{
        crossTabs[["ageCategoryp20"]]$low <- crossTabs[["ageCategoryp20"]]$low + confidence.tab$low
        crossTabs[["ageCategoryp20"]]$middle <- crossTabs[["ageCategoryp20"]]$middle + confidence.tab$middle
        crossTabs[["ageCategoryp20"]]$high <- crossTabs[["ageCategoryp20"]]$high + confidence.tab$high
      }  
    }
    #Sex-P20
    if(length(dat$sex[which(!is.na(dat$sex))])!=0){
      confidence.tab <- pop.confidence(dat$sex,dat$p20,dat$weights,this.pop)
      if(is.null(crossTabs[["sexp20"]])){
        crossTabs[["sexp20"]] <- confidence.tab
      }else{
        crossTabs[["sexp20"]]$low <- crossTabs[["sexp20"]]$low + confidence.tab$low
        crossTabs[["sexp20"]]$middle <- crossTabs[["sexp20"]]$middle + confidence.tab$middle
        crossTabs[["sexp20"]]$high <- crossTabs[["sexp20"]]$high + confidence.tab$high
      }  
    }
    #Head-age-P20
    if(length(dat$head.ageCategory[which(!is.na(dat$head.ageCategory))])!=0){
      confidence.tab <- pop.confidence(dat$head.ageCategory,dat$p20,dat$weights,this.pop)
      if(is.null(crossTabs[["head.ageCategoryp20"]])){
        crossTabs[["head.ageCategoryp20"]] <- confidence.tab
      }else{
        crossTabs[["head.ageCategoryp20"]]$low <- crossTabs[["head.ageCategoryp20"]]$low + confidence.tab$low
        crossTabs[["head.ageCategoryp20"]]$middle <- crossTabs[["head.ageCategoryp20"]]$middle + confidence.tab$middle
        crossTabs[["head.ageCategoryp20"]]$high <- crossTabs[["head.ageCategoryp20"]]$high + confidence.tab$high
      }  
    }
    #Head-sex-P20
    if(length(dat$head.sex[which(!is.na(dat$head.sex))])!=0){
      confidence.tab <- pop.confidence(dat$head.sex,dat$p20,dat$weights,this.pop)
      if(is.null(crossTabs[["head.sexp20"]])){
        crossTabs[["head.sexp20"]] <- confidence.tab
      }else{
        crossTabs[["head.sexp20"]]$low <- crossTabs[["head.sexp20"]]$low + confidence.tab$low
        crossTabs[["head.sexp20"]]$middle <- crossTabs[["head.sexp20"]]$middle + confidence.tab$middle
        crossTabs[["head.sexp20"]]$high <- crossTabs[["head.sexp20"]]$high + confidence.tab$high
      }  
    }
  }
}

library(openxlsx)

#Create workbook
wb <- createWorkbook("crosstabs")

crossNames <- names(crossTabs)
for(i in 1:length(crossNames)){
  crossName <- crossNames[i]
  crossTab <- crossTabs[[i]]
  tabNames <- names(crossTab)
  for(j in 1:length(crossTab)){
    this.tab <- crossTab[[j]]
    this.tabname <- paste0(crossName,".",tabNames[j])
    addWorksheet(wb,this.tabname)
    writeData(wb,sheet=this.tabname,this.tab,colNames=TRUE,rowNames=TRUE)
  }
}

saveWorkbook(wb, "DHS_and_MICS_P20_crosstabs_pop_weighted.xlsx", overwrite = TRUE)