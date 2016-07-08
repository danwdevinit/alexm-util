library(Hmisc)
library(plyr)
library(foreign)
library(data.table)
library(descr)
library(varhandle)

# Stop crosstab from plotting everything
options(descr.plot = FALSE)

wd <- "D:/Documents/Data/MICSmeta"
setwd(wd)
load("total_crosstabs_coded.RData")
load("D:/Documents/Data/ChinaSurvey/crosstab.RData")
load("D:/Documents/Data/BrazilSurvey/spss/crosstab.RData")
data.total <- rbind(data.total,china.data.total,fill=TRUE)
data.total <- rbind(data.total,brazil.data.total,fill=TRUE)

data.total$sex <- factor(data.total$sex,levels=c("Male","Female"))
data.total$head.sex <- factor(data.total$head.sex,levels=c("Male","Female"))
data.total$p20[which(data.total$p20==TRUE)] <- "P20"
data.total$p20[which(data.total$p20==FALSE)] <- "P80"
data.total$birth.reg[which(data.total$birth.reg==0)] <- "Not birth registered"
data.total$birth.reg[which(data.total$birth.reg==1)] <- "Birth registered"
data.total$urban[which(data.total$urban==0)] <- "Rural"
data.total$urban[which(data.total$urban==1)] <- "Urban"

conform <- function(complete,incomplete){
  return(
    pmax(
      incomplete[
        match(
          rownames(complete)
          ,rownames(incomplete)
        )
        ,
        match(
          colnames(complete)
          ,colnames(incomplete)
        )
        ]
      ,0
      ,na.rm=TRUE
    )
  )
}

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
  estimate.point <- props*pop
  high.end <- (props+(2*corrected.SEs))*pop
  high.end <- pmin(high.end,pop)
  return(
    list(
      low = low.end
      ,estimate = estimate.point
      ,high = high.end
    )
  )
}

countryMeta <- read.csv("headcounts.csv",as.is=TRUE)

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
  under5 <- subset(dat,age<5)
  over5 <- subset(dat,age>=5)
  under15 <- subset(dat,age<15)
  over15 <- subset(dat,age>=15)
  over25 <- subset(dat,age>=25)
  bet15and25 <- subset(dat,age>=15 & age<25)
  women <- subset(dat,sex=="Female")
  men <- subset(dat,sex=="Male")
  if(nrow(dat)>0){
    this.pop <- subset(countryMeta,filename==this.filename)$pop.total
    this.pop.under5.male <- subset(countryMeta,filename==this.filename)$male.under5
    this.pop.under5.female <- subset(countryMeta,filename==this.filename)$female.under5
    this.pop.under5 <- this.pop.under5.female + this.pop.under5.male
    this.pop.over5 <- this.pop - this.pop.under5
    this.pop.under15.male <- this.pop.under5.male + subset(countryMeta,filename==this.filename)$male.5.14
    this.pop.under15.female <- this.pop.under5.female + subset(countryMeta,filename==this.filename)$female.5.14
    this.pop.under15 <- this.pop.under15.male + this.pop.under15.female
    this.pop.over15 <- this.pop - this.pop.under15
    this.pop.male <- subset(countryMeta,filename==this.filename)$pop.male
    this.pop.female <- subset(countryMeta,filename==this.filename)$pop.female
    this.pop.over15.male <- this.pop.male - this.pop.under15.male
    this.pop.over15.female <- this.pop.female - this.pop.under15.female
    this.pop.over25.male <- subset(countryMeta,filename==this.filename)$male.25.plus
    this.pop.over25.female <- subset(countryMeta,filename==this.filename)$female.25.plus
    this.pop.over25 <- this.pop.over25.male + this.pop.over25.female
    this.pop.bet15and25 <- this.pop.over15 - this.pop.over25
    #Educ-P20
    if(length(bet15and25$educ[which(!is.na(bet15and25$educ))])!=0){
      confidence.tab <- pop.confidence(bet15and25$educ,bet15and25$p20,bet15and25$weights,this.pop.bet15and25)
      if(is.null(crossTabs[["bet15and25.educ.p20"]])){
        crossTabs[["bet15and25.educ.p20"]] <- confidence.tab
      }else{
        crossTabs[["bet15and25.educ.p20"]]$low <- crossTabs[["bet15and25.educ.p20"]]$low + conform(crossTabs[["bet15and25.educ.p20"]]$low,confidence.tab$low)
        crossTabs[["bet15and25.educ.p20"]]$estimate <- crossTabs[["bet15and25.educ.p20"]]$estimate + conform(crossTabs[["bet15and25.educ.p20"]]$estimate,confidence.tab$estimate)
        crossTabs[["bet15and25.educ.p20"]]$high <- crossTabs[["bet15and25.educ.p20"]]$high + conform(crossTabs[["bet15and25.educ.p20"]]$high,confidence.tab$high)
      }  
    }
    if(length(over25$educ[which(!is.na(over25$educ))])!=0){
      confidence.tab <- pop.confidence(over25$educ,over25$p20,over25$weights,this.pop.over25)
      if(is.null(crossTabs[["over25.educ.p20"]])){
        crossTabs[["over25.educ.p20"]] <- confidence.tab
      }else{
        crossTabs[["over25.educ.p20"]]$low <- crossTabs[["over25.educ.p20"]]$low + conform(crossTabs[["over25.educ.p20"]]$low,confidence.tab$low)
        crossTabs[["over25.educ.p20"]]$estimate <- crossTabs[["over25.educ.p20"]]$estimate + conform(crossTabs[["over25.educ.p20"]]$estimate,confidence.tab$estimate)
        crossTabs[["over25.educ.p20"]]$high <- crossTabs[["over25.educ.p20"]]$high + conform(crossTabs[["over25.educ.p20"]]$high,confidence.tab$high)
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

saveWorkbook(wb, "DHS_and_MICS_P20_crosstabs_educ.xlsx", overwrite = TRUE)