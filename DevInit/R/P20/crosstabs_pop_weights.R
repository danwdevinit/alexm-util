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
      ,]
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
    #Urban-P20
    if(length(dat$urban[which(!is.na(dat$urban))])!=0){
      confidence.tab <- pop.confidence(dat$urban,dat$p20,dat$weights,this.pop)
      if(is.null(crossTabs[["urbanp20"]])){
        crossTabs[["urbanp20"]] <- confidence.tab
      }else{
        crossTabs[["urbanp20"]]$low <- crossTabs[["urbanp20"]]$low + conform(crossTabs[["urbanp20"]]$low,confidence.tab$low)
        crossTabs[["urbanp20"]]$estimate <- crossTabs[["urbanp20"]]$estimate + conform(crossTabs[["urbanp20"]]$estimate,confidence.tab$estimate)
        crossTabs[["urbanp20"]]$high <- crossTabs[["urbanp20"]]$high + conform(crossTabs[["urbanp20"]]$high,confidence.tab$high)
      }  
    }
    #Educ-P20
    if(length(over15$educ[which(!is.na(over15$educ))])!=0){
      confidence.tab <- pop.confidence(over15$educ,over15$p20,over15$weights,this.pop.over15)
      if(is.null(crossTabs[["over15.educp20"]])){
        crossTabs[["over15.educp20"]] <- confidence.tab
      }else{
        crossTabs[["over15.educp20"]]$low <- crossTabs[["over15.educp20"]]$low + conform(crossTabs[["over15.educp20"]]$low,confidence.tab$low)
        crossTabs[["over15.educp20"]]$estimate <- crossTabs[["over15.educp20"]]$estimate + conform(crossTabs[["over15.educp20"]]$estimate,confidence.tab$estimate)
        crossTabs[["over15.educp20"]]$high <- crossTabs[["over15.educp20"]]$high + conform(crossTabs[["over15.educp20"]]$high,confidence.tab$high)
      }  
    }
    #Age-P20
    if(length(dat$ageCategory[which(!is.na(dat$ageCategory))])!=0){
      confidence.tab <- pop.confidence(dat$ageCategory,dat$p20,dat$weights,this.pop)
      if(is.null(crossTabs[["ageCategoryp20"]])){
        crossTabs[["ageCategoryp20"]] <- confidence.tab
      }else{
        crossTabs[["ageCategoryp20"]]$low <- crossTabs[["ageCategoryp20"]]$low + conform(crossTabs[["ageCategoryp20"]]$low,confidence.tab$low)
        crossTabs[["ageCategoryp20"]]$estimate <- crossTabs[["ageCategoryp20"]]$estimate + conform(crossTabs[["ageCategoryp20"]]$estimate,confidence.tab$estimate)
        crossTabs[["ageCategoryp20"]]$high <- crossTabs[["ageCategoryp20"]]$high + conform(crossTabs[["ageCategoryp20"]]$high,confidence.tab$high)
      }  
    }
    #Sex-P20
    if(length(dat$sex[which(!is.na(dat$sex))])!=0){
      confidence.tab <- pop.confidence(dat$sex,dat$p20,dat$weights,this.pop)
      if(is.null(crossTabs[["sexp20"]])){
        crossTabs[["sexp20"]] <- confidence.tab
      }else{
        crossTabs[["sexp20"]]$low <- crossTabs[["sexp20"]]$low + conform(crossTabs[["sexp20"]]$low,confidence.tab$low)
        crossTabs[["sexp20"]]$estimate <- crossTabs[["sexp20"]]$estimate + conform(crossTabs[["sexp20"]]$estimate,confidence.tab$estimate)
        crossTabs[["sexp20"]]$high <- crossTabs[["sexp20"]]$high + conform(crossTabs[["sexp20"]]$high,confidence.tab$high)
      }  
    }
    #Head-age-P20
    if(length(dat$head.ageCategory[which(!is.na(dat$head.ageCategory))])!=0){
      confidence.tab <- pop.confidence(dat$head.ageCategory,dat$p20,dat$weights,this.pop)
      if(is.null(crossTabs[["head.ageCategoryp20"]])){
        crossTabs[["head.ageCategoryp20"]] <- confidence.tab
      }else{
        crossTabs[["head.ageCategoryp20"]]$low <- crossTabs[["head.ageCategoryp20"]]$low + conform(crossTabs[["head.ageCategoryp20"]]$low,confidence.tab$low)
        crossTabs[["head.ageCategoryp20"]]$estimate <- crossTabs[["head.ageCategoryp20"]]$estimate + conform(crossTabs[["head.ageCategoryp20"]]$estimate,confidence.tab$estimate)
        crossTabs[["head.ageCategoryp20"]]$high <- crossTabs[["head.ageCategoryp20"]]$high + conform(crossTabs[["head.ageCategoryp20"]]$high,confidence.tab$high)
      }  
    }
    #Head-sex-P20
    if(length(dat$head.sex[which(!is.na(dat$head.sex))])!=0){
      confidence.tab <- pop.confidence(dat$head.sex,dat$p20,dat$weights,this.pop)
      if(is.null(crossTabs[["head.sexp20"]])){
        crossTabs[["head.sexp20"]] <- confidence.tab
      }else{
        crossTabs[["head.sexp20"]]$low <- crossTabs[["head.sexp20"]]$low + conform(crossTabs[["head.sexp20"]]$low,confidence.tab$low)
        crossTabs[["head.sexp20"]]$estimate <- crossTabs[["head.sexp20"]]$estimate + conform(crossTabs[["head.sexp20"]]$estimate,confidence.tab$estimate)
        crossTabs[["head.sexp20"]]$high <- crossTabs[["head.sexp20"]]$high + conform(crossTabs[["head.sexp20"]]$high,confidence.tab$high)
      }  
    }
    #Under5 registration
    if(length(under5$birth.reg[which(!is.na(under5$birth.reg))])!=0){
      confidence.tab <- pop.confidence(under5$birth.reg,under5$p20,under5$weights,this.pop.under5)
      if(is.null(crossTabs[["under5.regp20"]])){
        crossTabs[["under5.regp20"]] <- confidence.tab
      }else{
        crossTabs[["under5.regp20"]]$low <- crossTabs[["under5.regp20"]]$low + conform(crossTabs[["under5.regp20"]]$low,confidence.tab$low)
        crossTabs[["under5.regp20"]]$estimate <- crossTabs[["under5.regp20"]]$estimate + conform(crossTabs[["under5.regp20"]]$estimate,confidence.tab$estimate)
        crossTabs[["under5.regp20"]]$high <- crossTabs[["under5.regp20"]]$high + conform(crossTabs[["under5.regp20"]]$high,confidence.tab$high)
      }  
    }
    #Under5 nutrition
    if(length(under5$stunting[which(!is.na(under5$stunting))])!=0){
      confidence.tab <- pop.confidence(under5$stunting,under5$p20,under5$weights,this.pop.under5)
      if(is.null(crossTabs[["under5.nutritionp20"]])){
        crossTabs[["under5.nutritionp20"]] <- confidence.tab
      }else{
        crossTabs[["under5.nutritionp20"]]$low <- crossTabs[["under5.nutritionp20"]]$low + conform(crossTabs[["under5.nutritionp20"]]$low,confidence.tab$low)
        crossTabs[["under5.nutritionp20"]]$estimate <- crossTabs[["under5.nutritionp20"]]$estimate + conform(crossTabs[["under5.nutritionp20"]]$estimate,confidence.tab$estimate)
        crossTabs[["under5.nutritionp20"]]$high <- crossTabs[["under5.nutritionp20"]]$high + conform(crossTabs[["under5.nutritionp20"]]$high,confidence.tab$high)
      }  
    }
#     #Under15 urban
#     if(length(under15$urban[which(!is.na(under15$urban))])!=0){
#       confidence.tab <- pop.confidence(under15$urban,under15$p20,under15$weights,this.pop.under15)
#       if(is.null(crossTabs[["under15.urbanp20"]])){
#         crossTabs[["under15.urbanp20"]] <- confidence.tab
#       }else{
#         crossTabs[["under15.urbanp20"]]$low <- crossTabs[["under15.urbanp20"]]$low + conform(crossTabs[["under15.urbanp20"]]$low,confidence.tab$low)
#         crossTabs[["under15.urbanp20"]]$estimate <- crossTabs[["under15.urbanp20"]]$estimate + conform(crossTabs[["under15.urbanp20"]]$estimate,confidence.tab$estimate)
#         crossTabs[["under15.urbanp20"]]$high <- crossTabs[["under15.urbanp20"]]$high + conform(crossTabs[["under15.urbanp20"]]$high,confidence.tab$high)
#       }  
#     }
#     #Under15 sex
#     if(length(under15$sex[which(!is.na(under15$sex))])!=0){
#       confidence.tab <- pop.confidence(under15$sex,under15$p20,under15$weights,this.pop.under15)
#       if(is.null(crossTabs[["under15.sexp20"]])){
#         crossTabs[["under15.sexp20"]] <- confidence.tab
#       }else{
#         crossTabs[["under15.sexp20"]]$low <- crossTabs[["under15.sexp20"]]$low + conform(crossTabs[["under15.sexp20"]]$low,confidence.tab$low)
#         crossTabs[["under15.sexp20"]]$estimate <- crossTabs[["under15.sexp20"]]$estimate + conform(crossTabs[["under15.sexp20"]]$estimate,confidence.tab$estimate)
#         crossTabs[["under15.sexp20"]]$high <- crossTabs[["under15.sexp20"]]$high + conform(crossTabs[["under15.sexp20"]]$high,confidence.tab$high)
#       }  
#     }
#     #Over15 urban
#     if(length(over15$urban[which(!is.na(over15$urban))])!=0){
#       confidence.tab <- pop.confidence(over15$urban,over15$p20,over15$weights,this.pop.over15)
#       if(is.null(crossTabs[["over15.urbanp20"]])){
#         crossTabs[["over15.urbanp20"]] <- confidence.tab
#       }else{
#         crossTabs[["over15.urbanp20"]]$low <- crossTabs[["over15.urbanp20"]]$low + conform(crossTabs[["over15.urbanp20"]]$low,confidence.tab$low)
#         crossTabs[["over15.urbanp20"]]$estimate <- crossTabs[["over15.urbanp20"]]$estimate + conform(crossTabs[["over15.urbanp20"]]$estimate,confidence.tab$estimate)
#         crossTabs[["over15.urbanp20"]]$high <- crossTabs[["over15.urbanp20"]]$high + conform(crossTabs[["over15.urbanp20"]]$high,confidence.tab$high)
#       }  
#     }
#     #Over15 sex
#     if(length(over15$sex[which(!is.na(over15$sex))])!=0){
#       confidence.tab <- pop.confidence(over15$sex,over15$p20,over15$weights,this.pop.over15)
#       if(is.null(crossTabs[["over15.sexp20"]])){
#         crossTabs[["over15.sexp20"]] <- confidence.tab
#       }else{
#         crossTabs[["over15.sexp20"]]$low <- crossTabs[["over15.sexp20"]]$low + conform(crossTabs[["over15.sexp20"]]$low,confidence.tab$low)
#         crossTabs[["over15.sexp20"]]$estimate <- crossTabs[["over15.sexp20"]]$estimate + conform(crossTabs[["over15.sexp20"]]$estimate,confidence.tab$estimate)
#         crossTabs[["over15.sexp20"]]$high <- crossTabs[["over15.sexp20"]]$high + conform(crossTabs[["over15.sexp20"]]$high,confidence.tab$high)
#       }  
#     }
    #Woman bmi
    if(length(women$woman.bmi.class[which(!is.na(women$woman.bmi.class))])!=0){
      confidence.tab <- pop.confidence(women$woman.bmi.class,women$p20,women$weights,this.pop.female)
      if(is.null(crossTabs[["women.bmip20"]])){
        crossTabs[["women.bmip20"]] <- confidence.tab
      }else{
        crossTabs[["women.bmip20"]]$low <- crossTabs[["women.bmip20"]]$low + conform(crossTabs[["women.bmip20"]]$low,confidence.tab$low)
        crossTabs[["women.bmip20"]]$estimate <- crossTabs[["women.bmip20"]]$estimate + conform(crossTabs[["women.bmip20"]]$estimate,confidence.tab$estimate)
        crossTabs[["women.bmip20"]]$high <- crossTabs[["women.bmip20"]]$high + conform(crossTabs[["women.bmip20"]]$high,confidence.tab$high)
      }  
    }
    #man bmi
    if(length(men$man.bmi.class[which(!is.na(men$man.bmi.class))])!=0){
      confidence.tab <- pop.confidence(men$man.bmi.class,men$p20,men$weights,this.pop.male)
      if(is.null(crossTabs[["men.bmip20"]])){
        crossTabs[["men.bmip20"]] <- confidence.tab
      }else{
        crossTabs[["men.bmip20"]]$low <- crossTabs[["men.bmip20"]]$low + conform(crossTabs[["men.bmip20"]]$low,confidence.tab$low)
        crossTabs[["men.bmip20"]]$estimate <- crossTabs[["men.bmip20"]]$estimate + conform(crossTabs[["men.bmip20"]]$estimate,confidence.tab$estimate)
        crossTabs[["men.bmip20"]]$high <- crossTabs[["men.bmip20"]]$high + conform(crossTabs[["men.bmip20"]]$high,confidence.tab$high)
      }  
    }
    #Under5 registration by gender
    under5.male <- subset(under5,sex=="Male")
    under5.female <- subset(under5,sex=="Female")
    if(length(under5.male$birth.reg[which(!is.na(under5.male$birth.reg))])!=0){
      confidence.tab <- pop.confidence(under5.male$birth.reg,under5.male$p20,under5.male$weights,this.pop.under5.male)
      if(is.null(crossTabs[["under5.male.reg.p20"]])){
        crossTabs[["under5.male.reg.p20"]] <- confidence.tab
      }else{
        crossTabs[["under5.male.reg.p20"]]$low <- crossTabs[["under5.male.reg.p20"]]$low + conform(crossTabs[["under5.male.reg.p20"]]$low,confidence.tab$low)
        crossTabs[["under5.male.reg.p20"]]$estimate <- crossTabs[["under5.male.reg.p20"]]$estimate + conform(crossTabs[["under5.male.reg.p20"]]$estimate,confidence.tab$estimate)
        crossTabs[["under5.male.reg.p20"]]$high <- crossTabs[["under5.male.reg.p20"]]$high + conform(crossTabs[["under5.male.reg.p20"]]$high,confidence.tab$high)
      }  
    }
    if(length(under5.female$birth.reg[which(!is.na(under5.female$birth.reg))])!=0){
      confidence.tab <- pop.confidence(under5.female$birth.reg,under5.female$p20,under5.female$weights,this.pop.under5.female)
      if(is.null(crossTabs[["under5.female.reg.p20"]])){
        crossTabs[["under5.female.reg.p20"]] <- confidence.tab
      }else{
        crossTabs[["under5.female.reg.p20"]]$low <- crossTabs[["under5.female.reg.p20"]]$low + conform(crossTabs[["under5.female.reg.p20"]]$low,confidence.tab$low)
        crossTabs[["under5.female.reg.p20"]]$estimate <- crossTabs[["under5.female.reg.p20"]]$estimate + conform(crossTabs[["under5.female.reg.p20"]]$estimate,confidence.tab$estimate)
        crossTabs[["under5.female.reg.p20"]]$high <- crossTabs[["under5.female.reg.p20"]]$high + conform(crossTabs[["under5.female.reg.p20"]]$high,confidence.tab$high)
      }  
    }
    #Educ-P20
    over15.male <- subset(over15,sex=="Male")
    over15.female <- subset(over15,sex=="Female")
    if(length(over15.male$educ[which(!is.na(over15.male$educ))])!=0){
      confidence.tab <- pop.confidence(over15.male$educ,over15.male$p20,over15.male$weights,this.pop.over15.male)
      if(is.null(crossTabs[["over15.male.educ.p20"]])){
        crossTabs[["over15.male.educ.p20"]] <- confidence.tab
      }else{
        crossTabs[["over15.male.educ.p20"]]$low <- crossTabs[["over15.male.educ.p20"]]$low + conform(crossTabs[["over15.male.educ.p20"]]$low,confidence.tab$low)
        crossTabs[["over15.male.educ.p20"]]$estimate <- crossTabs[["over15.male.educ.p20"]]$estimate + conform(crossTabs[["over15.male.educ.p20"]]$estimate,confidence.tab$estimate)
        crossTabs[["over15.male.educ.p20"]]$high <- crossTabs[["over15.male.educ.p20"]]$high + conform(crossTabs[["over15.male.educ.p20"]]$high,confidence.tab$high)
      }  
    }
    if(length(over15.female$educ[which(!is.na(over15.female$educ))])!=0){
      confidence.tab <- pop.confidence(over15.female$educ,over15.female$p20,over15.female$weights,this.pop.over15.female)
      if(is.null(crossTabs[["over15.female.educ.p20"]])){
        crossTabs[["over15.female.educ.p20"]] <- confidence.tab
      }else{
        crossTabs[["over15.female.educ.p20"]]$low <- crossTabs[["over15.female.educ.p20"]]$low + conform(crossTabs[["over15.female.educ.p20"]]$low,confidence.tab$low)
        crossTabs[["over15.female.educ.p20"]]$estimate <- crossTabs[["over15.female.educ.p20"]]$estimate + conform(crossTabs[["over15.female.educ.p20"]]$estimate,confidence.tab$estimate)
        crossTabs[["over15.female.educ.p20"]]$high <- crossTabs[["over15.female.educ.p20"]]$high + conform(crossTabs[["over15.female.educ.p20"]]$high,confidence.tab$high)
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