####Function and setup####
library(Hmisc)
library(plyr)
library(foreign)
library(data.table)

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

dir <- "D:/Documents/Data/DHSauto/iahr52dt"

####Run function####
# set our working directory, change this if using on another machine
wd <- "D:/Documents/Data/DHSauto/"
setwd(wd)

# Pull some coded info out of the dir name
country <- tolower(substr(basename(dir),1,2))
recode <- tolower(substr(basename(dir),3,4))
phase <- as.integer(substr(basename(dir),5,5))
# For this analysis, we're only interested in individual member recodes, or "hr"
message(basename(dir))
hrwd <- dir
if(!file_test(op="-d", hrwd)){next;}

hrBase <- basename(hrwd)
iso2 <- toupper(substr(hrBase,1,2))
phase <- substr(hrBase,5,6)

prwd <- paste0("D:/Documents/Data/DHSauto/",tolower(iso2),"pr",phase,"dt/")
if(!file_test(op="-d", prwd)){next;}

pr <- read.csv(paste0(prwd,iso2,"PR",phase,"FL.csv")
               ,na.strings="",as.is=TRUE,check.names=FALSE)

names(pr)[which(names(pr)=="hv271")] <- "wealth"
pr$wealth <- pr$wealth/100000

names(pr)[which(names(pr)=="hv245")] <- "hectares"
names(pr)[which(names(pr)=="sh44")] <- "religion"
names(pr)[which(names(pr)=="sh45")] <- "casteortribe"
names(pr)[which(names(pr)=="sh46")] <- "castetype"

pr$castetype[which(pr$casteortribe=="no caste/tribe")] <- "no caste/tribe"
pr$castetype[which(pr$castetype=="don't know")] <- NA

#Rename sample.weights var
names(pr)[which(names(pr)=="hv005")] <- "sample.weights"
pr$weights <- pr$sample.weights/1000000

#Rename urban var
names(pr)[which(names(pr)=="hv025")] <- "urban.rural"

#Rename educ var
names(pr)[which(names(pr)=="hv109")] <- "educ"
recode.educ <- function(x){
  if(is.na(x)){return(NA)}
  else if(tolower(x)=="dk" | tolower(x)=="don't know" | tolower(x)=="missing" | x==8 | x==9){return(NA)}
  else if(x==0 | x==1 | tolower(x)=="no education, preschool" | tolower(x)=="no education" | tolower(x)=="incomplete primary"){return("No education, preschool")}
  else if(x==2 | x==3 | tolower(x)=="complete primary" | tolower(x)=="incomplete secondary"){return("Primary")}
  else if(x==4 | tolower(x)=="complete secondary"){return("Secondary")}
  else if(x==5 | tolower(x)=="higher"){return("Higher")}
  else{return(NA)}
}
pr$educ <- sapply(pr$educ,recode.educ)

#Rename age var
names(pr)[which(names(pr)=="hv105")] <- "age"

#Rename sex var
names(pr)[which(names(pr)=="hv104")] <- "sex"

#Rename cluster/hh var
names(pr)[which(names(pr)=="hv001")] <- "cluster"
names(pr)[which(names(pr)=="hv002")] <- "household"
names(pr)[which(names(pr)=="hvidx")] <- "line"
names(pr)[which(names(pr)=="hv112")] <- "mother.line"
pr$mother.line[which(pr$mother.line==99)] <- NA

#Head vars
names(pr)[which(names(pr)=="hv219")] <- "head.sex"
names(pr)[which(names(pr)=="hv220")] <- "head.age"

#reg?
names(pr)[which(names(pr)=="hv140")] <- "birth.cert"

#nutrition
names(pr)[which(names(pr)=="ha40")] <- "woman.bmi"
if(typeof(pr$woman.bmi)!="NULL"){
  pr$woman.bmi <- pr$woman.bmi/100 
}else{
  pr$woman.bmi <- NA
}
names(pr)[which(names(pr)=="hb40")] <- "man.bmi"
if(typeof(pr$man.bmi)!="NULL"){
  pr$man.bmi <- pr$man.bmi/100 
}else{
  pr$man.bmi <- NA
}
names(pr)[which(names(pr)=="hc1")] <- "age.months"
names(pr)[which(names(pr)=="hc2")] <- "weight.kg"
names(pr)[which(names(pr)=="hc3")] <- "height.cm"
names(pr)[which(names(pr)=="hc15")] <- "standing.lying"
names(pr)[which(names(pr)=="hc5")] <- "child.height.age"
if(typeof(pr$child.height.age)=="NULL"){
  pr$child.height.age <- NA
}else{
  pr$child.height.age <- pr$child.height.age/100
}
pr$child.weights <- pr$weights

povcalcut <- subset(povcalcuts,filename==hrBase)$hc
povcalperc <- weighted.percentile(pr$wealth,pr$weights,prob=povcalcut)

pr$p20 <- (pr$wealth < povcalperc)

mothers <- unique(pr[c("cluster","household","line","woman.bmi")])
mothers <- mothers[complete.cases(mothers),]
names(mothers) <- c("cluster","household","mother.line","mother.bmi")
pr <- join(
  pr
  ,mothers
  ,by=c("cluster","household","mother.line")
)

keep <- c("wealth","weights","urban.rural","educ","age","sex","cluster","household","head.sex","head.age","p20"
          ,"birth.cert","birth.reg","age.months","weight.kg","height.cm","standing.lying","child.height.age"
          ,"woman.bmi","man.bmi","child.weights","mother.bmi","castetype"
)
prNames <- names(pr)
namesDiff <- setdiff(keep,prNames)
if(length(namesDiff)>0){
  for(y in 1:length(namesDiff)){
    pr[namesDiff[y]] <- NA
    message(paste("Missing variable",namesDiff[y]))
  } 
}
data <- pr[keep]
data$filename <- hrBase

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

recode.urban <- function(x){
  if(is.na(x)){return(NA)}
  else if(x==0 | tolower(x)=="rural"){return(0)}
  else if(x==1 | tolower(x)=="urban"){return(1)}
  else{return(NA)}
}
data$urban <- sapply(data$urban.rural,recode.urban)

recode.educ <- function(x){
  if(is.na(x)){return(NA)}
  else if(tolower(x)=="dk" | tolower(x)=="don't know"){return(NA)}
  else if(x==0 | tolower(x)=="no education, preschool"){return("No education, preschool")}
  else if(x==1 | tolower(x)=="primary"){return("Primary")}
  else if(x==2 | tolower(x)=="secondary"){return("Secondary")}
  else if(x==3 | tolower(x)=="higher"){return("Higher")}
  else{return(NA)}
}
data$educ <- sapply(data$educ,recode.educ)
data$educ <- factor(data$educ
                          ,levels = c("No education, preschool","Primary","Secondary","Higher")
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

data$ageCategory <- vapply(data$age,codeAgeCat,character(1))
data$ageCategory <- factor(data$ageCategory,
                                 levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34"
                                            ,"35-39","40-44","45-49","50-54","55-59","60-64"
                                            ,"65-69","70-74","75-79","80-84","85-89","90-94"
                                            ,"95+","missing")                          
)

data$head.ageCategory <- vapply(data$head.age,codeAgeCat,character(1))
data$head.ageCategory <- factor(data$head.ageCategory,
                                      levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34"
                                                 ,"35-39","40-44","45-49","50-54","55-59","60-64"
                                                 ,"65-69","70-74","75-79","80-84","85-89","90-94"
                                                 ,"95+","missing")                          
)

sex.missing = c(NA,"missing",9)
sex.male = c(1,"male","masculin","hombre")
sex.female = c(2, "female","feminin","mujer")
data$sex[which(tolower(data$sex) %in% sex.missing)] <- NA
data$sex[which(tolower(data$sex) %in% sex.male)] <- "Male"
data$sex[which(tolower(data$sex) %in% sex.female)] <- "Female"
data$head.sex[which(tolower(data$head.sex) %in% sex.missing)] <- NA
data$head.sex[which(tolower(data$head.sex) %in% sex.male)] <- "Male"
data$head.sex[which(tolower(data$head.sex) %in% sex.female)] <- "Female"

#0 - neither certificate or registered
#1 - has certificate
#2 - registered
#8 - dk
birth.cert.missing <- c(NA,"dk","don't know",8,9,"missing","nsp","manquant","no sabe")
birth.cert.no <- c("registered",0,2,"neither certificate or registered","no","non","has only hospital card")
birth.cert.yes <- setdiff(unique(tolower(data$birth.cert)),c(birth.cert.no,birth.cert.missing))

data$birth.cert[which(tolower(data$birth.cert) %in% birth.cert.missing)] <- NA
data$birth.cert[which(tolower(data$birth.cert) %in% birth.cert.no)] <- 0
data$birth.cert[which(tolower(data$birth.cert) %in% birth.cert.yes)] <- 1
data$birth.cert[which(grepl("visto",data$birth.cert))] <- 1

data$woman.bmi[which(data$woman.bmi>80)] <- NA
data$woman.bmi.class <- NA
data$woman.bmi.class[which(data$woman.bmi<16)] <- "Severe thinness"
data$woman.bmi.class[which(data$woman.bmi>=16 & data$woman.bmi<17)] <- "Moderate thinness"
data$woman.bmi.class[which(data$woman.bmi>=17 & data$woman.bmi<18.5)] <- "Mild thinness"
data$woman.bmi.class[which(data$woman.bmi>=18.5 & data$woman.bmi<25)] <- "Normal range"
data$woman.bmi.class[which(data$woman.bmi>=25 & data$woman.bmi<30)] <- "Pre-obese"
data$woman.bmi.class[which(data$woman.bmi>=30 & data$woman.bmi<35)] <- "Obese class I"
data$woman.bmi.class[which(data$woman.bmi>=35 & data$woman.bmi<40)] <- "Obese class II"
data$woman.bmi.class[which(data$woman.bmi>=40)] <- "Obese class III"

data$woman.bmi.class <- factor(data$woman.bmi.class
                                     ,levels=c(
                                       "Severe thinness"
                                       ,"Moderate thinness"
                                       ,"Mild thinness"
                                       ,"Normal range"
                                       ,"Pre-obese"
                                       ,"Obese class I"
                                       ,"Obese class II"
                                       ,"Obese class III"
                                     ))

data$man.bmi[which(data$man.bmi>80)] <- NA
data$man.bmi.class <- NA
data$man.bmi.class[which(data$man.bmi<16)] <- "Severe thinness"
data$man.bmi.class[which(data$man.bmi>=16 & data$man.bmi<17)] <- "Moderate thinness"
data$man.bmi.class[which(data$man.bmi>=17 & data$man.bmi<18.5)] <- "Mild thinness"
data$man.bmi.class[which(data$man.bmi>=18.5 & data$man.bmi<25)] <- "Normal range"
data$man.bmi.class[which(data$man.bmi>=25 & data$man.bmi<30)] <- "Pre-obese"
data$man.bmi.class[which(data$man.bmi>=30 & data$man.bmi<35)] <- "Obese class I"
data$man.bmi.class[which(data$man.bmi>=35 & data$man.bmi<40)] <- "Obese class II"
data$man.bmi.class[which(data$man.bmi>=40)] <- "Obese class III"

data$man.bmi.class <- factor(data$man.bmi.class
                                   ,levels=c(
                                     "Severe thinness"
                                     ,"Moderate thinness"
                                     ,"Mild thinness"
                                     ,"Normal range"
                                     ,"Pre-obese"
                                     ,"Obese class I"
                                     ,"Obese class II"
                                     ,"Obese class III"
                                   ))

data$mother.bmi[which(data$mother.bmi>80)] <- NA
data$mother.bmi.class <- NA
data$mother.bmi.class[which(data$mother.bmi<16)] <- "Severe thinness"
data$mother.bmi.class[which(data$mother.bmi>=16 & data$mother.bmi<17)] <- "Moderate thinness"
data$mother.bmi.class[which(data$mother.bmi>=17 & data$mother.bmi<18.5)] <- "Mild thinness"
data$mother.bmi.class[which(data$mother.bmi>=18.5 & data$mother.bmi<25)] <- "Normal range"
data$mother.bmi.class[which(data$mother.bmi>=25 & data$mother.bmi<30)] <- "Pre-obese"
data$mother.bmi.class[which(data$mother.bmi>=30 & data$mother.bmi<35)] <- "Obese class I"
data$mother.bmi.class[which(data$mother.bmi>=35 & data$mother.bmi<40)] <- "Obese class II"
data$mother.bmi.class[which(data$mother.bmi>=40)] <- "Obese class III"

data$mother.bmi.class <- factor(data$mother.bmi.class
                                      ,levels=c(
                                        "Severe thinness"
                                        ,"Moderate thinness"
                                        ,"Mild thinness"
                                        ,"Normal range"
                                        ,"Pre-obese"
                                        ,"Obese class I"
                                        ,"Obese class II"
                                        ,"Obese class III"
                                      ))

data$child.height.age[which(data$child.height.age>80)] <- NA
data$stunting <- NA
# data$stunting[which(data$child.height.age<= (-6))] <- "Implausibly low"
data$stunting[which(data$child.height.age > (-6) & data$child.height.age<= (-3))] <- "Severely stunted"
data$stunting[which(data$child.height.age > (-3) & data$child.height.age<= (-2))] <- "Stunted, but not severely"
data$stunting[which(data$child.height.age > (-2) & data$child.height.age< (6))] <- "Not stunted"
# data$stunting[which(data$child.height.age>= (6))] <- "Implausibly high"

data$stunting <- factor(data$stunting
                              ,levels=c(
                                "Implausibly low"
                                ,"Severely stunted"
                                ,"Stunted, but not severely"
                                ,"Not stunted"
                                ,"Implausibly high"
                              ))

save(data,file="india.data.RData")
# load("india.data.Rdata")

data$sex <- factor(data$sex,levels=c("Male","Female"))
data$castetype <- factor(data$castetype,levels=
  c(
    "scheduled caste"
    ,"scheduled tribe"
    ,"other backward class"
    ,"no caste/tribe"
    ,"none of above"
    )
  )

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
this.filename <- "iahr52dt"
message(this.filename)
dat <- data
surveyed.pop <- nrow(dat)
surveyed.households <- length(unique(dat$household))
under5 <- subset(dat,age<5)
over5 <- subset(dat,age>=5)
under15 <- subset(dat,age<15)
over15 <- subset(dat,age>=15)
over25 <- subset(dat,age>=25)
women <- subset(dat,sex=="Female")
men <- subset(dat,sex=="Male")

this.pop <- subset(countryMeta,filename==this.filename)$pop.total
this.pop.under5 <- subset(countryMeta,filename==this.filename)$female.under5 + subset(countryMeta,filename==this.filename)$male.under5
this.pop.over5 <- this.pop - this.pop.under5
this.pop.under15 <- this.pop.under5 + subset(countryMeta,filename==this.filename)$female.5.14 +
  subset(countryMeta,filename==this.filename)$male.5.14
this.pop.over15 <- this.pop - this.pop.under15
this.pop.female <- subset(countryMeta,filename==this.filename)$pop.female
this.pop.male <- subset(countryMeta,filename==this.filename)$pop.male
this.pop.over25.male <- subset(countryMeta,filename==this.filename)$male.25.plus
this.pop.over25.female <- subset(countryMeta,filename==this.filename)$female.25.plus
this.pop.over25 <- this.pop.over25.male + this.pop.over25.female

crossTabs <- list()

crossTabs[["castep20"]] <- pop.confidence(dat$castetype,dat$p20,dat$weights,this.pop)
crossTabs[["over25.casteeduc"]] <- pop.confidence(over25$educ,over25$castetype,over25$weights,this.pop.over25)
crossTabs[["under5.castestunting"]] <- pop.confidence(under5$stunting,under5$castetype,under5$weights,this.pop.under5)
crossTabs[["under5.castecert"]] <- pop.confidence(under5$birth.cert,under5$castetype,under5$weights,this.pop.under5)


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

saveWorkbook(wb, "Castetabs.xlsx", overwrite = TRUE)
