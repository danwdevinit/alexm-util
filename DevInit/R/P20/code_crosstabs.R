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
load("total_crosstabs.RData")

recode.urban <- function(x){
  if(is.na(x)){return(NA)}
  else if(x==0 | tolower(x)=="rural"){return(0)}
  else if(x==1 | tolower(x)=="urban"){return(1)}
  else{return(NA)}
}
data.total$urban <- sapply(data.total$urban.rural,recode.urban)

recode.educ <- function(x){
  if(is.na(x)){return(NA)}
  else if(tolower(x)=="dk" | tolower(x)=="don't know"){return(NA)}
  else if(x==0 | tolower(x)=="no education, preschool"){return("No education, preschool")}
  else if(x==1 | tolower(x)=="primary"){return("Primary")}
  else if(x==2 | tolower(x)=="secondary"){return("Secondary")}
  else if(x==3 | tolower(x)=="higher"){return("Higher")}
  else{return(NA)}
}
data.total$educ <- sapply(data.total$educ,recode.educ)
data.total$educ <- factor(data.total$educ
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

data.total$ageCategory <- vapply(data.total$age,codeAgeCat,character(1))
data.total$ageCategory <- factor(data.total$ageCategory,
                         levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34"
                                    ,"35-39","40-44","45-49","50-54","55-59","60-64"
                                    ,"65-69","70-74","75-79","80-84","85-89","90-94"
                                    ,"95+","missing")                          
)

data.total$head.ageCategory <- vapply(data.total$head.age,codeAgeCat,character(1))
data.total$head.ageCategory <- factor(data.total$head.ageCategory,
                                 levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34"
                                            ,"35-39","40-44","45-49","50-54","55-59","60-64"
                                            ,"65-69","70-74","75-79","80-84","85-89","90-94"
                                            ,"95+","missing")                          
)

sex.missing = c(NA,"missing",9)
sex.male = c(1,"male","masculin","hombre")
sex.female = c(2, "female","feminin","mujer")
data.total$sex[which(tolower(data.total$sex) %in% sex.missing)] <- NA
data.total$sex[which(tolower(data.total$sex) %in% sex.male)] <- "Male"
data.total$sex[which(tolower(data.total$sex) %in% sex.female)] <- "Female"
data.total$head.sex[which(tolower(data.total$head.sex) %in% sex.missing)] <- NA
data.total$head.sex[which(tolower(data.total$head.sex) %in% sex.male)] <- "Male"
data.total$head.sex[which(tolower(data.total$head.sex) %in% sex.female)] <- "Female"

#0 - neither certificate or registered
#1 - has certificate
#2 - registered
#8 - dk
birth.cert.missing <- c(NA,"dk","don't know",8,9,"missing","nsp","manquant","no sabe")
birth.cert.no <- c("registered",0,2,"neither certificate or registered","no","non","has only hospital card")
birth.cert.yes <- setdiff(unique(tolower(data.total$birth.cert)),c(birth.cert.no,birth.cert.missing))

birth.reg.missing <- c(NA,"dk","missing","nsp","manquant")
birth.reg.no <- c("no","non")
birth.reg.yes <- c("yes","oui","sí")
#count registrations if birth.cert var reveals it to be so
birth.cert.registered <- c(2,"registered","has only hospital card",birth.cert.yes)
birth.cert.not.registered <- c(0,"neither certificate or registered","no","non")
data.total$birth.reg.coded <- unfactor(data.total$birth.reg)
data.total$birth.reg.coded[which(is.na(data.total$birth.reg.coded) & tolower(data.total$birth.cert) %in% birth.cert.registered)] <- "Yes"
data.total$birth.reg.coded[which(is.na(data.total$birth.reg.coded) & tolower(data.total$birth.cert) %in% birth.cert.not.registered)] <- "No"
data.total$birth.reg.coded[which(is.na(data.total$birth.reg.coded) & grepl("visto",data.total$birth.cert))] <- "Yes"

data.total$birth.reg.coded[which(tolower(data.total$birth.reg.coded) %in% birth.reg.missing)] <- NA
data.total$birth.reg.coded[which(tolower(data.total$birth.reg.coded) %in% birth.reg.no)] <- 0
data.total$birth.reg.coded[which(tolower(data.total$birth.reg.coded) %in% birth.reg.yes)] <- 1
data.total$birth.reg.coded[which(substr(data.total$birth.reg.coded,1,1)=="S")] <- 1

data.total$birth.reg <- data.total$birth.reg.coded

data.total$birth.cert <- unfactor(data.total$birth.cert)
data.total$birth.cert[which(tolower(data.total$birth.cert) %in% birth.cert.missing)] <- NA
data.total$birth.cert[which(tolower(data.total$birth.cert) %in% birth.cert.no)] <- 0
data.total$birth.cert[which(tolower(data.total$birth.cert) %in% birth.cert.yes)] <- 1
data.total$birth.cert[which(grepl("visto",data.total$birth.cert))] <- 1

data.total$woman.bmi[which(data.total$woman.bmi>80)] <- NA
data.total$woman.bmi.class <- NA
data.total$woman.bmi.class[which(data.total$woman.bmi<16)] <- "Severe thinness"
data.total$woman.bmi.class[which(data.total$woman.bmi>=16 & data.total$woman.bmi<17)] <- "Moderate thinness"
data.total$woman.bmi.class[which(data.total$woman.bmi>=17 & data.total$woman.bmi<18.5)] <- "Mild thinness"
data.total$woman.bmi.class[which(data.total$woman.bmi>=18.5 & data.total$woman.bmi<25)] <- "Normal range"
data.total$woman.bmi.class[which(data.total$woman.bmi>=25 & data.total$woman.bmi<30)] <- "Pre-obese"
data.total$woman.bmi.class[which(data.total$woman.bmi>=30 & data.total$woman.bmi<35)] <- "Obese class I"
data.total$woman.bmi.class[which(data.total$woman.bmi>=35 & data.total$woman.bmi<40)] <- "Obese class II"
data.total$woman.bmi.class[which(data.total$woman.bmi>=40)] <- "Obese class III"

data.total$woman.bmi.class <- factor(data.total$woman.bmi.class
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

data.total$man.bmi[which(data.total$man.bmi>80)] <- NA
data.total$man.bmi.class <- NA
data.total$man.bmi.class[which(data.total$man.bmi<16)] <- "Severe thinness"
data.total$man.bmi.class[which(data.total$man.bmi>=16 & data.total$man.bmi<17)] <- "Moderate thinness"
data.total$man.bmi.class[which(data.total$man.bmi>=17 & data.total$man.bmi<18.5)] <- "Mild thinness"
data.total$man.bmi.class[which(data.total$man.bmi>=18.5 & data.total$man.bmi<25)] <- "Normal range"
data.total$man.bmi.class[which(data.total$man.bmi>=25 & data.total$man.bmi<30)] <- "Pre-obese"
data.total$man.bmi.class[which(data.total$man.bmi>=30 & data.total$man.bmi<35)] <- "Obese class I"
data.total$man.bmi.class[which(data.total$man.bmi>=35 & data.total$man.bmi<40)] <- "Obese class II"
data.total$man.bmi.class[which(data.total$man.bmi>=40)] <- "Obese class III"

data.total$man.bmi.class <- factor(data.total$man.bmi.class
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

data.total$mother.bmi[which(data.total$mother.bmi>80)] <- NA
data.total$mother.bmi.class <- NA
data.total$mother.bmi.class[which(data.total$mother.bmi<16)] <- "Severe thinness"
data.total$mother.bmi.class[which(data.total$mother.bmi>=16 & data.total$mother.bmi<17)] <- "Moderate thinness"
data.total$mother.bmi.class[which(data.total$mother.bmi>=17 & data.total$mother.bmi<18.5)] <- "Mild thinness"
data.total$mother.bmi.class[which(data.total$mother.bmi>=18.5 & data.total$mother.bmi<25)] <- "Normal range"
data.total$mother.bmi.class[which(data.total$mother.bmi>=25 & data.total$mother.bmi<30)] <- "Pre-obese"
data.total$mother.bmi.class[which(data.total$mother.bmi>=30 & data.total$mother.bmi<35)] <- "Obese class I"
data.total$mother.bmi.class[which(data.total$mother.bmi>=35 & data.total$mother.bmi<40)] <- "Obese class II"
data.total$mother.bmi.class[which(data.total$mother.bmi>=40)] <- "Obese class III"

data.total$mother.bmi.class <- factor(data.total$mother.bmi.class
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

data.total$child.height.age[which(data.total$child.height.age>80)] <- NA
data.total$stunting <- NA
# data.total$stunting[which(data.total$child.height.age<= (-6))] <- "Implausibly low"
data.total$stunting[which(data.total$child.height.age > (-6) & data.total$child.height.age<= (-3))] <- "Severely stunted"
data.total$stunting[which(data.total$child.height.age > (-3) & data.total$child.height.age<= (-2))] <- "Stunted, but not severely"
data.total$stunting[which(data.total$child.height.age > (-2) & data.total$child.height.age< (6))] <- "Not stunted"
# data.total$stunting[which(data.total$child.height.age>= (6))] <- "Implausibly high"

data.total$stunting <- factor(data.total$stunting
                              ,levels=c(
                                "Implausibly low"
                                ,"Severely stunted"
                                ,"Stunted, but not severely"
                                ,"Not stunted"
                                ,"Implausibly high"
                              ))

# data.total$p20[which(data.total$p20==TRUE)] <- "P20"
# data.total$p20[which(data.total$p20==FALSE)] <- "P80"

save(data.total,file="total_crosstabs_coded.RData")

# crossTabs <- list()
# crossTabs[["urbanp20"]] <- crosstab(data.total$urban,data.total$p20,weight=data.total$weights)$tab
# crossTabs[["educp20"]] <- crosstab(data.total$educ,data.total$p20,weight=data.total$weights)$tab
# crossTabs[["agep20"]] <- crosstab(data.total$ageCategory,data.total$p20,weight=data.total$weights)$tab
# crossTabs[["sexp20"]] <- crosstab(data.total$sex,data.total$p20,weight=data.total$weights)$tab
# crossTabs[["head.agep20"]] <- crosstab(data.total$head.ageCategory,data.total$p20,weight=data.total$weights)$tab
# crossTabs[["head.sexp20"]] <- crosstab(data.total$head.sex,data.total$p20,weight=data.total$weights)$tab
# under5 <- subset(data.total,age<5)
# over5 <- subset(data.total,age>=5)
# under15 <- subset(data.total,age<15)
# over15 <- subset(data.total,age>=15)
# women <- subset(data.total,sex=="Female")
# men <- subset(data.total,sex=="Male")
# crossTabs[["under5.certp20"]] <- crosstab(under5$birth.cert,under5$p20,weight=under5$child.weights)$tab
# crossTabs[["under5.regp20"]] <- crosstab(under5$birth.reg,under5$p20,weight=under5$child.weights)$tab
# crossTabs[["over5.certp20"]] <- crosstab(over5$birth.cert,over5$p20,weight=over5$weights)$tab
# crossTabs[["under5.nutritionp20"]] <- crosstab(under5$stunting,under5$p20,weight=under5$child.weights)$tab
# crossTabs[["under15.urbanp20"]] <- crosstab(under15$urban,under15$p20,weight=under15$weights)$tab
# crossTabs[["under15.sexp20"]] <- crosstab(under15$sex,under15$p20,weight=under15$weights)$tab
# crossTabs[["over15.urbanp20"]] <- crosstab(under15$urban,under15$p20,weight=under15$weights)$tab
# crossTabs[["over15.sexp20"]] <- crosstab(under15$sex,under15$p20,weight=under15$weights)$tab
# crossTabs[["women.bmip20"]] <- crosstab(women$woman.bmi.class,women$p20,weight=women$weights)$tab
# crossTabs[["men.bmip20"]] <- crosstab(men$man.bmi.class,men$p20,weight=men$weights)$tab
# 
# library(openxlsx)
# 
# #Create workbook
# wb <- createWorkbook("crosstabs")
# 
# crossNames <- names(crossTabs)
# for(i in 1:length(crossNames)){
#   crossName <- crossNames[i]
#   crossTab <- crossTabs[[i]]
#   addWorksheet(wb,crossName)
#   writeData(wb,sheet=crossName,crossTab,colNames=TRUE,rowNames=TRUE)
# }
# 
# saveWorkbook(wb, "DHS_and_MICS_P20_crosstabs_weighted.xlsx", overwrite = TRUE)