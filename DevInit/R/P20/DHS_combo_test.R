library(foreign)
library(Hmisc)

setwd("~/Downloads/UGPR60DT")
ug <- read.dta("UGPR60FL.dta")
setwd("~/Downloads/GHPR70DT")
gh <- read.dta("GHPR70FL.DTA")
setwd("~/Downloads/TZPR63DT")
tz <- read.dta("TZPR63FL.DTA")

common <- intersect(intersect(names(ug),names(gh)),names(tz))

ug <- ug[common]
ug$country <- "ug"
i <- sapply(ug, is.factor)
ug[i] <- lapply(ug[i], as.character)
gh <- gh[common]
gh$country <- "gh"
i <- sapply(gh, is.factor)
gh[i] <- lapply(gh[i], as.character)
tz <- tz[common]
tz$country <- "tz"
i <- sapply(tz, is.factor)
tz[i] <- lapply(tz[i], as.character)

# df <- rbind(ug,gh,tz)
# i <- sapply(df, is.character)
# df[i] <- lapply(df[i], as.factor)

names(df)[which(names(df)=="hv101")] <- "relationship"
names(df)[which(names(df)=="hv104")] <- "gender"
names(df)[which(names(df)=="hv105")] <- "age"
names(df)[which(names(df)=="hv106")] <- "educ"

recodeAge <- function(x){
  if(is.na(x)){
    return(NA)
  }else if(x>95){
    return(NA)
  }else{
    return(x)
  }
}
df$age <- sapply(df$age,recodeAge)
#Recoding education is more complicated than this...
recodeEduc <- function(x){
  if(is.na(x)){
    return(NA)
  }else if(x==8 | x==9){
    return(NA)
  }else if(x=="don't know"){
    return(NA)
  }else if(x=="higher"){
    return(3)
  }else if(x=="no education, preschool"){
    return(0)
  }else if(x=="primary"){
    return(1)
  }else if(x=="secondary"){
    return(2)
  }else{
    return(x)
  }
}
df$educ <- sapply(df$educ,recodeEduc)

describe(df$relationship)
describe(df$age)
describe(df$educ)

codeAgeCat <- function(x){
  startAge <- 0
  ageDiff <- 4
  endAge <- 4
  if(is.na(x)){
    return("missing")
  }
  while(endAge<95){
    endAge <- startAge+ageDiff
    if(x>=startAge & x<=endAge){
      return(
        paste0(startAge,"-",endAge)  
      )
    }
    startAge <- endAge + 1
  }
  return("missing")
}

df$ageCategory <- vapply(df$age,codeAgeCat,character(1))
df$ageCategory <- factor(df$ageCategory,
                         levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34"
                                    ,"35-39","40-44","45-49","50-54","55-59","60-64"
                                    ,"65-69","70-74","75-79","80-84","85-89","90-94"
                                    ,"95+","missing")                          
)

educ.age.tab <- table(df$ageCategory,df$educ)
educ.age.tab
