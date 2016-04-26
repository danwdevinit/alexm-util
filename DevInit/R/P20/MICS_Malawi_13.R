library(foreign)
library(plyr)
library(Hmisc)
library(ggplot2)

wd <- "D:/Documents/Data/MICS/Malawi_MICS5_Datasets/Malawi MICS 2013-14 SPSS Datasets"
setwd(wd)

df <- read.spss("hl.sav",to.data.frame=TRUE)

names(df)[which(names(df)=="HL6")] <- "age"
names(df)[which(names(df)=="ED4A")] <- "educ"

recodeEduc <- function(x){
  if(is.na(x)){
    return(NA)
  }else if(x=="DK" | x=="Missing"){
    return(NA)
  }else{
    return(x)
  }
}
df$educ <- sapply(df$educ,recodeEduc)

df$educ <- factor(df$educ,
                  levels = c(1,2,3,4),
                  labels = c("Preschool","Primary","Secondary","Higher"))

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

plot(table(df$ageCategory,df$educ))
