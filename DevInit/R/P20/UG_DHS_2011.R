#install.packages('foreign')
#install.packages('Hmisc')
#install.packages('hexbin')
library(foreign)
library(Hmisc)
library(hexbin)

wd <- "D:/Documents/Data/DHSauto/UGPR60DT"
setwd(wd)

df <- read.dta("UGPR60FL.dta")

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
recodeEduc <- function(x){
  if(is.na(x)){
    return(NA)
  }else if(x==8 | x==9){
    return(NA)
  }else{
    return(x)
  }
}
df$educ <- sapply(df$educ,recodeEduc)
calcDisabilityScore <- function(vars){
  scores <- double(nrow(vars))
  for(i in 1:nrow(vars)){
    score <- 0
    row <- vars[i,]
    for(j in 1:length(vars)){
      var <- row[1,j]
      if(!is.na(var)){
        if(var==2){
          score <- score + 1
        }else if(var==3){
          score <- score + 2
        }else if(var==4){
          score <- score + 3
        }
      }
    }
    scores[i] <- score
  }
  return(scores)
}
disVars <- c("sh24","sh25","sh26","sh27","sh28","sh29")
df$disability <- calcDisabilityScore(df[disVars])

p1 <- ggplot(data=df,aes(x=age,y=disability)) +
  stat_binhex()
p1
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
educ.age.tab <- as.data.frame(educ.age.tab)
names(educ.age.tab) <- c("Age","Education","Count")
educ.age.tab$Education <- factor(
  educ.age.tab$Education
  ,levels = c(0,1,2,3)
  ,labels = c("No education","Primary school","Secondary school","Higher education")
  )

heatmap <- ggplot(educ.age.tab,aes(x=Age,y=Education,fill=Count)) +
  geom_tile()
heatmap

educ.disa.tab <- table(df$disability,df$educ)
educ.disa.tab
