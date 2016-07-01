#install.packages('foreign')
#install.packages('Hmisc')
#install.packages('hexbin')
library(foreign)
library(Hmisc)
library(hexbin)
library(descr)
library(data.table)
library(plyr)
library(lmtest)
library(sandwich)

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


wd <- "D:/Documents/Data/DHSauto/UGPR60DT"
setwd(wd)

df <- read.dta("UGPR60FL.dta")

names(df)[which(names(df)=="hv101")] <- "relationship"
names(df)[which(names(df)=="hv104")] <- "gender"
names(df)[which(names(df)=="hv105")] <- "age"
names(df)[which(names(df)=="hv106")] <- "educ"
names(df)[which(names(df)=="hv001")] <- "cluster"
names(df)[which(names(df)=="hv002")] <- "household"
names(df)[which(names(df)=="hv005")] <- "sample.weights"
df$weights <- df$sample.weights/1000000

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
    missing.count <- 0
    for(j in 1:length(vars)){
      var <- row[1,j]
      if(!is.na(var)){
        if(var==2){
          score <- score + 1
        }else if(var==3){
          score <- score + 2
        }else if(var==4){
          score <- score + 3
        }else if(var==8 | var==9){
          missing.count <- missing.count + 1
        }
      }else{
        missing.count <- missing.count + 1
      }
    }
    if(missing.count<6){
      scores[i] <- score
    }else{
      scores[i] <- NA
    }
    
  }
  return(scores)
}
disVars <- c("sh24","sh25","sh26","sh27","sh28","sh29")
df$disability <- calcDisabilityScore(df[disVars])

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
  if(x>=95){
    return("95+")
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

# educ.age.tab <- table(df$ageCategory,df$educ)
# educ.age.tab
# educ.age.tab <- as.data.frame(educ.age.tab)
# names(educ.age.tab) <- c("Age","Education","Count")
# educ.age.tab$Education <- factor(
#   educ.age.tab$Education
#   ,levels = c(0,1,2,3)
#   ,labels = c("No education","Primary school","Secondary school","Higher education")
#   )
# 
# heatmap <- ggplot(educ.age.tab,aes(x=Age,y=Education,fill=Count)) +
#   geom_tile()
# heatmap
# 
# educ.disa.tab <- table(df$disability,df$educ)
# educ.disa.tab

hr <- read.dta("../ughr60dt/UGHR60FL.dta")
names(hr)[which(names(hr)=="hv271")] <- "wealth"
hr$wealth <- hr$wealth/100000
names(hr)[which(names(hr)=="hv001")] <- "cluster"
names(hr)[which(names(hr)=="hv002")] <- "household"
names(hr)[which(names(hr)=="hv005")] <- "sample.weights"
hr$weights <- hr$sample.weights/1000000

this.pop <- 37782000
povcalcut <- 0.4715
povcalperc <- weighted.percentile(hr$wealth,hr$weights,prob=povcalcut)
hr$p20 <- (hr$wealth < povcalperc)
hrKeep <- c("cluster","household","wealth","weights","p20")
hr <- hr[hrKeep]

df <- join(
  df
  ,hr
  ,by=c("cluster","household")
)

crossTabs <- list()

# Stop crosstab from plotting everything
options(descr.plot = FALSE)

df$p20.bool <- df$p20

df$p20[which(df$p20==TRUE)] <- "P20"
df$p20[which(df$p20==FALSE)] <- "non-P20"

p1 <- ggplot(data=df,aes(x=wealth,y=disability)) +
  stat_binhex()
p1

confidence.tab <- pop.confidence(df$disability,df$p20,df$weights,this.pop)
crossTabs[["aggregate"]] <- confidence.tab

newNames <- c("sight","hearing","mobility","memory","self.care","communication")

names(df)[which(names(df) %in% disVars)] <- newNames
df[newNames][df[newNames] == 1] <- "no - no difficulty"
df[newNames][df[newNames] == 2] <- "yes - some difficulty"
df[newNames][df[newNames] == 3] <- "yes - a lot of difficulty"
df[newNames][df[newNames] == 4] <- "cannot do at all"
df[newNames][df[newNames] == 8] <- NA
df[newNames][df[newNames] == 9] <- NA

for(i in 1:length(newNames)){
  this.var <- newNames[i]
  df[,this.var] <- factor(df[,this.var],levels=c(
    "no - no difficulty"
    ,"yes - some difficulty"
    ,"yes - a lot of difficulty"
    ,"cannot do at all"
    ))
}

confidence.tab <- pop.confidence(df$sight,df$p20,df$weights,this.pop)
crossTabs[["sight"]] <- confidence.tab

confidence.tab <- pop.confidence(df$hearing,df$p20,df$weights,this.pop)
crossTabs[["hearing"]] <- confidence.tab

confidence.tab <- pop.confidence(df$mobility,df$p20,df$weights,this.pop)
crossTabs[["mobility"]] <- confidence.tab

confidence.tab <- pop.confidence(df$memory,df$p20,df$weights,this.pop)
crossTabs[["memory"]] <- confidence.tab

confidence.tab <- pop.confidence(df$self.care,df$p20,df$weights,this.pop)
crossTabs[["self.care"]] <- confidence.tab

confidence.tab <- pop.confidence(df$communication,df$p20,df$weights,this.pop)
crossTabs[["communication"]] <- confidence.tab

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

# saveWorkbook(wb, "D:/Documents/Data/MICSmeta/Disability.xlsx", overwrite = TRUE)

fit <- glm(p20.bool~disability,data=df,family="binomial",weights=df$weights)
summary(fit)

fit <- lm(wealth~disability,data=df,weights=df$weights)
summary(fit)

#Robust for heteroskedasticity
coeftest(fit, vcov = vcovHC(fit, "HC1"))
