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

wd <- "D:/Documents/Data/BrazilSurvey/spss"
setwd(wd)

library(foreign)
library(plyr)
library(data.table)
library(Hmisc)
library(varhandle)

load("PNDS2006_BR_DOM_PESS.RData")
pr <- data.frame(dat,as.is=TRUE,check.names=FALSE)
load("PNDS2006_BR_FILHOS.RData")
ch <- data.frame(dat,as.is=TRUE,check.names=FALSE)

load("wealth.RData")
dat <- dat[c("DOMICILIO_ID","wealth")]

pr <- pr[order(pr$DOMICILIO_ID),]
dat <- dat[order(dat$DOMICILIO_ID),]
wealth <- dat$wealth

pr <- cbind(pr,wealth)

names(pr)[which(names(pr)=="CD017_LINH")] <- "line"
names(pr)[which(names(pr)=="CD002_CONG")] <- "cluster"
names(pr)[which(names(pr)=="DOMICILIO_ID")] <- "household"

names(pr)[which(names(pr)=="CD008_SITU")] <- "urban.rural"
pr$urban <- NA
pr$urban[which(pr$urban.rural=="Urbano")] <- 1
pr$urban[which(pr$urban.rural=="Rural")] <- 0

names(pr)[which(names(pr)=="XP999_PESO")] <- "weights"
pr$weights <- pr$weights/10000

names(pr)[which(names(pr)=="P004_SEXO")] <- "sex"
pr$gender <- NA
pr$gender[which(pr$sex=="Masculino")] <- "Male"
pr$gender[which(pr$sex=="Feminino")] <- "Female"
pr$sex <- pr$gender
pr$gender <- NULL
names(pr)[which(names(pr)=="XP010_MELH")] <- "age"
names(pr)[which(names(pr)=="P011B_GRAU")] <- "educ"

names(pr)[which(names(pr)=="P003_PARE")] <- "head"
head <- subset(pr,head=="Responsável")
names(head)[which(names(head)=="age")] <- "head.age"
names(head)[which(names(head)=="sex")] <- "head.sex"
keep <- c("household","head.age","head.sex")
head <- head[keep]

pr <- join(
  pr
  ,head
  ,by="household"
  )

povcalcut <- subset(povcalcuts,filename=="Brazil")$hc
povcalperc <- weighted.percentile(pr$wealth,pr$weights,prob=povcalcut)

pr$p20 <- (pr$wealth <= povcalperc)

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

pr$head.ageCategory <- vapply(pr$head.age,codeAgeCat,character(1))
pr$head.ageCategory <- factor(pr$head.ageCategory,
                              levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34"
                                         ,"35-39","40-44","45-49","50-54","55-59","60-64"
                                         ,"65-69","70-74","75-79","80-84","85-89","90-94"
                                         ,"95+","missing")                          
)

names(ch)[which(names(ch)=="M241_LINH")] <- "line"
names(ch)[which(names(ch)=="MULHER_ID")] <- "mother.id"
ch$household <- substr(ch$mother.id,1,nchar(ch$mother.id)-2)
names(ch)[which(names(ch)=="CM002_CONG")] <- "cluster"
names(ch)[which(names(ch)=="XF110_PESO")] <- "weight.kg"
ch$weight.kg[which(ch$weight.kg>=998)] <- NA
names(ch)[which(names(ch)=="XF120_ALTU")] <- "height.cm"
ch$height.cm[which(ch$height.cm>=998)] <- NA
names(ch)[which(names(ch)=="XF100_MESE")] <- "age.months"
ch$height.cm[which(ch$height.cm>=998)] <- NA
names(ch)[which(names(ch)=="M847_DEIT")] <- "standing.lying"
names(ch)[which(names(ch)=="XF310_INDI")] <- "child.height.age"
names(ch)[which(names(ch)=="XF320_INDI")] <- "child.weight.age"

chKeep <- c("line","cluster","household","weight.kg"
            ,"height.cm","age.months","standing.lying"
            ,"child.height.age","child.weight.age")
ch <- ch[chKeep]

#Neither of these are unique!!! What is the purpose of the line number?!?!
pr$unique <- paste0(pr$household,pr$line)
ch$unique <- paste0(ch$household,ch$line)

data <- cbind(pr,ch[match(pr$unique,ch$unique),])

keep <- c("unique","wealth","weights","urban.rural","urban","educ","age","sex","cluster","household","head.sex","head.age","p20"
          ,"birth.cert","birth.reg","age.months","weight.kg","height.cm","standing.lying","child.height.age","child.weight.age"
          ,"woman.bmi","man.bmi","ageCategory","head.ageCategory","stunting"
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
data$filename <- "Brazil"

brazil.data.total <- data
save(brazil.data.total,file="crosstab.RData")
