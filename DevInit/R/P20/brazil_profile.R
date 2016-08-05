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

names(pr)[which(names(pr)=="P000_NQUE")] <- "line"
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
natpovcut <- 0.173
povcalperc <- weighted.percentile(pr$wealth,pr$weights,prob=povcalcut)
natpovperc <- weighted.percentile(pr$wealth,pr$weights,prob=natpovcut)

pr$p20 <- (pr$wealth < povcalperc)
pr$npp <- (pr$wealth < natpovperc)

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

names(ch)[which(names(ch)=="M241_LINH")] <- "ch.line"
names(ch)[which(names(ch)=="M241Z_NQUE")] <- "line"
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

pr <- cbind(pr,ch[match(pr$line,ch$line),])

pr$stunting <- NA
# pr$stunting[which(pr$child.height.age<= (-6))] <- "Implausibly low"
pr$stunting[which(pr$child.height.age > (-6) & pr$child.height.age<= (-3))] <- "Severely stunted"
pr$stunting[which(pr$child.height.age > (-3) & pr$child.height.age<= (-2))] <- "Stunted, but not severely"
pr$stunting[which(pr$child.height.age > (-2) & pr$child.height.age< (6))] <- "Not stunted"
# pr$stunting[which(pr$child.height.age>= (6))] <- "Implausibly high"

pr$stunting <- factor(pr$stunting
                      ,levels=c(
                        "Implausibly low"
                        ,"Severely stunted"
                        ,"Stunted, but not severely"
                        ,"Not stunted"
                        ,"Implausibly high"
                      ))

recode.educ <- function(x){
  if(is.na(x)){return(NA)}
  else if(x=="Sem resposta" | x=="Não sabe"){return(NA)}
  else if(x=="Nenhum" | x=="Creche (não seriado)" | x=="Pré-escola (não seriado)"){return("No education, preschool")}
  else if(x=="CA / Alfab adultos (não seriado)" | x=="EJA (não seriado)" | x=="Ensino fundamental (seriado)" | x=="Crianças especiais"){return("Primary")}
  else if(x=="Supletivo ensino fundamental" | x=="Ensino médio"){return("Secondary")}
  else{return("Higher")}
}
pr$educ <- sapply(pr$educ,recode.educ)
pr$educ <- factor(pr$educ
                  ,levels = c("No education, preschool","Primary","Secondary","Higher")
)

keep <- c("wealth","weights","urban.rural","urban","educ","age","sex","cluster","household","head.sex","head.age","p20"
          ,"birth.cert","birth.reg","age.months","weight.kg","height.cm","standing.lying","child.height.age","child.weight.age"
          ,"woman.bmi","man.bmi","ageCategory","head.ageCategory","stunting","npp"
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


save(data,file="profiledat.RData")

data$inbetween <- data$p20==FALSE & data$npp==TRUE
tweens <- subset(data,inbetween)
weighted.mean(tweens$urban,tweens$weights)
weighted.mean(tweens$educ=="No education, preschool",tweens$weights,na.rm=TRUE)
weighted.mean(tweens$age,tweens$weights)

countryMeta <- read.csv("D:/Documents/Data/MICSmeta/headcounts.csv",as.is=TRUE)
under5 <- subset(data,age<5)
over5 <- subset(data,age>=5)
under15 <- subset(data,age<15)
over15 <- subset(data,age>=15)
over25 <- subset(data,age>=25)
women <- subset(data,sex=="Female")
men <- subset(data,sex=="Male")
this.pop <- subset(countryMeta,filename=="Brazil")$pop.total
this.pop.under5.male <- subset(countryMeta,filename=="Brazil")$male.under5
this.pop.under5.female <- subset(countryMeta,filename=="Brazil")$female.under5
this.pop.under5 <- this.pop.under5.female + this.pop.under5.male
this.pop.over5 <- this.pop - this.pop.under5
this.pop.under15.male <- this.pop.under5.male + subset(countryMeta,filename=="Brazil")$male.5.14
this.pop.under15.female <- this.pop.under5.female + subset(countryMeta,filename=="Brazil")$female.5.14
this.pop.under15 <- this.pop.under15.male + this.pop.under15.female
this.pop.over15 <- this.pop - this.pop.under15
this.pop.male <- subset(countryMeta,filename=="Brazil")$pop.male
this.pop.female <- subset(countryMeta,filename=="Brazil")$pop.female
this.pop.over15.male <- this.pop.male - this.pop.under15.male
this.pop.over15.female <- this.pop.female - this.pop.under15.female
this.pop.over25.male <- subset(countryMeta,filename=="Brazil")$male.25.plus
this.pop.over25.female <- subset(countryMeta,filename=="Brazil")$female.25.plus
this.pop.over25 <- this.pop.over25.male + this.pop.over25.female
library(descr)
library(varhandle)

# Stop crosstab from plotting everything
options(descr.plot = FALSE)
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
#Educ-P20
over25.male <- subset(over25,sex=="Male")
over25.female <- subset(over25,sex=="Female")
pop.confidence(over25.male$educ,over25.male$p20,over25.male$weights,this.pop.over25.male)$estimate
pop.confidence(over25.female$educ,over25.female$p20,over25.female$weights,this.pop.over25.female)$estimate

pop.confidence(data$ageCategory,data$p20,data$weights,this.pop)$estimate
