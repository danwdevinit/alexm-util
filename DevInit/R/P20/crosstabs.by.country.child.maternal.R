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
load("child.maternal.RData")

any.vacc.missing <- c("DK","Manquant","Missing","No sabe","NSP")
any.vacc.yes <- c("Oui","Sí","TRUE",TRUE,"Yes")
any.vacc.no <- c("FALSE",FALSE,"No","Non")
child.health$any.vacc <- unfactor(child.health$any.vacc)
child.health$any.vacc[which(child.health$any.vacc %in% any.vacc.missing)] <- NA
child.health$any.vacc[which(child.health$any.vacc %in% any.vacc.yes)] <- TRUE
child.health$any.vacc[which(child.health$any.vacc %in% any.vacc.no)] <- FALSE

maternal.health$md <- maternal.health$maternal.deaths>=1

maternal.health$csurv <- (maternal.health$ceb-maternal.health$cdead)/maternal.health$ceb
maternal.health$csurv[which(maternal.health$csurv==-Inf)] <- NA
maternal.health$csurv.cat <- NA
maternal.health$csurv.cat <- maternal.health$csurv==1

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

countryMeta$filename[which(countryMeta$filename=="bfhr70dt")] <- "bfhr62dt"
countryMeta$filename[which(countryMeta$filename=="kehr7hdt")] <- "kehr70dt"
countryMeta$filename[which(countryMeta$filename=="mdhr6hdt")] <- "mdhr51dt"
countryMeta$filename[which(countryMeta$filename=="mwhr71dt")] <- "mwhr61dt"
countryMeta$filename[which(countryMeta$filename=="tzhr6adt")] <- "tzhr63dt"
countryMeta$filename[which(countryMeta$filename=="ughr72dt")] <- "ughr60dt"

newNames <- c("p20.ch.skilled.attendant"
              ,"p20.ch.no.skilled.attendant"
              ,"p80.ch.skilled.attendant"
              ,"p80.ch.no.skilled.attendant"
              ,"p20.ch.any.vacc"
              ,"p20.ch.no.vacc"
              ,"p80.ch.any.vacc"
              ,"p80.ch.no.vacc"
              ,"p20.wm.skilled.attendant"
              ,"p20.wm.no.skilled.attendant"
              ,"p80.wm.skilled.attendant"
              ,"p80.wm.no.skilled.attendant"
              ,"p20.wm.md"
              ,"p20.wm.no.md"
              ,"p80.wm.md"
              ,"p80.wm.no.md"
              ,"p20.wm.csurv.1"
              ,"p20.wm.csurv.not.1"
              ,"p80.wm.csurv.1"
              ,"p80.wm.csurv.not.1"
)

for(i in 1:length(newNames)){
  countryMeta[[newNames[i]]] <- NA
}

filenames <- countryMeta$filename
for(i in 1:length(filenames)){
  this.filename <- filenames[i]
  message(this.filename)
  this.pop.under5.male <- subset(countryMeta,filename==this.filename)$male.under5
  this.pop.under5.female <- subset(countryMeta,filename==this.filename)$female.under5
  ch.pop <- this.pop.under5.female + this.pop.under5.male
  wm.pop <- subset(countryMeta,filename==this.filename)$female.15.49
  ch <- subset(child.health,filename==this.filename)
  wm <- subset(maternal.health,filename==this.filename)
  if(nrow(ch)>0){
    #Skilled.attendant
    if(length(ch$skilled.attendant[which(!is.na(ch$skilled.attendant))])!=0){
      confidence.tab <- pop.confidence(ch$skilled.attendant,ch$p20,ch$weights,ch.pop)
      countryMeta$p80.ch.no.skilled.attendant[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["FALSE","FALSE"]},error=function(e){0})
      countryMeta$p80.ch.skilled.attendant[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["TRUE","FALSE"]},error=function(e){0})
      countryMeta$p20.ch.no.skilled.attendant[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["FALSE","TRUE"]},error=function(e){0})
      countryMeta$p20.ch.skilled.attendant[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["TRUE","TRUE"]},error=function(e){0})
    } 
    #any.vacc
    if(length(ch$any.vacc[which(!is.na(ch$any.vacc))])!=0){
      confidence.tab <- pop.confidence(ch$any.vacc,ch$p20,ch$weights,ch.pop)
      countryMeta$p80.ch.no.vacc[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["FALSE","FALSE"]},error=function(e){0})
      countryMeta$p80.ch.any.vacc[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["TRUE","FALSE"]},error=function(e){0})
      countryMeta$p20.ch.no.vacc[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["FALSE","TRUE"]},error=function(e){0})
      countryMeta$p20.ch.any.vacc[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["TRUE","TRUE"]},error=function(e){0})
    } 
  }
  if(nrow(wm)>0){
    #skilled.attendant
    if(length(wm$skilled.attendant[which(!is.na(wm$skilled.attendant))])!=0){
      confidence.tab <- pop.confidence(wm$skilled.attendant,wm$p20,wm$weights,wm.pop)
      countryMeta$p80.wm.no.skilled.attendant[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["FALSE","FALSE"]},error=function(e){0})
      countryMeta$p80.wm.skilled.attendant[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["TRUE","FALSE"]},error=function(e){0})
      countryMeta$p20.wm.no.skilled.attendant[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["FALSE","TRUE"]},error=function(e){0})
      countryMeta$p20.wm.skilled.attendant[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["TRUE","TRUE"]},error=function(e){0})
    } 
    #md
    if(length(wm$md[which(!is.na(wm$md))])!=0){
      confidence.tab <- pop.confidence(wm$md,wm$p20,wm$weights,wm.pop)
      countryMeta$p80.wm.no.md[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["FALSE","FALSE"]},error=function(e){0})
      countryMeta$p80.wm.md[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["TRUE","FALSE"]},error=function(e){0})
      countryMeta$p20.wm.no.md[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["FALSE","TRUE"]},error=function(e){0})
      countryMeta$p20.wm.md[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["TRUE","TRUE"]},error=function(e){0})
    }
    #csurv
    if(length(wm$csurv.cat[which(!is.na(wm$csurv.cat))])!=0){
      confidence.tab <- pop.confidence(wm$csurv.cat,wm$p20,wm$weights,wm.pop)
      countryMeta$p80.wm.csurv.not.1[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["FALSE","FALSE"]},error=function(e){0})
      countryMeta$p80.wm.csurv.1[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["TRUE","FALSE"]},error=function(e){0})
      countryMeta$p20.wm.csurv.not.1[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["FALSE","TRUE"]},error=function(e){0})
      countryMeta$p20.wm.csurv.1[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["TRUE","TRUE"]},error=function(e){0}) 
    } 
  }
}

write.csv(countryMeta,"p20_ch_wm.csv",row.names=FALSE,na="")
