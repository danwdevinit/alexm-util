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

child.health$p20[which(child.health$p20==TRUE)] <- "P20"
child.health$p20[which(child.health$p20==FALSE)] <- "P80"
maternal.health$p20[which(maternal.health$p20==TRUE)] <- "P20"
maternal.health$p20[which(maternal.health$p20==FALSE)] <- "P80"

any.vacc.missing <- c("DK","Manquant","Missing","No sabe","NSP")
any.vacc.yes <- c("Oui","Sí","TRUE",TRUE,"Yes")
any.vacc.no <- c("FALSE",FALSE,"No","Non")
child.health$any.vacc <- unfactor(child.health$any.vacc)
child.health$any.vacc[which(child.health$any.vacc %in% any.vacc.missing)] <- NA
child.health$any.vacc[which(child.health$any.vacc %in% any.vacc.yes)] <- "Received at least one vaccination"
child.health$any.vacc[which(child.health$any.vacc %in% any.vacc.no)] <- "Received no vaccinations"

maternal.health$md <- maternal.health$maternal.deaths>=1
maternal.health$md[which(maternal.health$md==TRUE)] <- "Experienced at least one maternal death in the family"
maternal.health$md[which(maternal.health$md==FALSE)] <- "Experienced no maternal deaths in the family"

maternal.health$skilled.attendant[which(maternal.health$skilled.attendant==TRUE)] <- "Had skilled birth attendant for most recent birth"
maternal.health$skilled.attendant[which(maternal.health$skilled.attendant==FALSE)] <- "Had no skilled birth attendant for most recent birth"

child.health$skilled.attendant[which(child.health$skilled.attendant==TRUE)] <- "Had skilled birth attendant"
child.health$skilled.attendant[which(child.health$skilled.attendant==FALSE)] <- "Had no skilled birth attendant"

maternal.health$csurv <- (maternal.health$ceb-maternal.health$cdead)/maternal.health$ceb
maternal.health$csurv[which(maternal.health$csurv==-Inf)] <- NA
maternal.health$csurv.cat <- NA
maternal.health$csurv.cat[which(maternal.health$csurv>=0 & maternal.health$csurv<.5)] <- "0-50% survival"
maternal.health$csurv.cat[which(maternal.health$csurv>=.5 & maternal.health$csurv<.99)] <- "50-99% survival"
maternal.health$csurv.cat[which(maternal.health$csurv==1)] <- "100% survival"
maternal.health$csurv.cat <- factor(maternal.health$csurv.cat,levels=c(
  "0-50% survival"
  ,"50-99% survival"
  ,"100% survival"
  ))

conform <- function(complete,incomplete){
  return(
    pmax(
      incomplete[
        match(
          rownames(complete)
          ,rownames(incomplete)
        )
        ,
        match(
          colnames(complete)
          ,colnames(incomplete)
        )
        ]
      ,0
      ,na.rm=TRUE
    )
  )
}

conformAndAdd <- function(mat1,mat2){
  mat1.row <- length(rownames(mat1))
  mat2.row <- length(rownames(mat2))
  mat1.col <- length(colnames(mat1))
  mat2.col <- length(colnames(mat2))
  if(mat1.row>mat2.row){
    mat2 <- pmax(mat2[match(rownames(mat1),rownames(mat2)),],na.rm=TRUE,0)
  }else if(mat1.row<mat2.row){
    mat1 <- pmax(mat1[match(rownames(mat2),rownames(mat1)),],na.rm=TRUE,0)
  }
  if(mat1.col>mat2.col){
    mat2 <- pmax(mat2[,match(colnames(mat1),colnames(mat2))],na.rm=TRUE,0)
  }else if(mat1.col<mat2.col){
    mat1 <- pmax(mat1[,match(colnames(mat2),colnames(mat1))],na.rm=TRUE,0)
  }
  return(
    mat1+mat2
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

countryMeta$filename[which(countryMeta$filename=="bfhr70dt")] <- "bfhr62dt"
countryMeta$filename[which(countryMeta$filename=="kehr7hdt")] <- "kehr70dt"
countryMeta$filename[which(countryMeta$filename=="mdhr6hdt")] <- "mdhr51dt"
countryMeta$filename[which(countryMeta$filename=="mwhr71dt")] <- "mwhr61dt"
countryMeta$filename[which(countryMeta$filename=="tzhr6adt")] <- "tzhr63dt"
countryMeta$filename[which(countryMeta$filename=="ughr72dt")] <- "ughr60dt"

crossTabs <- list()

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
      if(is.null(crossTabs[["ch.skilled.attendant"]])){
        crossTabs[["ch.skilled.attendant"]] <- confidence.tab
      }else{
        crossTabs[["ch.skilled.attendant"]]$low <- crossTabs[["ch.skilled.attendant"]]$low + conform(crossTabs[["ch.skilled.attendant"]]$low,confidence.tab$low)
        crossTabs[["ch.skilled.attendant"]]$estimate <- crossTabs[["ch.skilled.attendant"]]$estimate + conform(crossTabs[["ch.skilled.attendant"]]$estimate,confidence.tab$estimate)
        crossTabs[["ch.skilled.attendant"]]$high <- crossTabs[["ch.skilled.attendant"]]$high + conform(crossTabs[["ch.skilled.attendant"]]$high,confidence.tab$high)
      }  
    } 
    #any.vacc
    if(length(ch$any.vacc[which(!is.na(ch$any.vacc))])!=0){
      confidence.tab <- pop.confidence(ch$any.vacc,ch$p20,ch$weights,ch.pop)
      if(is.null(crossTabs[["ch.any.vacc"]])){
        crossTabs[["ch.any.vacc"]] <- confidence.tab
      }else{
        crossTabs[["ch.any.vacc"]]$low <- crossTabs[["ch.any.vacc"]]$low + conform(crossTabs[["ch.any.vacc"]]$low,confidence.tab$low)
        crossTabs[["ch.any.vacc"]]$estimate <- crossTabs[["ch.any.vacc"]]$estimate + conform(crossTabs[["ch.any.vacc"]]$estimate,confidence.tab$estimate)
        crossTabs[["ch.any.vacc"]]$high <- crossTabs[["ch.any.vacc"]]$high + conform(crossTabs[["ch.any.vacc"]]$high,confidence.tab$high)
      }  
    } 
  }
  if(nrow(wm)>0){
    #skilled.attendant
    if(length(wm$skilled.attendant[which(!is.na(wm$skilled.attendant))])!=0){
      confidence.tab <- pop.confidence(wm$skilled.attendant,wm$p20,wm$weights,wm.pop)
      if(is.null(crossTabs[["wm.skilled.attendant"]])){
        crossTabs[["wm.skilled.attendant"]] <- confidence.tab
      }else{
        crossTabs[["wm.skilled.attendant"]]$low <- crossTabs[["wm.skilled.attendant"]]$low + conform(crossTabs[["wm.skilled.attendant"]]$low,confidence.tab$low)
        crossTabs[["wm.skilled.attendant"]]$estimate <- crossTabs[["wm.skilled.attendant"]]$estimate + conform(crossTabs[["wm.skilled.attendant"]]$estimate,confidence.tab$estimate)
        crossTabs[["wm.skilled.attendant"]]$high <- crossTabs[["wm.skilled.attendant"]]$high + conform(crossTabs[["wm.skilled.attendant"]]$high,confidence.tab$high)
      }  
    } 
    #md
    if(length(wm$md[which(!is.na(wm$md))])!=0){
      confidence.tab <- pop.confidence(wm$md,wm$p20,wm$weights,wm.pop)
      if(is.null(crossTabs[["wm.md"]])){
        crossTabs[["wm.md"]] <- confidence.tab
      }else{
        crossTabs[["wm.md"]]$low <- conformAndAdd(crossTabs[["wm.md"]]$low,confidence.tab$low)
        crossTabs[["wm.md"]]$estimate <- conformAndAdd(crossTabs[["wm.md"]]$estimate,confidence.tab$estimate)
        crossTabs[["wm.md"]]$high <- conformAndAdd(crossTabs[["wm.md"]]$high,confidence.tab$high)
      }  
    }
    #csurv
    if(length(wm$csurv.cat[which(!is.na(wm$csurv.cat))])!=0){
      confidence.tab <- pop.confidence(wm$csurv.cat,wm$p20,wm$weights,wm.pop)
      if(is.null(crossTabs[["wm.csurv.cat"]])){
        crossTabs[["wm.csurv.cat"]] <- confidence.tab
      }else{
        crossTabs[["wm.csurv.cat"]]$low <- crossTabs[["wm.csurv.cat"]]$low + conform(crossTabs[["wm.csurv.cat"]]$low,confidence.tab$low)
        crossTabs[["wm.csurv.cat"]]$estimate <- crossTabs[["wm.csurv.cat"]]$estimate + conform(crossTabs[["wm.csurv.cat"]]$estimate,confidence.tab$estimate)
        crossTabs[["wm.csurv.cat"]]$high <- crossTabs[["wm.csurv.cat"]]$high + conform(crossTabs[["wm.csurv.cat"]]$high,confidence.tab$high)
      }  
    } 
  }
}


rownames(crossTabs[["wm.md"]]$low)[1] <- "Experienced at least one maternal death in the family"
rownames(crossTabs[["wm.md"]]$estimate)[1] <- "Experienced at least one maternal death in the family"
rownames(crossTabs[["wm.md"]]$high)[1] <- "Experienced at least one maternal death in the family"

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

saveWorkbook(wb, "DHS_and_MICS_P20_child_mat_pop_weighted.xlsx", overwrite = TRUE)