wd <- "D:/Documents/Data/DHSauto/khhr61dt/"

setwd(wd)

library(foreign)
library(plyr)
library(data.table)
library(Hmisc)

dat <- read.csv("KHHR61FL.csv",na.strings="",as.is=TRUE,check.names=FALSE)

dummyList <- list(
  dum1 = model.matrix( ~ hv102_11 - 1, data=dat )
  ,dum2 = model.matrix( ~ hv102_12 - 1, data=dat )
  ,dum3 = model.matrix( ~ hv102_13 - 1, data=dat )
  ,dum4 = model.matrix( ~ hv102_21 - 1, data=dat )
  ,dum5 = model.matrix( ~ hv102_31 - 1, data=dat )
  ,dum6 = model.matrix( ~ hv102_32 - 1, data=dat )
  ,dum7 = model.matrix( ~ D025C2_BANH - 1, data=dat )
  ,dum8 = model.matrix( ~ D025D2_AUTO - 1, data=dat )
  ,dum9 = model.matrix( ~ D025E2_GELA - 1, data=dat )
  ,dum10 = model.matrix( ~ D025F2_FREE - 1, data=dat )
  ,dum11 = model.matrix( ~ D025G2_ASPI - 1, data=dat )
  ,dum12 = model.matrix( ~ D025H2_MAQU - 1, data=dat )
  ,dum13 = model.matrix( ~ D025I2_VIDE - 1, data=dat )
  ,dum14 = model.matrix( ~ D025J2_EMPR - 1, data=dat )
  ,dum15 = model.matrix( ~ D026_QUAN - 1, data=dat )
  ,dum16 = model.matrix( ~ D027_COMO - 1, data=dat )
  ,dum17 = model.matrix( ~ D028_MATE - 1, data=dat )
  ,dum18 = model.matrix( ~ D029_MATE - 1, data=dat )
  ,dum19 = model.matrix( ~ D030_MATE - 1, data=dat )
  ,dum20 = model.matrix( ~ D031_SAL - 1, data=dat )
  ,dum21 = model.matrix( ~ D032_SAL - 1, data=dat )
)

cbindlist <- function(list) {
  n <- length(list)
  res <- list[[1]]
  for (i in 2:n){
    item <- list[[i]]
    res <- cbind(res, item[match(rownames(res),rownames(item)),]) 
  }
  return(res)
}

dummies <- cbindlist(dummyList)

dummy.columns <- colnames(dummies)
deleted <- 0

for(i in 1:length(dummy.columns)){
  dummy.column <- dummy.columns[i]
  if(grepl("Sem resposta",dummy.column,ignore.case=TRUE)){
    index <- i-deleted
    dummies <- dummies[,-index]
    deleted <- deleted + 1
  }
}

dat <- cbindlist(list(dat,dummies))

asset.cap.vars <- c(
  "hv102_11"
  ,"hv102_11"
)

recode.assets <- function(x){
  if(is.na(x)){
    return(0)
  }else if(x=="Sem resposta"){
    return(0)
  }else if(x=="Não sabe"){
    return(0)
  }else if(x=="Recusou-se a responder"){
    return(0)
  }else if(grepl("Não",x,ignore.case=TRUE,fixed=TRUE)){
    return(0)
  }else{
    return(1)
  }
}

for(i in 1:length(asset.cap.vars)){
  asset.cap.var <- asset.cap.vars[i]
  dat[[asset.cap.var]] <- sapply(dat[[asset.cap.var]],recode.assets)
}

good.keep <- c()
asset.cap.vars <- c(asset.cap.vars,colnames(dummies))
for(i in 1:length(asset.cap.vars)){
  varname <- asset.cap.vars[i];
  if((varname %in% names(dat))){
    dat[[varname]][!complete.cases(dat[[varname]])] <- 0
    var.sd <- sd(dat[[varname]],na.rm=TRUE)
    if(var.sd!=0){
      good.keep <- c(good.keep,varname) 
    } 
  }
}

dat.asset.cap <- dat[good.keep]

#Common

dat.pca <- prcomp(dat.asset.cap)

pca1 <- dat.pca$rotation[,1]

pca.vars <- names(pca1)

c.wealth <- c()
for(i in 1:length(pca.vars)){
  pca.var <- pca.vars[i]
  message(paste(i,pca.var,sep=". "))
  component <- pca1[[pca.var]]
  column <- dat[[pca.var]]
  var.mean <- mean(column,na.rm=TRUE)
  var.sd <- sd(column,na.rm=TRUE)
  for(j in 1:length(column)){
    val <- column[j]
    wealth.contribution <- ((val-var.mean)/var.sd)*component
    if(is.null(c.wealth[j])){
      c.wealth[j] = wealth.contribution
    }else{
      c.wealth[j] = sum(c.wealth[j], wealth.contribution,na.rm=TRUE)
    }
  }
}

dat <- cbind(dat,c.wealth)

#urban
urban <- subset(dat,CD008_SITU=="Urbano")

dat.pca <- prcomp(dat.asset.cap[which(dat$CD008_SITU=="Urbano"),])

pca1 <- dat.pca$rotation[,1]

pca.vars <- names(pca1)

u.wealth <- c()
for(i in 1:length(pca.vars)){
  pca.var <- pca.vars[i]
  message(paste(i,pca.var,sep=". "))
  component <- pca1[[pca.var]]
  column <- urban[[pca.var]]
  var.mean <- mean(column,na.rm=TRUE)
  var.sd <- sd(column,na.rm=TRUE)
  for(j in 1:length(column)){
    val <- column[j]
    wealth.contribution <- ((val-var.mean)/var.sd)*component
    if(is.null(u.wealth[j])){
      u.wealth[j] = wealth.contribution
    }else{
      u.wealth[j] = sum(u.wealth[j], wealth.contribution,na.rm=TRUE)
    }
  }
}

urban <- cbind(urban,u.wealth)

urban.lm <- lm(c.wealth~u.wealth,data=urban)
u.alpha <- urban.lm$coefficients[[1]]
u.beta <- urban.lm$coefficients[[2]]

#Rural
rural <- subset(dat,CD008_SITU=="Rural")

dat.pca <- prcomp(dat.asset.cap[which(dat$CD008_SITU=="Rural"),])

pca1 <- dat.pca$rotation[,1]

pca.vars <- names(pca1)

r.wealth <- c()
for(i in 1:length(pca.vars)){
  pca.var <- pca.vars[i]
  message(paste(i,pca.var,sep=". "))
  component <- pca1[[pca.var]]
  column <- rural[[pca.var]]
  var.mean <- mean(column,na.rm=TRUE)
  var.sd <- sd(column,na.rm=TRUE)
  for(j in 1:length(column)){
    val <- column[j]
    wealth.contribution <- ((val-var.mean)/var.sd)*component
    if(is.null(r.wealth[j])){
      r.wealth[j] = wealth.contribution
    }else{
      r.wealth[j] = sum(r.wealth[j], wealth.contribution,na.rm=TRUE)
    }
  }
}

rural <- cbind(rural,r.wealth)

rural.lm <- lm(c.wealth~r.wealth,data=rural)
r.alpha <- rural.lm$coefficients[[1]]
r.beta <- rural.lm$coefficients[[2]]

#Composite
rural$u.wealth <- NA
urban$r.wealth <- NA
urban$wealth <- u.alpha+(u.beta*u.wealth)
rural$wealth <- r.alpha+(r.beta*r.wealth)
dat <- rbind(urban,rural)
save(dat,file=paste0("wealth.RData"))
