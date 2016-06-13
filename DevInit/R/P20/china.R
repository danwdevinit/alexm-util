wd <- "D:/Documents/Data/ChinaSurvey/"
setwd(wd)

library(foreign)
library(sas7bdat)
library(readstata13)
library(data.table)
library(plyr)
library(varhandle)

# fam <- read.dta("STATA/CFPS_2012_family_dta/Ecfps2012family_032015.dta")
# attributes(fam)$variable.labels <- data.frame(names(fam),attributes(fam)[7])
# famros <- read.dta13("STATA/CFPS_2012_famconf_dta/Ecfps2012famroster_032015compress.dta")
# attributes(famros)$variable.labels <- data.frame(names(famros),attributes(famros)[8])
# tracking <- read.dta("STATA/CFPS_2012_tracking_dta/Ecrosswaveid_032015.dta")
# attributes(tracking)$variable.labels <- data.frame(names(tracking),attributes(tracking)[7])
# adult <- read.dta("STATA/CFPS_2012_adult_dta/Ecfps2012adultcombined_032015.dta")
# attributes(adult)$variable.labels <- data.frame(names(adult),attributes(adult)[7])
# child <- read.dta("STATA/CFPS_2012_child_dta/Ecfps2012childcombined_032015.dta")
# attributes(child)$variable.labels <- data.frame(names(child),attributes(child)[7])
# 
# 
# save(fam,famros,tracking,adult,child,file="dat2012.RData")
load("dat2012.RData")
# write.csv(attributes(fam)$variable.labels,"fam.vars.csv")
# write.csv(attributes(famros)$variable.labels,"famros.vars.csv")
# write.csv(attributes(tracking)$variable.labels,"tracking.vars.csv")
# write.csv(attributes(adult)$variable.labels,"adult.vars.csv")
# write.csv(attributes(child)$variable.labels,"child.vars.csv")

dat <- data.frame(fam)

assets <- c(
  "fs6_s_1"
  ,"fs6_s_2"
  ,"fs6_s_3"
  ,"fs6_s_4"
  ,"fs6_s_5"
  ,"fs6_s_6"
  ,"fs6_s_7"
  ,"fs6_s_8"
  ,"fs6_s_9"
  ,"fs6_s_10"
  ,"fs6_s_11"
  ,"fs6_s_12"
  ,"fs6_s_13"
  ,"fs6_s_14"
  ,"fs6_s_15"
  ,"fs6_s_16"
  ,"fs6_s_17"
  ,"fs6_s_18"
  ,"fs6_s_19"
  )

assetDf <- dat[assets]
uniqueAssets <- c()
for(i in 1:length(assetDf)){
  uniqueAssets <- unique(c(uniqueAssets,as.character(assetDf[,i])))
}

uniqueAssets <- subset(uniqueAssets,!is.na(uniqueAssets) & uniqueAssets!="NA" & uniqueAssets!="Unknown")

recode.assets <- function(assetDf,uniqueAssets){
  fs6 <- list()
  for(i in 1:nrow(assetDf)){
    #for each family
    row <- assetDf[i,]
    for(j in 1:length(uniqueAssets)){
      #for each asset
      uniqueAsset <- uniqueAssets[j]
      if(is.null(fs6[[uniqueAsset]])){
        fs6[[uniqueAsset]] <- uniqueAsset %in% unfactor(row)
      }else{
        fs6[[uniqueAsset]] <- c(fs6[[uniqueAsset]],uniqueAsset %in% unfactor(row)) 
      }
    } 
  }
  return(fs6)
}

fs6 <- recode.assets(assetDf,uniqueAssets)
fs6 <- data.frame(fs6)
fs6.names <- names(fs6)
dat <- cbind(dat,fs6)

dummyList <- list(
  dum1 = model.matrix( ~ fq1 - 1, data=dat )
  ,dum2 = model.matrix( ~ fb1 - 1, data=dat )
  ,dum3 = model.matrix( ~ fb2 - 1, data=dat )
  ,dum4 = model.matrix( ~ fb3 - 1, data=dat )
  ,dum5 = model.matrix( ~ fb5 - 1, data=dat )
  ,dum6 = model.matrix( ~ fb7 - 1, data=dat )
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
  if(
    grepl("unknown",dummy.column,ignore.case=TRUE) |
      grepl("NA",dummy.column) |
      grepl("refuse",dummy.column,ignore.case=TRUE)
    ){
    index <- i-deleted
    dummies <- dummies[,-index]
    deleted <- deleted + 1
  }
}

dat <- cbindlist(list(dat,dummies))

asset.cap.vars <- c(fs6.names)

recode.assets2 <- function(x){
  if(is.na(x)){
    return(0)
  }else if(is.null(x)){
    return(0)
  }else if(x==TRUE){
    return(1)
  }else if(x==1){
    return(1)
  }else{
    return(0)
  }
}

for(i in 1:length(asset.cap.vars)){
  asset.cap.var <- asset.cap.vars[i]
  dat[[asset.cap.var]] <- sapply(dat[[asset.cap.var]],recode.assets2)
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
urban <- subset(dat,urban12=="Urban")

dat.pca <- prcomp(dat.asset.cap[which(dat$urban12=="Urban"),])

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
rural <- subset(dat,urban12=="Rural")

dat.pca <- prcomp(dat.asset.cap[which(dat$urban12=="Rural"),])

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
