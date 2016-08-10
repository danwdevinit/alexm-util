library(foreign)
library(Hmisc)
library(data.table)
library(plyr)
library(varhandle)

wealth <- function(df,catvars,numvars,urbanvar=NA){
  require(data.table)
  
  name.i <- function(i,df){
    return(names(df)[i])
  }
  if(typeof(catvars)=="double" |typeof(catvars)=="integer"){
    catvars <- sapply(catvars,name.i,df=hh)
  }
  if(typeof(numvars)=="double" |typeof(numvars)=="integer"){
    numvars <- sapply(numvars,name.i,df=hh)
  }
  
  generateDummies <- function(df,vars){
    dummyList <- list()
    listIndex <- 1
    for(i in 1:length(vars)){
      var = vars[i]
      df[,var] <- factor(df[,var])
      cmd = paste0("dum = model.matrix( ~ ",var," - 1, data=df)")
      eval(parse(text=cmd))
      dummyList[[listIndex]] = dum
      listIndex <- listIndex + 1
    }
    return(dummyList)
  }
  
  dummyList <- generateDummies(df,catvars)
  
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
  
#   dummy.columns <- colnames(dummies)
#   deleted <- 0
#   
#   for(i in 1:length(dummy.columns)){
#     dummy.column <- dummy.columns[i]
#     if(
#       grepl("unknown",dummy.column,ignore.case=TRUE) |
#         grepl("NA",dummy.column) |
#         grepl("refuse",dummy.column,ignore.case=TRUE)
#     ){
#       index <- i-deleted
#       dummies <- dummies[,-index]
#       deleted <- deleted + 1
#     }
#   }
  
  df <- cbindlist(list(df,dummies))
  
  good.keep <- c()
  wealth.vars <- c(numvars,colnames(dummies))
  for(i in 1:length(wealth.vars)){
    varname <- wealth.vars[i];
    if((varname %in% names(df))){
      df[[varname]][!complete.cases(df[[varname]])] <- 0
      var.sd <- sd(df[[varname]],na.rm=TRUE)
      if(var.sd!=0){
        good.keep <- c(good.keep,varname) 
      } 
    }
  }
  
  dat.asset.cap <- df[good.keep]
  
  #Common
  
  dat.pca <- prcomp(dat.asset.cap)
  
  pca1 <- dat.pca$rotation[,1]
  
  pca.vars <- names(pca1)
  
  c.wealth <- c()
  for(i in 1:length(pca.vars)){
    pca.var <- pca.vars[i]
    message(paste(i,pca.var,sep=". "))
    component <- pca1[[pca.var]]
    column <- df[[pca.var]]
    var.mean <- mean(column,na.rm=TRUE)
    if(pca.var %in% numvars){var.mean <- 0}
    var.sd <- sd(column,na.rm=TRUE)
    for(j in 1:length(column)){
      val <- column[j]
      if(is.na(val)){val<-var.mean}
      wealth.contribution <- ((val-var.mean)/var.sd)*component
      if(is.null(c.wealth[j])){
        c.wealth[j] = wealth.contribution
      }else{
        c.wealth[j] = sum(c.wealth[j], wealth.contribution,na.rm=TRUE)
      }
    }
  }
  
  df <- cbind(df,c.wealth)
  
  if(!is.na(urbanvar)){
    #urban
    urban <- df[which(df[,urbanvar]==1),]
    
    dat.pca <- prcomp(dat.asset.cap[which(df[,urbanvar]==1),])
    
    pca1 <- dat.pca$rotation[,1]
    
    pca.vars <- names(pca1)
    
    u.wealth <- c()
    for(i in 1:length(pca.vars)){
      pca.var <- pca.vars[i]
      message(paste(i,pca.var,sep=". "))
      component <- pca1[[pca.var]]
      column <- urban[[pca.var]]
      var.mean <- mean(column,na.rm=TRUE)
      if(pca.var %in% numvars){var.mean <- 0}
      var.sd <- sd(column,na.rm=TRUE)
      for(j in 1:length(column)){
        val <- column[j]
        if(is.na(val)){val<-var.mean}
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
    rural <- df[which(df[,urbanvar]==0),]
    
    dat.pca <- prcomp(dat.asset.cap[which(df[,urbanvar]==0),])
    
    pca1 <- dat.pca$rotation[,1]
    
    pca.vars <- names(pca1)
    
    r.wealth <- c()
    for(i in 1:length(pca.vars)){
      pca.var <- pca.vars[i]
      message(paste(i,pca.var,sep=". "))
      component <- pca1[[pca.var]]
      column <- rural[[pca.var]]
      var.mean <- mean(column,na.rm=TRUE)
      if(pca.var %in% numvars){var.mean <- 0}
      var.sd <- sd(column,na.rm=TRUE)
      for(j in 1:length(column)){
        val <- column[j]
        if(is.na(val)){val<-var.mean}
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
    df <- rbind(urban,rural) 
  }else{
    df$wealth <- df$c.wealth
  }
  return(df)
}

wd <- "D:/Documents/Data/LSMS/"
setwd(wd)

hh <- read.dta("RUS_2014/hh2014w.dta")
hh.labs <- data.frame(names(hh),attributes(hh)[7])
# View(hh.labs)
# write.csv(hh.labs,"RUS_2014/hh.labs.csv")

adult <- read.dta("RUS_2014/adult2014w.dta")
adult.labs <- data.frame(names(adult),attributes(adult)[7])
# View(adult.labs)
# write.csv(adult.labs,"RUS_2014/adult.labs.csv")

child <- read.dta("RUS_2014/child2014w.dta")
child.labs <- data.frame(names(child),attributes(child)[7])
# View(child.labs)
# write.csv(child.labs,"RUS_2014/child.labs.csv")

catvars <- c(
  345
  ,347
  ,348
  ,352:359
  ,seq(361,420,2)
  )
numvars <- c(
  346
  ,349
  ,350
  ,351
  ,seq(360,416,2)
  )

#Demean numeric vars
for(i in 1:length(numvars)){
  var <- numvars[i]
  hh[,var] <- hh[,var]-mean(hh[,var],na.rm=TRUE)
}

hh$urban.cat <- NA
hh$urban.cat[which(hh$urban=="urban")] <- 1
hh$urban.cat[which(hh$urban=="pgt")] <- 0
hh$urban.cat[which(hh$urban=="rural")] <- 0

hh <- data.frame(hh)
urbanvar <- "urban.cat"

wealth.dat <- wealth(hh,catvars,numvars,"urban.cat")

# hist(wealth.dat$wealth)
# hist(wealth.dat$totexprw)
# hist(wealth.dat$tincm_rw)

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

ex.rate <- (72.3917/1275.53)
hc <- 0.00157659
pov.line <- 72.3917

wealth.dat$pcexpppp <- (wealth.dat$totexprw*ex.rate)/wealth.dat$hwnfmemn
wealth.dat$pcincppp <- (wealth.dat$tincm_rw*ex.rate)/wealth.dat$hwnfmemn
wealth.dat$epoor <- wealth.dat$pcexpppp<pov.line
wealth.dat$ipoor <- wealth.dat$pcincppp<pov.line

povperc <- weighted.percentile(wealth.dat$wealth,wealth.dat$hhwgt_w,prob=hc)
wealth.dat$wpoor <- wealth.dat$wealth<povperc

e = round(weighted.mean(wealth.dat$epoor,wealth.dat$hhwgt_w,na.rm=TRUE)*100,2)
i = round(weighted.mean(wealth.dat$ipoor,wealth.dat$hhwgt_w,na.rm=TRUE)*100,2)
w = round(weighted.mean(wealth.dat$wpoor,wealth.dat$hhwgt_w,na.rm=TRUE)*100,2)

ei = round(weighted.mean(wealth.dat$epoor&wealth.dat$ipoor,wealth.dat$hhwgt_w,na.rm=TRUE)*100,2)
iw = round(weighted.mean(wealth.dat$ipoor&wealth.dat$wpoor,wealth.dat$hhwgt_w,na.rm=TRUE)*100,2)
we = round(weighted.mean(wealth.dat$wpoor&wealth.dat$epoor,wealth.dat$hhwgt_w,na.rm=TRUE)*100,2)

eiw = round(weighted.mean(wealth.dat$epoor&wealth.dat$ipoor&wealth.dat$wpoor,wealth.dat$hhwgt_w,na.rm=TRUE)*100,2)

# install.packages('VennDiagram')
library(VennDiagram)
grid.newpage()
draw.triple.venn(
  area1=e
  ,area2=i
  ,area3=w
  ,n12=ei
  ,n23=iw
  ,n13=we
  ,n123=eiw
  ,category=c("Expenditure poor","Income poor","Wealth poor")
  ,lty=rep("blank",3)
  ,fill=c("red","green","blue")
  )
