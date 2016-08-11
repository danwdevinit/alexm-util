library(foreign)
library(Hmisc)
library(data.table)
library(plyr)
library(varhandle)

source("C:/git/alexm-util/DevInit/R/p20/wealth_pca.R")

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
wealth.dat.no.home <- wealth(hh,catvars,NA,"urban.cat")

hist(wealth.dat$wealth)
hist(wealth.dat.no.home$wealth)
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

#Regression?
dummies <- c(1176:1262)
reg.dat <- cbind(wealth.dat$pcexpppp,wealth.dat$pcincppp,wealth.dat[dummies],wealth.dat[numvars])
names(reg.dat)[1:2] <- c("pcexpppp","pcincppp")
fit <- lm(reg.dat$pcexpppp~.,data=reg.dat)
summary(fit)
fit <- lm(reg.dat$pcexpppp~hwhord1+hwhord2+hwobicyc1+hwhfsp+hwhlsp+hwhrooms,data=reg.dat)
summary(fit)
fit <- lm(reg.dat$pcincppp~.,data=reg.dat)
summary(fit)
