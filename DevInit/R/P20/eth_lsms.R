library(foreign)
library(data.table)
library(plyr)
library(Hmisc)

wd <- "D:/Documents/Data/LSMSauto/ETH_2011_ERSS_v02_M_STATA"

setwd(wd)

dat <- read.dta("cons_agg_w1.dta",convert.factors=FALSE)
var.labs <- data.frame(names(dat),attributes(dat)[7])
# View(var.labs)

ex.rate <- (72.3917/344.707)
pov.line <- 72.3917

dat <- transform(dat,pcexpppp=((total_cons_ann/hh_size)*ex.rate)/12)
dat <- transform(dat,poverty=pcexpppp<pov.line)
keep <- c("household_id","saq01","rural","pw","hh_size","poverty")
dat <- dat[keep]

geo <- read.dta("Pub_ETH_HouseholdGeovariables_Y1.dta")
geo.labs <- data.frame(names(geo),attributes(geo)[7])
# View(geo.labs)

assets <- read.dta("sect10_hh_w1.dta")
asset.labs <- data.frame(names(assets),attributes(assets)[7])
# View(asset.labs)
# write.csv(asset.labs,"asset.vars.csv")
keep <- c("household_id","hh_s10q00","hh_s10q01")
assets <- assets[keep]
assets.wide <- reshape(assets,idvar="household_id",timevar="hh_s10q00",direction="wide")
names(assets.wide)[3:length(assets.wide)] <- sapply(names(assets.wide)[3:length(assets.wide)],substring,first=11)
names(assets.wide) <- make.names(names(assets.wide),unique=TRUE)

dat <- join(dat,assets.wide,by="household_id")
dat <- join(dat,geo,by="household_id")

dat <- data.table(dat)
dat[,c("household_id","pw","ea_id","LAT_DD_MOD","LON_DD_MOD"):=NULL]
dat <- data.frame(dat)

dat$poverty[which(dat$poverty==TRUE)] <- 1
dat$poverty[which(dat$poverty==FALSE)] <- 0

for(i in 1:length(dat)){
  column = dat[[i]]
  if(is.factor(column)){
    dat[[i]] <- as.integer(column)
  }
}

dat <- dat[c(4,1:3,5:length(dat))]

write.csv(dat,"eth_dat.csv",row.names=FALSE,na="")
fit <- glm(poverty~.,data=dat,family="binomial")
summary(fit)
# install.packages("pscl")
library("pscl")
pR2(fit)
