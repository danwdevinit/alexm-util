####Function and setup####
library(Hmisc)
library(plyr)
library(foreign)
library(data.table)

pvals <- c()
filenames <- c()

setwd("D:/Documents/Data/DHSmeta/")
classes <- read.csv("global_cwi_classes.csv",na.strings=c("","NAN"),as.is=TRUE)

tl.cuts <- read.csv("D:/Documents/Data/DHSauto/tlhr61dt/cuts.csv")$cuts

tl.data <- read.csv("D:/Documents/Data/DHSauto/tlhr61dt/wealth.csv")
# tl.cuts[1] <- mean(tl.data[which(tl.data$car==1),]$wealth,na.rm=TRUE)
# tl.cuts[2] <- mean(tl.data[which(tl.data$fridge==1),]$wealth,na.rm=TRUE)
# tl.cuts[3] <- mean(tl.data[which(tl.data$phone==1),]$wealth,na.rm=TRUE)
# tl.cuts[4] <- mean(tl.data[which(tl.data$tv==1),]$wealth,na.rm=TRUE)
# tl.cuts[5] <- mean(tl.data[which(tl.data$ubn>=4),]$wealth,na.rm=TRUE)
# tl.cuts[6] <- mean(tl.data[which(tl.data$ubn>=3),]$wealth,na.rm=TRUE)
# tl.cuts[7] <- mean(tl.data[which(tl.data$ubn>=2),]$wealth,na.rm=TRUE)
# tl.cuts[8] <- mean(tl.data[which(tl.data$ubn>=1),]$wealth,na.rm=TRUE)
tl.percents <- prop.table(table(tl.data$ubn))
tl.cum.percents <- c(
  sum(tl.percents[5:2],na.rm=TRUE)
  ,sum(tl.percents[5:3],na.rm=TRUE)
  ,sum(tl.percents[5:4],na.rm=TRUE)
  ,sum(tl.percents[5],na.rm=TRUE)
)
tl.cum.percents[which(tl.cum.percents==0)] <- NA

tl.cuts[5:8] <- quantile(tl.data$wealth,tl.cum.percents,na.rm=TRUE)


wd <- "D:/Documents/Data/DHSauto/"
setwd(wd)

# List out all the directories in our wd, this is where our data is contained
dirs <- list.dirs(wd,full.names=TRUE)

dataList <- list()
dataIndex <- 1

# Loop through every dir
for(i in 2:length(dirs)){
  # for(i in 1209:length(dirs)){
  dir <- dirs[i]
  # Pull some coded info out of the dir name
  country <- tolower(substr(basename(dir),1,2))
  recode <- tolower(substr(basename(dir),3,4))
  phase <- as.integer(substr(basename(dir),5,5))
  # For this analysis, we're only interested in individual member recodes, or "hr"
  if(recode=="hr" & phase>=5){
    message(basename(dir))
    files <- list.files(dir)
    if("wealth.csv" %in% files){
      data <- read.csv(paste0(dir,"/wealth.csv"),na.strings="",as.is=TRUE)
      cuts <- read.csv(paste0(dir,"/cuts.csv"),na.strings="",as.is=TRUE)$cuts
#       cuts[1] <- mean(data[which(data$car==1),]$wealth,na.rm=TRUE)
#       cuts[2] <- mean(data[which(data$fridge==1),]$wealth,na.rm=TRUE)
#       cuts[3] <- mean(data[which(data$phone==1),]$wealth,na.rm=TRUE)
#       cuts[4] <- mean(data[which(data$tv==1),]$wealth,na.rm=TRUE)
#       cuts[5] <- mean(data[which(data$ubn>=4),]$wealth,na.rm=TRUE)
#       cuts[6] <- mean(data[which(data$ubn>=3),]$wealth,na.rm=TRUE)
#       cuts[7] <- mean(data[which(data$ubn>=2),]$wealth,na.rm=TRUE)
#       cuts[8] <- mean(data[which(data$ubn>=1),]$wealth,na.rm=TRUE)
      percents <- prop.table(table(data$ubn))
      cum.percents <- c(
        sum(percents[5:2],na.rm=TRUE)
        ,sum(percents[5:3],na.rm=TRUE)
        ,sum(percents[5:4],na.rm=TRUE)
        ,sum(percents[5],na.rm=TRUE)
      )
      cum.percents[which(cum.percents==0)] <- NA
      cuts[5:8] <- quantile(data$wealth,cum.percents,na.rm=TRUE)
      cut.lm <- lm(tl.cuts~cuts)
      pvals <- c(pvals,summary(cut.lm)$coefficients[,4][[2]])
      filenames <- c(filenames,basename(dir))
      alpha <- cut.lm$coefficients[[1]]
      beta <- cut.lm$coefficients[[2]]
      data$cwi <- alpha+(beta*data$wealth)
      dataList[[dataIndex]] <- data
      dataIndex <- dataIndex + 1
    }
  }
}

wd <- "D:/Documents/Data/DHSmeta"
setwd(wd)
metaData <- rbindlist(dataList,fill=TRUE)

# set our working directory, change this if using on another machine
wd <- "D:/Documents/Data/MICSauto/"
setwd(wd)

# List out all the directories in our wd, this is where our data is contained
dirs <- list.dirs(wd,full.names=TRUE)

dataList <- list()
dataIndex <- 1

for(i in 2:length(dirs)){
  dir <- dirs[i]
  if(file.exists(paste0(dir,"/cwi.RData"))){
    message(basename(dir))
    if(exists("data")){rm(data)}
    if(exists("cut.df")){rm(cut.df)}
    load(paste0(dir,"/cwi.RData"))
    cuts <- cut.df$cuts
#     cuts[1] <- mean(data[which(data$car==1),]$wealth,na.rm=TRUE)
#     cuts[2] <- mean(data[which(data$fridge==1),]$wealth,na.rm=TRUE)
#     cuts[3] <- mean(data[which(data$phone==1),]$wealth,na.rm=TRUE)
#     cuts[4] <- mean(data[which(data$tv==1),]$wealth,na.rm=TRUE)
#     cuts[5] <- mean(data[which(data$ubn>=4),]$wealth,na.rm=TRUE)
#     cuts[6] <- mean(data[which(data$ubn>=3),]$wealth,na.rm=TRUE)
#     cuts[7] <- mean(data[which(data$ubn>=2),]$wealth,na.rm=TRUE)
#     cuts[8] <- mean(data[which(data$ubn>=1),]$wealth,na.rm=TRUE)
    percents <- prop.table(table(data$ubn))
    cum.percents <- c(
      sum(percents[5:2],na.rm=TRUE)
      ,sum(percents[5:3],na.rm=TRUE)
      ,sum(percents[5:4],na.rm=TRUE)
      ,sum(percents[5],na.rm=TRUE)
    )
    cum.percents[which(cum.percents==0)] <- NA
    cuts[5:8] <- quantile(data$wealth,cum.percents,na.rm=TRUE)
    cut.lm <- lm(tl.cuts~cuts)
    pvals <- c(pvals,summary(cut.lm)$coefficients[,4][[2]])
    filenames <- c(filenames,basename(dir))
    alpha <- cut.lm$coefficients[[1]]
    beta <- cut.lm$coefficients[[2]]
    data$cwi <- alpha+(beta*data$wealth)
    dataList[[dataIndex]] <- data
    dataIndex <- dataIndex + 1 
  }
}

wd <- "D:/Documents/Data/MICSmeta"
setwd(wd)
mics.cwi <- rbindlist(dataList,fill=TRUE)
cwi <- metaData

# save(mics.cwi,cwi,file="cwi_replication.RData")
pval.df <- data.frame(filenames,pvals)
pval.df <- pval.df[order(pval.df$pvals),]
write.csv(pval.df,"pvals_replication_mean.csv",row.names=FALSE,na="")

library(Hmisc)
library(data.table)
library(foreign)
library(descr)
library(plyr)

setwd("D:/Documents/Data/DHSmeta")

cwi$sample.weights <- cwi$sample.weights/1000000

mics.cwi$iso2 <- NA

data <- rbind(cwi,mics.cwi)
data <- data.frame(data)
data$weights <- data$sample.weights
data <- data[complete.cases(data$weights),]
data <- data[complete.cases(data$cwi),]

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

latest_surveys <- c(
  "alhr50dt", "amhr61dt", "aohr61dt", "azhr52dt", "bdhr70dt", "bfhr70dt"
  ,"bjhr61dt", "bohr51dt", "buhr61dt", "cdhr61dt", "cghr60dt"
  ,"cihr61dt", "cmhr60dt", "cohr61dt", "drhr61dt", "eghr61dt"
  ,"ethr61dt", "gahr60dt", "ghhr70dt", "gmhr60dt", "gnhr61dt", "gyhr5idt"
  ,"hnhr62dt", "hthr61dt", "iahr52dt", "idhr63dt", "johr6cdt"
  ,"kehr7hdt","khhr72dt", "kmhr61dt"
  #   , "kyhr61dt"
  , "lbhr6adt", "lshr61dt"
  #   ,"mbhr53dt"
  , "mdhr6hdt", "mlhr6hdt", "mvhr51dt", "mwhr71dt"
  ,"mzhr62dt", "nghr6adt", "nihr61dt", "nmhr61dt"
  # , "nphr60dt"
  ,"pehr6idt","phhr61dt","pkhr61dt"
  ,"rwhr70dt","slhr61dt","snhr70dt", "sthr50dt"
  # , "szhr51dt"
  ,"tghr61dt", "tjhr61dt", "tlhr61dt","tzhr6adt"
  # , "uahr51dt"
  ,"ughr72dt"
  # , "vnhr52dt"
  , "yehr61dt", "zmhr61dt"
  # , "zwhr62dt"
  #MICS
  ,"Afghanistan_MICS4_Datasets","Algeria_MICS4_Datasets"
  ,"Barbados_MICS4_Datasets","Belarus_MICS4_Datasets"
  ,"Belize_MICS4_Datasets","Bhutan_MICS4_Datasets"
  ,"Bosnia and Herzegovina_MICS4_Datasets","Central African Republic_MICS4_Datasets"
  ,"Chad_MICS4_Datasets","Costa Rica_MICS4_Datasets","Georgia MICS 2005 SPSS Datasets"
  ,"Guinea-Bissau MICS 2006 SPSS Datasets","Iraq_MICS4_Datasets","Jamaica_MICS4_Datasets"
  ,"Kazakhstan_MICS4_Datasets","Kosovo under UNSC res. 1244_MICS5_Datasets"
  ,"Kyrgyzstan MICS5 Datasets","Lao People's Democratic Republic_LSIS_Datasets"
  #   ,"Lebanon (Palestinians)_MICS4_Datasets"
  ,"Macedonia, The former Yugoslav Republic of_MICS4_Datasets","Mauritania_MICS4_Datasets"
  ,"Moldova_MICS4_Datasets","Mongolia_MICS5_Datasets","Montenegro_MICS5_Datasets"
  ,"Nepal_MICS5_Datasets"
  #   ,"Pakistan (Punjab)_MICS5_Datasets"
  ,"Serbia_MICS5_Datasets"
  #   ,"Somalia (Northeast Zone)_MICS4_Datasets"                                             
  #   ,"Somalia (Somaliland)_MICS4_Datasets"                                                 
  ,"Somalia MICS 2006 SPSS Datasets" 
  ,"South Sudan_MICS4_Datasets"
  ,"Sudan_MICS5_Datasets"
  ,"St.Lucia_MICS4_Datasets","State of Palestine_MICS5_Datasets","Suriname_MICS4_Datasets"
  ,"Swaziland_MICS4_Datasets","Syria MICS 2006 SPSS Datasets","Thailand_MICS4_Datasets"
  ,"Trinidad and Tobago MICS 2006 SPSS Datasets","Tunisia_MICS4_Datasets"
  ,"Turkmenistan_MICS3_Datasets","Ukraine_MICS4_Datasets","Uruguay_MICS4_Datasets"
  ,"Uzbekistan MICS 2006 SPSS Datasets","Vanuatu MICS 2007 SPSS Datasets","Viet Nam_MICS5_Datasets"
  ,"Zimbabwe_MICS5_Datasets"
)

cwi <- subset(data,filename %in% latest_surveys)
cwi <- cwi[order(cwi$cwi),]

quints <- c(-0.06008803)
names(quints) <- c("20%")
cwi$quint.20 <- (cwi$cwi <= -0.06008803)

cwi.table <- data.table(cwi)

cwi.collapse <- cwi.table[
  ,.(p20=weighted.mean(quint.20,weights,na.rm=TRUE))
  , by=.(filename)]

p20.table <- data.table(subset(cwi,quint.20==TRUE))

p20.collapse <- p20.table[
  ,.(pov.gap=weighted.mean((quints[["20%"]]-cwi),weights,na.rm=TRUE)
     ,pov.gap.sqr=weighted.mean((quints[["20%"]]-cwi)*(quints[["20%"]]-cwi),weights,na.rm=TRUE))
  ,by=.(filename)]

data <- join(cwi.collapse,p20.collapse,by="filename")

setwd("D:/Documents/Data/DHSmeta")
write.csv(data,"cwi_depth_means_and_238.csv",row.names=FALSE,na="")
