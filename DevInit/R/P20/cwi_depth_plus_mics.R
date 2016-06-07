library(Hmisc)
library(data.table)
library(foreign)
library(descr)
library(plyr)

setwd("D:/Documents/Data/DHSmeta")

cwi <- read.csv("global_cwi.csv",na.strings="",as.is=TRUE)
fileName <- "depth_of_cwi_plus_mics_238.csv"

cwi$source <- "DHS"
cwi$sample.weights <- cwi$sample.weights/1000000

load("D:/Documents/Data/MICSmeta/global_mics_cwi.RData")

mics.cwi$source <- "MICS"
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

# quints <- weighted.percentile(cwi$cwi,cwi$weights,prob=seq(0,1,length=6))
# 
# for(i in 2:length(quints)){
#   quint <- quints[i]
#   quintName <- paste0("quint.",(i-1)*20)
#   cwi[[quintName]] <- (cwi$cwi <= quint)
# }
# 
# decs <- weighted.percentile(cwi$cwi,cwi$weights,prob=seq(0,1,length=11))
# cwi$dec.50 <- (cwi$cwi <= decs[6])

quints <- c(0.1804672)
names(quints) <- c("20%")
cwi$quint.20 <- (cwi$cwi <= 0.1804672)

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
write.csv(data,fileName,row.names=FALSE,na="")
