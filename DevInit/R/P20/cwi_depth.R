library(Hmisc)
library(data.table)
library(foreign)
library(descr)
library(plyr)

setwd("D:/Documents/Data/DHSmeta")

# cwi <- read.csv("global_cwi.csv",na.strings="",as.is=TRUE)

cwi <- read.csv("global_ccwi2.csv",na.strings="",as.is=TRUE)
setnames(cwi,"cwi","cwi.urb.rur")
setnames(cwi,"ccwi","cwi")

cwi <- cwi[order(cwi$cwi),]
# fit <- lm(cwi~ubn+tv+phone+fridge+car,data=cwi)
# summary(fit)

cwi$weights <- cwi$sample.weights/1000000

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

cwi$phase <- substr(cwi$filename,5,5)

latest_surveys <- c(
  "alhr50dt", "amhr61dt", "aohr61dt", "azhr52dt", "bdhr70dt", "bfhr70dt"
  ,"bjhr61dt", "bohr51dt", "buhr61dt", "cdhr61dt", "cghr60dt"
  ,"cihr61dt", "cmhr60dt", "cohr61dt", "drhr61dt", "eghr61dt"
  ,"ethr61dt", "gahr60dt", "ghhr70dt", "gmhr60dt", "gnhr61dt", "gyhr5idt"
  ,"hnhr62dt", "hthr61dt", "iahr52dt", "idhr63dt", "johr6cdt"
  ,"kehr7hdt","khhr72dt", "kmhr61dt", "kyhr61dt", "lbhr6adt", "lshr61dt"
  ,"mbhr53dt", "mdhr6hdt", "mlhr6hdt", "mvhr51dt", "mwhr71dt"
  ,"mzhr62dt", "nghr6adt", "nihr61dt", "nmhr61dt", "nphr60dt"
  ,"pehr6idt","phhr61dt","pkhr61dt"
  ,"rwhr70dt","slhr61dt","snhr70dt", "sthr50dt", "szhr51dt"
  ,"tghr61dt", "tjhr61dt", "tlhr61dt","tzhr6adt", "uahr51dt"
  ,"ughr72dt", "vnhr52dt", "yehr61dt", "zmhr61dt", "zwhr62dt"
  )

cwi <- subset(cwi,filename %in% latest_surveys)

quints <- weighted.percentile(cwi$cwi,cwi$weights,prob=seq(0,1,length=6))

for(i in 2:length(quints)){
  quint <- quints[i]
  quintName <- paste0("quint.",(i-1)*20)
  cwi[[quintName]] <- (cwi$cwi <= quint)
}

decs <- weighted.percentile(cwi$cwi,cwi$weights,prob=seq(0,1,length=11))
cwi$dec.50 <- (cwi$cwi <= decs[6])

cwi.table <- data.table(cwi)

cwi.collapse <- cwi.table[
  ,.(p20=weighted.mean(quint.20,weights,na.rm=TRUE))
  , by=.(iso2,phase)]

p20.table <- data.table(subset(cwi,quint.20==TRUE))

p20.collapse <- p20.table[
  ,.(pov.gap=weighted.mean((quints[["20%"]]-cwi),weights,na.rm=TRUE)
     ,pov.gap.sqr=weighted.mean((quints[["20%"]]-cwi)*(quints[["20%"]]-cwi),weights,na.rm=TRUE))
  ,by=.(iso2)]

data <- join(cwi.collapse,p20.collapse,by="iso2")

setwd("D:/Documents/Data/DHSmeta")
write.csv(data,"depth_of_ccwi.csv",row.names=FALSE,na="")
