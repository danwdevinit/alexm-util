library(Hmisc)
library(data.table)
library(foreign)
library(descr)
library(plyr)

setwd("D:/Documents/Data/DHSmeta")

cwi <- read.csv("global_cwi.csv",na.strings="",as.is=TRUE)

# cwi <- read.csv("global_ccwi2.csv",na.strings="",as.is=TRUE)
# setnames(cwi,"cwi","cwi.urb.rur")
# setnames(cwi,"ccwi","cwi")

# cwi <- read.csv("global_ccwi.csv",na.strings="",as.is=TRUE)
# setnames(cwi,"ccwi","cwi")

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

cwi.table <- data.table(cwi)

cwi.collapse <- cwi.table[
  ,.(mean.cwi=weighted.mean(cwi,weights,na.rm=TRUE)
     ,median.cwi=weighted.percentile(cwi,weights,.5)
     ,filename=max(filename))
  , by=.(iso2,phase)]

isos <- unique(cwi.collapse$iso2)

specials_to_avoid <- c(
  "bfhr70dt"
  ,"buhr6hdt"
  )

latest_surveys <- c()
for(i in 1:length(isos)){
  iso <- isos[i]
  sub <- subset(cwi.collapse,iso2==iso)
  sub <- sub[order(-sub$phase)]
  latest <- sub$filename[1]
  j <- 2
  while(latest %in% specials_to_avoid){
    latest <- sub$filename[j]
    j <- j + 1
  }
  latest_surveys <- c(latest_surveys,latest)
}

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
write.csv(data,"depth_of_cwi.csv",row.names=FALSE,na="")
