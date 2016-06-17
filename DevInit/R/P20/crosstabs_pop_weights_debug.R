library(Hmisc)
library(plyr)
library(foreign)
library(data.table)
library(descr)
library(varhandle)
library(WDI)

# Stop crosstab from plotting everything
options(descr.plot = FALSE)

wd <- "D:/Documents/Data/MICSmeta"
setwd(wd)
load("total_crosstabs_coded.RData")
load("D:/Documents/Data/ChinaSurvey/crosstab.RData")
data.total <- rbind(data.total,china.data.total,fill=TRUE)

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
  middle.end <- props*pop
  high.end <- (props+(2*corrected.SEs))*pop
  high.end <- pmin(high.end,pop)
  return(
    list(
      low = low.end
      ,middle = middle.end
      ,high = high.end
    )
  )
}

countryMeta <- read.csv("headcounts.csv",as.is=TRUE)

indicator <- "SP.POP.TOTL"

pop <- WDI(country = "all", 
           indicator = indicator, 
           start = 2000, 
           end = 2015,
           extra = TRUE
)

keep <- c("iso3c","year","SP.POP.TOTL")
pop <- pop[keep]
names(pop) <- c("iso3","year","pop")

countryMeta <- join(
  countryMeta
  ,pop
  ,by=c("iso3","year")
)

countryMeta[which(countryMeta$iso3=="KEN"),]$pop <- 44863583
countryMeta[which(countryMeta$iso3=="RWA"),]$pop <- 11341544
countryMeta[which(countryMeta$iso3=="TMP"),]$pop <- 1048367
countryMeta[which(countryMeta$iso2=="XK"),]$pop <- 1823149

####Debug####
dat.tab <- data.table(data.total)
dat.collapse <- dat.tab[
  ,.(p20=weighted.mean(p20,weights,na.rm=TRUE))
  , by=.(filename)
  ]
countryMeta <- join(countryMeta,dat.collapse,by="filename")
####End debug####

countryMeta$tab.p20 <- NA

filenames <- countryMeta$filename
for(i in 1:length(filenames)){
  this.filename <- filenames[i]
  message(this.filename)
  dat <- subset(data.total,filename==this.filename)
  if(nrow(dat)>0){
    this.pop <- subset(countryMeta,filename==this.filename)$pop
    #Urban-P20
    if(length(dat$urban[which(!is.na(dat$urban))])!=0){
      confidence.tab <- pop.confidence(dat$urban,dat$p20,dat$weights,this.pop)
      countryMeta$tab.p20[which(countryMeta$filename==this.filename)] <- sum((confidence.tab$middle/this.pop)[,2])
    }
  }
}

write.csv(countryMeta,"hc_vs_p20.csv")