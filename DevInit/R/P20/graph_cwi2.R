library(Hmisc)
library(ggplot2)
library(data.table)
library(WDI)
library(rCharts)
library(plyr)

load("D:/Documents/Data/MICSmeta/cwi_final.RData")
# wd <- "D:/Documents/Data/DHSmeta"
# setwd(wd)
# cwi <- read.csv("global_cwi.csv",na.strings="",as.is=TRUE)
# wd <- "D:/Documents/Data/MICSmeta"
# setwd(wd)
# load("global_mics_cwi.RData")


indicator <- "NY.GNP.PCAP.PP.KD"

gni <- WDI(country = "all", 
           indicator = indicator, 
           start = 2000, 
           end = 2015,
           extra = TRUE
)

cwi$weights <- cwi$sample.weights/1000000

cwi$iso2[which(cwi$iso2=="BU")] <- "BI"
cwi$iso2[which(cwi$iso2=="DR")] <- "DO"
cwi$iso2[which(cwi$iso2=="IA")] <- "IN"
cwi$iso2[which(cwi$iso2=="KY")] <- "KG"
cwi$iso2[which(cwi$iso2=="LB")] <- "LR"
cwi$iso2[which(cwi$iso2=="MD")] <- "MG"
cwi$iso2[which(cwi$iso2=="MB")] <- "MD"
cwi$iso2[which(cwi$iso2=="NM")] <- "NA"
cwi$iso2[which(cwi$iso2=="NI")] <- "NE"

mics_isos <- read.csv("D:/Documents/Data/MICSmeta/isos.csv")

mics.cwi <- join(
  mics.cwi
  ,mics_isos
  ,by="filename"
)

mics.cwi <- subset(mics.cwi,!is.na(iso2))

mics.cwi$weights <- mics.cwi$sample.weights

data <- rbind(cwi,mics.cwi)
data <- data.frame(data)

data$year <- NULL

all.years <- read.csv("D:/Documents/Data/MICSmeta/all.years.csv")

data <- join(
  data
  ,all.years
  ,by="filename"
  )

cwi <- data

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

cwi.table <- data.table(cwi)

cwi.collapse <- cwi.table[
  ,.(mean.cwi=weighted.mean(cwi,weights,na.rm=TRUE)
     ,median.cwi=weighted.percentile(cwi,weights,.5))
  , by=.(iso2,year)]

setnames(cwi.collapse,"iso2","iso2c")

data <- join(
  cwi.collapse
  ,gni
  ,by=c("iso2c","year")
)

diColors <- c("#ba0c2f" #Red
              ,"#1b365d" #blue
              ,"#ea7600" #Orange
              ,"#93328e" #purple
              ,"#0095c8" #lightblue
              ,"#b7bf10" #Yellow
)

p <- ggplot(data,aes(x=mean.cwi,y=NY.GNP.PCAP.PP.KD,colour=region)) +
  geom_point(size=3) +
  scale_colour_manual(values=diColors)
p
p2 <- ggplot(data,aes(x=median.cwi,y=NY.GNP.PCAP.PP.KD,colour=region)) +
  geom_point(size=3) +
  scale_colour_manual(values=diColors)
p2

data.no.na <- data[complete.cases(data),]
d <- dPlot(
  y = "NY.GNP.PCAP.PP.KD",
  x = "mean.cwi",
  groups = c("country","iso2c","year","region"),
  data = data.no.na,
  type = "bubble",
  bounds = list(x = 50, y = 50, height = 600, width = 700),
  height = 750,
  width = 800
)
d$defaultColors(diColors)
d$xAxis( type = "addMeasureAxis")
d$yAxis( type = "addMeasureAxis")
# d$set(legend = list(x=510,y=40,height=100,width=100))
d$setTemplate(afterScript = "
              <script>
              myChart.draw()
              myChart.axes[0].titleShape.text('Mean CWI')
              myChart.axes[1].titleShape.text('GNI per capita (Constant 2011 PPP)')
              myChart.svg.append('text')
              .attr('x', 250)
              .attr('y', 20)
              .text('CWI vs. GNI per capita (DHS phase 6)')
              .style('text-anchor','beginning')
              .style('font-size', '100%')
              .style('font-family','sans-serif')
              </script>               
              ")
d

data.no.na$log.gni <- log(data.no.na$NY.GNP.PCAP.PP.KD)

fit <- lm(mean.cwi~NY.GNP.PCAP.PP.KD,data=data.no.na)
summary(fit)

fit <- lm(median.cwi~NY.GNP.PCAP.PP.KD,data=data.no.na)
summary(fit)

fit <- lm(mean.cwi~log.gni,data=data.no.na)
summary(fit)

fit <- lm(log.gni~mean.cwi,data=data.no.na)
summary(fit)

fit <- lm(median.cwi~log.gni,data=data.no.na)
summary(fit)

plot(mean.cwi~NY.GNP.PCAP.PP.KD,data=data.no.na)
abline(lm(mean.cwi~NY.GNP.PCAP.PP.KD,data=data.no.na))

plot(mean.cwi~log.gni,data=data.no.na)
abline(lm(mean.cwi~log.gni,data=data.no.na))

d$save("gni_interactive.html", cdn = TRUE)

###Tomfoolery below this line

# #Monetize CWI? Or reverse monetize cutpoint?
# fit <- lm(log.gni~mean.cwi,data=data.no.na)
# summary(fit)
# 
# cwi$monetized.log <- fit$coefficients[[1]]+(fit$coefficients[[2]]*cwi$cwi)
# cwi$monetized <- exp(cwi$monetized.log)/365
# 
# monetary.cut <- 2.38
# 
# cwi$monetary.p20 <- cwi$monetized<=monetary.cut

fit <- lm(mean.cwi~log.gni,data=data.no.na)
summary(fit)

monetary.cut <- log(2.38*365)

cwi.cut <- fit$coefficients[[1]]+(fit$coefficients[[2]]*(monetary.cut))

cwi$monetary.p20 <- cwi$cwi<=cwi.cut

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
cwi <- cwi[order(cwi$cwi),]

cwi.table <- data.table(cwi)

cwi.collapse <- cwi.table[
  ,.(p20=weighted.mean(monetary.p20,weights,na.rm=TRUE))
  , by=.(iso2,phase)]

p20.table <- data.table(subset(cwi,monetary.p20==TRUE))

# p20.collapse <- p20.table[
#   ,.(pov.gap=weighted.mean((monetary.cut-monetized),weights,na.rm=TRUE)
#      ,pov.gap.sqr=weighted.mean((monetary.cut-monetized)*(monetary.cut-monetized),weights,na.rm=TRUE))
#   ,by=.(iso2)]

p20.collapse <- p20.table[
  ,.(pov.gap=weighted.mean((cwi.cut-cwi),weights,na.rm=TRUE)
     ,pov.gap.sqr=weighted.mean((cwi.cut-cwi)*(cwi.cut-cwi),weights,na.rm=TRUE))
  ,by=.(iso2)]

data <- join(cwi.collapse,p20.collapse,by="iso2")

data[which(data$iso2=="BU")]$iso2 <- "BI"
data[which(data$iso2=="DR")]$iso2 <- "DO"
data[which(data$iso2=="IA")]$iso2 <- "IN"
data[which(data$iso2=="KY")]$iso2 <- "KG"
data[which(data$iso2=="LB")]$iso2 <- "LR"
data[which(data$iso2=="MD")]$iso2 <- "MG"
data[which(data$iso2=="MB")]$iso2 <- "MD"
data[which(data$iso2=="NM")]$iso2 <- "NA"
data[which(data$iso2=="NI")]$iso2 <- "NE"

setwd("D:/Documents/Data/DHSmeta")
# write.csv(data,"cwi_monetized_pov_depth.csv",row.names=FALSE,na="")
write.csv(data,"cwi_238_pov_depth.csv",row.names=FALSE,na="")
