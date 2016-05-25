library(Hmisc)
library(ggplot2)
library(data.table)
library(WDI)
library(rCharts)

setwd("D:/Documents/Data/DHSmeta")

indicator <- "NY.GNP.PCAP.PP.KD"

gni <- WDI(country = "all", 
           indicator = indicator, 
           start = 2000, 
           end = 2015,
           extra = TRUE
)

cwi <- read.csv("global_cwi.csv",na.strings="",as.is=TRUE)

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

cwi.table <- data.table(cwi)

cwi.collapse <- cwi.table[
  ,.(mean.cwi=weighted.mean(cwi,weights,na.rm=TRUE)
  ,median.cwi=weighted.percentile(cwi,weights,.5))
  , by=.(iso2,year)]

setnames(cwi.collapse,"iso2","iso2c")

cwi.collapse[which(cwi.collapse$iso2c=="LB")]$iso2c <- "LR"

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
d$set(legend = list(x=510,y=40,height=100,width=100))
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

cwi$monetized.log <- fit$coefficients[[1]]+(fit$coefficients[[2]]*cwi$cwi)
cwi$monetized <- exp(cwi$monetized.log)

fit <- lm(median.cwi~log.gni,data=data.no.na)
summary(fit)

plot(mean.cwi~NY.GNP.PCAP.PP.KD,data=data.no.na)
abline(lm(mean.cwi~NY.GNP.PCAP.PP.KD,data=data.no.na))

plot(mean.cwi~log.gni,data=data.no.na)
abline(lm(mean.cwi~log.gni,data=data.no.na))

d$save("gni_interactive.html", cdn = TRUE)

mon.quints <- weighted.percentile(cwi$monetized,cwi$weights,seq(0,1,length=6))

mon.quints[2]
mon.quints[3]-mon.quints[2]
mon.quints[4]-mon.quints[3]
mon.quints[5]-mon.quints[4]
