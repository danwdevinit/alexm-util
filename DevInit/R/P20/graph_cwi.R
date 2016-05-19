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

cwi <- read.csv("global_cwi_replication.csv",na.strings="",as.is=TRUE)

cwi.table <- data.table(cwi)

cwi.collapse <- cwi.table[
  ,.(mean.cwi=mean(cwi,na.rm=TRUE)
  ,median.cwi=median(cwi,na.rm=TRUE))
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

d$save("gni_interactive.html", cdn = TRUE)
