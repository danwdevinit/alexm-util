library(Hmisc)
library(ggplot2)
library(data.table)
library(rCharts)

setwd("D:/Documents/Data/")

mpi <- read.csv("mpi_living.csv",na.strings="",as.is=TRUE)

cwi <- read.csv("DHSmeta/global_cwi.csv",na.strings="",as.is=TRUE)

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

cwi.collapse[which(cwi.collapse$iso2c=="LB")]$iso2c <- "LR"

data1 <- merge(
  cwi.collapse
  ,mpi
  ,by=c("iso2","year")
)

setnames(cwi.collapse,"year","year2")

data2 <- merge(
  cwi.collapse
  ,mpi
  ,by=c("iso2","year2")
)

data <- rbind(data1,data2)
data <- data.frame(data)
  
diColors <- c("#ba0c2f" #Red
              ,"#1b365d" #blue
              ,"#ea7600" #Orange
              ,"#93328e" #purple
              ,"#0095c8" #lightblue
              ,"#b7bf10" #Yellow
)

p <- ggplot(data,aes(x=mean.cwi,y=assets)) +
  geom_point(size=3) +
  scale_colour_manual(values=diColors)
p
p2 <- ggplot(data,aes(x=mean.cwi,y=deprived.std.living)) +
  geom_point(size=3) +
  scale_colour_manual(values=diColors)
p2

d <- dPlot(
  y = "assets",
  x = "mean.cwi",
  groups = c("country","iso2c","year"),
  data = data,
  type = "bubble",
  bounds = list(x = 50, y = 50, height = 600, width = 700),
  height = 750,
  width = 800
)
d$defaultColors(diColors)
d$xAxis( type = "addMeasureAxis")
d$yAxis( type = "addMeasureAxis")
d$set(legend = list(x=760,y=40,height=100,width=100))
d$setTemplate(afterScript = "
              <script>
              myChart.draw()
              myChart.axes[0].titleShape.text('Mean CWI')
              myChart.axes[1].titleShape.text('Deprivation of assets (MPI)')
              myChart.svg.append('text')
              .attr('x', 250)
              .attr('y', 20)
              .text('CWI vs. MPI (DHS phase 6)')
              .style('text-anchor','beginning')
              .style('font-size', '100%')
              .style('font-family','sans-serif')
              </script>               
              ")
d

###assets = exp(-cwi)

data$exp.neg.cwi <- exp(-1*data$mean.cwi)

plot(assets~exp.neg.cwi,data=data)

fit <- lm(assets~exp.neg.cwi,data=data)
summary(fit)

fit <- lm(deprived.std.living~mean.cwi,data=data)
summary(fit)

fit <- lm(deprived.std.living~median.cwi,data=data)
summary(fit)

d2 <- dPlot(
  y = "assets",
  x = "exp.neg.cwi",
  groups = c("country","iso2c","mean.cwi","year"),
  data = data,
  type = "bubble",
  bounds = list(x = 50, y = 50, height = 600, width = 700),
  height = 750,
  width = 800
)
d2$defaultColors(diColors)
d2$xAxis( type = "addMeasureAxis")
d2$yAxis( type = "addMeasureAxis")
d2$set(legend = list(x=760,y=40,height=100,width=100))
d2$setTemplate(afterScript = "
              <script>
              myChart.draw()
              myChart.axes[0].titleShape.text('e^-cwi')
              myChart.axes[1].titleShape.text('Deprivation of assets (MPI)')
              myChart.svg.append('text')
              .attr('x', 250)
              .attr('y', 20)
              .text('assets = e^-cwi')
              .style('text-anchor','beginning')
              .style('font-size', '100%')
              .style('font-family','sans-serif')
              </script>               
              ")
d2

d2$save("mpi_interactive.html", cdn = TRUE)

