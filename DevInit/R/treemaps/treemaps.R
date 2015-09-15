#install.packages("treemap")
library(treemap)
wd <- "C:/git/alexm-util/DevInit/R/treemaps"
setwd(wd)

diRamp <- function(colorText1,colorText2=NA,colorText3=NA){
  colorRef <- list("red"="#BA0C2F")
  colorRef <- c(colorRef,"white"="#FFFFFF")
  colorRef <- c(colorRef,"black"="#000000")
  colorRef <- c(colorRef,"orange"="#EA7600")
  colorRef <- c(colorRef,"purple"="#93328E")
  colorRef <- c(colorRef,"blue"="#1B365D")
  colorRef <- c(colorRef,"lightblue"="#0095CB")
  colorRef <- c(colorRef,"yellow"="#B7BF10")
  colorRef <- c(colorRef,"darkred"=rgb(96, 6, 24,1,maxColorValue=255))
  colorRef <- c(colorRef,"pink"=rgb(251, 197, 208,1,maxColorValue=255))
  colorRef <- c(colorRef,"blue4"=rgb(27, 54, 93,1,maxColorValue=255))
  colorRef <- c(colorRef,"blue3"=rgb(73, 94, 125,1,maxColorValue=255))
  colorRef <- c(colorRef,"blue2"=rgb(118, 134, 158,1,maxColorValue=255))
  colorRef <- c(colorRef,"blue1"=rgb(164, 175, 190,1,maxColorValue=255))
  colorRef <- c(colorRef,"blue0"=rgb(209, 215, 223,1,maxColorValue=255))
  
  if(!is.na(colorText2)){
    if(!is.na(colorText3)){
      color1 <- colorRef[[colorText1]]
      if(is.null(color1)){color1 <- colorText1}
      color2 <- colorRef[[colorText2]]
      if(is.null(color2)){color2 <- colorText2}
      color3 <- colorRef[[colorText3]]
      if(is.null(color3)){color3 <- colorText3}
      colorRamp(c(color1,color2,color3), interpolate="linear")
    }else{
      color1 <- colorRef[[colorText1]]
      if(is.null(color1)){color1 <- colorText1}
      color2 <- colorRef[[colorText2]]
      if(is.null(color2)){color2 <- colorText2}
      colorRamp(c(color1,color2), interpolate="linear")
    }
  }else{
    color1 <- colorRef[["white"]]
    color2 <- colorRef[[colorText1]]
    if(is.null(color2)){color2 <- colorText2}
    colorRamp(c(color1,color2), interpolate="linear")
  }
}

treeMapRamp <- function(vector){
  vectorMax = max(vector)
  message(vectorMax)
  colors <- c()
  for(i in 1:length(vector)){
    newVal <- vector[i]/vectorMax
    if(vector[i]>0){
      rgb <- diRamp("red")(newVal)
      color <- substr(rgb(rgb[1,1],rgb[1,2],rgb[1,3],1,maxColorValue=255),1,7)
      colors <- c(colors,color)
    }else{
      color <- "#cccccc"
      colors <- c(colors,color)
    }
  }
  return(colors)
}

data <- read.csv("treemaps.csv",na.strings="")
data <- data[,1:3]
entities <- read.csv("C:/git/digital-platform/reference/entity.csv",na.strings="",as.is=TRUE)
entities$name[44] <- "Cote d'Ivoire"
entities$name[241] <- "Vietnam"
data <- merge(
  data
  ,entities
  ,by.x="country.id"
  ,by.y="id"
  )

depth <- subset(data,flow=="depth")
names(depth)[3] <- "depth"

oda <- subset(data,flow=="oda")
oda <- subset(oda,value>0)
oda <- merge(oda
             ,depth
             ,by=c("country.id")
             ,all.x=TRUE)
oda <- transform(oda,color=treeMapRamp(depth))

fdi <- subset(data,flow=="fdi")
fdi <- subset(fdi,value>0)
fdi <- merge(fdi
             ,depth
             ,by=c("country.id")
             ,all.x=TRUE)
fdi <- transform(fdi,color=treeMapRamp(depth))

longdebt <- subset(data,flow=="longdebt")
longdebt <- subset(longdebt,value>0)
longdebt <- merge(longdebt
             ,depth
             ,by=c("country.id")
             ,all.x=TRUE)
longdebt <- transform(longdebt,color=treeMapRamp(depth))

remittances <- subset(data,flow=="remittances")
remittances <- subset(remittances,value>0)
remittances <- merge(remittances
             ,depth
             ,by=c("country.id")
             ,all.x=TRUE)
remittances <- transform(remittances,color=treeMapRamp(depth))

#Remove missing
oda <- subset(oda,depth>0)
fdi <- subset(fdi,depth>0)

treemap(oda
        ,index="name.y"
        ,vSize="value"
        ,vColor="color"
        ,type="color"
        ,title=""
        ,lowerbound.cex.labels=1
        ,fontsize.labels=25
        ,inflate.labels=TRUE
        )

treemap(fdi
        ,index="name.y"
        ,vSize="value"
        ,vColor="color"
        ,type="color"
        ,title=""
        ,lowerbound.cex.labels=1
        ,fontsize.labels=25
        ,inflate.labels=TRUE
        )

treemap(remittances
        ,index="name.y"
        ,vSize="value"
        ,vColor="color"
        ,type="color"
        ,title=""
        ,lowerbound.cex.labels=1
        ,fontsize.labels=25
        ,inflate.labels=TRUE
        )

treemap(longdebt
        ,index="name.y"
        ,vSize="value"
        ,vColor="color"
        ,type="color"
        ,title=""
        ,lowerbound.cex.labels=1
        ,fontsize.labels=25
        ,inflate.labels=TRUE
        )
