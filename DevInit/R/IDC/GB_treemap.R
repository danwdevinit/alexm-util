#install.packages("treemap")
library(treemap)
library(plyr)
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

depth <- subset(data,flow=="depth")
names(depth)[4] <- "depth"

wd <- "C:/git/digital-platform/country-year"
setwd(wd)
data <- read.csv("oda.csv",na.strings="",stringsAsFactors=FALSE)
entity <- read.csv("../reference/entity.csv",na.strings="",stringsAsFactors=FALSE)
entity <- entity[c("id","name")]
entity$name[44] <- "Cote d'Ivoire"

oda <- subset(data,id.from=="GB")
oda <- ddply(oda,.(id.to,year),summarize,value=sum(value,na.rm=TRUE))
oda <- merge(
  oda
  ,depth
  ,by.x=c("id.to","year")
  ,by.y=c("country.id","year")
  )
oda <- transform(oda,color=treeMapRamp(depth))
oda <- merge(
  oda
  ,entity
  ,by.x="id.to"
  ,by.y="id")
treemap(oda
        ,index="name"
        ,vSize="value"
        ,vColor="color"
        ,type="color"
        ,title="UK ODA (box volume) vs. Recipient depth of poverty (hue) - 2011"
        #,fontcolor.labels=rgb(0,0,0,0)
)
