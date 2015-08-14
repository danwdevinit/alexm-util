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

depth <- read.csv("depth.csv",as.is=TRUE)
depth[nrow(depth)+1,] <- c("Palestine",-1.0)
depth$depth <- as.numeric(depth$depth)

oda <- read.csv("oda.csv")
oda <- reshape(oda,varying=list(2:length(oda)),idvar="country",v.names="value",times=c(2000:2014),direction="long")
oda <- transform(oda,value=as.numeric(gsub(",","",value)))
oda <- subset(oda,value>0 & time==2013)
oda <- merge(oda
             ,depth
             ,by=c("country")
             ,all.x=TRUE)
oda <- transform(oda,color=treeMapRamp(depth))
fdi <- read.csv("fdi.csv")
fdi <- reshape(fdi,varying=list(2:length(fdi)),idvar="country",v.names="value",times=c(2000:2013),direction="long")
fdi <- transform(fdi,value=as.numeric(gsub(",","",value)))
fdi <- subset(fdi,value>0 & time==2013)
fdi <- merge(fdi
             ,depth
             ,by=c("country")
             ,all.x=TRUE)
fdi <- transform(fdi,color=treeMapRamp(depth))
remit <- read.csv("remit.csv")
remit <- reshape(remit,varying=list(2:length(remit)),idvar="country",v.names="value",times=c(2000:2013),direction="long")
remit <- transform(remit,value=as.numeric(gsub(",","",value)))
remit <- subset(remit,value>0 & time==2013)
remit <- merge(remit
             ,depth
             ,by=c("country")
             ,all.x=TRUE)
remit <- transform(remit,color=treeMapRamp(depth))
longdebt <- read.csv("longdebt.csv")
longdebt <- reshape(longdebt,varying=list(2:length(longdebt)),idvar="country",v.names="value",times=c(2000:2013),direction="long")
longdebt <- transform(longdebt,value=as.numeric(gsub(",","",value)))
longdebt <- subset(longdebt,value>0 & time==2013)
longdebt <- merge(longdebt
             ,depth
             ,by=c("country")
             ,all.x=TRUE)
longdebt <- transform(longdebt,color=treeMapRamp(depth))

treemap(oda
        ,index="country"
        ,vSize="value"
        ,vColor="color"
        ,type="color"
        ,title="ODA 2013"
        #,fontcolor.labels=rgb(0,0,0,0)
        )

treemap(fdi
        ,index="country"
        ,vSize="value"
        ,vColor="color"
        ,type="color"
        ,title="FDI 2013"
        #,fontcolor.labels=rgb(0,0,0,0)
        )

treemap(remit
        ,index="country"
        ,vSize="value"
        ,vColor="color"
        ,type="color"
        ,title="Remittances 2013"
        #,fontcolor.labels=rgb(0,0,0,0)
        )

treemap(longdebt
        ,index="country"
        ,vSize="value"
        ,vColor="color"
        ,type="color"
        ,title="Long-debt 2013"
        #,fontcolor.labels=rgb(0,0,0,0)
        )
