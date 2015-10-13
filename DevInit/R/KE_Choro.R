###Function########################################################################################
###Thanks to Kyle Walker, http://rpubs.com/walkerke/wdi_leaflet
#install.packages('rgdal')
#install.packages('devtools')
#install.packages('plyr')
#library(devtools)
#devtools::install_github("rstudio/leaflet")
library(rgdal)
library(leaflet)
library(plyr)

windows <- TRUE
if(windows){pathpre<-"C:"}else{pathpre<-"~"}

### Function to create a Leaflet interactive map.

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

ke_leaflet <- function(series,indicator, year = NA, value = "value", classes = 5, colors = "Blues") {

  districts <- readOGR(dsn = paste0(pathpre,"/git/digital-platform/shape-files/kenya/kenya.shp"), 
                       layer = "kenya", 
                       encoding = "UTF-8",
                       verbose = FALSE)
  keep <- c("ID","NAME_2")
  districts <- districts[keep]
  names(districts) <- c("id","name")
  
  datPath <- paste(paste0(pathpre,"/git/digital-platform/"),series,"/",indicator,".csv",sep="")
  dat <- read.csv(datPath, header = TRUE,sep=",",na.strings="",check.names=FALSE,stringsAsFactors=FALSE,row.names=NULL)
  if(!is.na(year)){
    dat <- dat[which(dat$year==year),]
  }
  keep <- c("id","year",indicator)
  names(dat)[names(dat) == value] <- indicator
  dat <- dat[keep]
  
  districts2 <- merge(districts, 
                      dat, 
                      by = c("id"),                 
                      sort = FALSE)
  districts2$district <- districts2$name
  
  if(is.numeric(classes)){
    if(length(classes)==1){
      pal <- colorQuantile(colors, NULL, n = classes)
    }else{
      levels <- classes[order(classes)]
      indDat <- dat[[indicator]]
      indDat <- indDat[which(!is.na(indDat))]
      indMin <- min(indDat)
      indMax <- max(indDat)
      if(levels[1]>indMin){
        levels <- c(indMin,levels)
      }
      if(levels[length(levels)]<indMax){
        levels <- c(levels,indMax)
      }
      pal <- colorBin(colors, c(indMin,indMax) , bins = levels)
    }
  }else{
    conceptPath <- paste0(pathpre,"/git/digital-platform/concepts.csv")
    concepts <- read.csv(conceptPath, header = TRUE,sep=",",na.strings="",check.names=FALSE,stringsAsFactors=FALSE)
    range <- concepts[which(concepts$id==indicator&concepts$series=="country-year"),]$range
    if(!is.na(range)){
      classes <- as.numeric(strsplit(range,",")[[1]])
      levels <- classes[order(classes)]
      indDat <- dat[[indicator]]
      indDat <- indDat[which(!is.na(indDat))]
      indMin <- min(indDat)
      indMax <- max(indDat)
      if(levels[1]>indMin){
        levels <- c(indMin,levels)
      }
      if(levels[length(levels)]<indMax){
        levels <- c(levels,indMax)
      }
      pal <- colorBin(colors, c(indMin,indMax) , bins = levels)
    }else{
      warning("Error reading range from concepts.csv. Choosing 5 chucks instead.")
      pal <- colorQuantile(colors, NULL, n = 5)
    }  
  }
  
  popup <- paste0("<strong>District: </strong>", 
                          districts2$district, 
                          "<br><strong>", 
                          indicator, 
                          ", ", 
                          as.character(districts2$year), 
                          ": </strong>", 
                  districts2[[indicator]])
  
  stamen_tiles <- "http://{s}.tile.stamen.com/toner-lite/{z}/{x}/{y}.png"
  
  stamen_attribution <- 'Map tiles by <a href="http://stamen.com">Stamen Design</a>, under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>. Data by <a href="http://openstreetmap.org">OpenStreetMap</a>, under <a href="http://www.openstreetmap.org/copyright">ODbL</a>.'

  val<-districts2[[indicator]]
  color<- pal(districts2[[indicator]])
  legend <- data.frame(val,color,stringsAsFactors=FALSE)
  legend <- ddply(legend,.(color),summarize,from=min(val),to=max(val),count=length(val),stringsAsFactors=FALSE)
  legend <- legend[order(legend$from),]
  legend$from.to <- paste(as.character(legend$from),as.character(legend$to),sep=" - ")
  legend<- legend[c("color","from.to","count")]
  for(i in 1:length(legend$from.to)){
    if(legend$from.to[i]=="NA - NA"){
      legend$from.to[i]="NA"
    }
  }
  bp <- barplot(legend$count,
          legend.text=legend$from.to,
          beside=FALSE,
          col=legend$color,
          main = indicator)
          text(bp, 0, round(legend$count, 1),cex=1,pos=3)
  
  leaflet(data = districts2) %>%
#     addTiles(urlTemplate = stamen_tiles,  
#              attribution = stamen_attribution) %>%
    setView(38, .5, zoom = 6) %>%
    addPolygons(fillColor = ~pal(districts2[[indicator]]), 
                fillOpacity = 0.8, 
                color = "#BDBDC3", 
                weight = 1, 
                popup = popup)
}

##Syntax is:
#ke_leaflet(series,indicator,year,value,classes,colorRamp)
#Map appears in viewer, legend appears in plots along with count of data in those bins

ke_leaflet("country-year","kenya-births-pc-skilled",2009,"value",c(40,50,60,70),diRamp("purple"))
ke_leaflet("country-year","kenya-fertility-rate",2009,"value",c(3,4,5,6),diRamp("purple"))
ke_leaflet("country-year","kenya-pc-female-know-hiv",2009,"value",c(85,90,92.5,95),diRamp("purple"))
ke_leaflet("country-year","kenya-pc-female-tested-hiv",2009,"value",c(40,50,55,60),diRamp("purple"))
ke_leaflet("country-year","kenya-pc-house-malaria-nets",2009,"value",c(20,30,40,50),diRamp("purple"))
ke_leaflet("country-year","kenya-pc-male-know-hiv",2009,"value",c(85,90,92.5,95),diRamp("purple"))
ke_leaflet("country-year","kenya-pc-male-tested-hiv",2009,"value",c(40,50,55,60),diRamp("purple"))
ke_leaflet("country-year","kenya-pc-modern-contra",2009,"value",c(30,40,50,60),diRamp("purple"))
ke_leaflet("country-year","kenya-pc-no-contra",2009,"value",c(50,60,70,80),diRamp("purple"))
ke_leaflet("country-year","kenya-pc-trad-contra",2009,"value",c(2,4,6,8),diRamp("purple"))
ke_leaflet("country-year","kenya-pop-female",2009,"value",c(250000, 350000, 450000, 550000),diRamp("purple"))
ke_leaflet("country-year","kenya-pop-male",2009,"value",c(250000,350000,450000,550000),diRamp("purple"))
ke_leaflet("country-year","kenya-pop-pc-female",2009,"value",c(49.5,50,50.5,51),diRamp("purple"))
ke_leaflet("country-year","kenya-pop-pc-male",2009,"value",c(49.5,50,50.5,51),diRamp("purple"))
ke_leaflet("country-year","kenya-pop-total",2009,"value",c(500000, 700000, 900000, 1000000),diRamp("purple"))
ke_leaflet("country-year","kenya-treat-child-diarr",2009,"value",c(50,55,60,65),diRamp("purple"))
ke_leaflet("country-year","kenya-treat-child-respir",2009,"value",c(60,65,70,75),diRamp("purple"))
ke_leaflet("country-year","kenya-weight-below-3sd",2009,"value",c(1.5,2.5,3.5,4.5),diRamp("purple"))

ke_leaflet("country-year","kenya-electricity",2009,"value",5,diRamp("purple"))
ke_leaflet("country-year","kenya-improved-sani",2009,"value",5,diRamp("purple"))

ke_leaflet("country-year","kenya-improved-water",2009,"value",5,diRamp("purple"))

ke_leaflet("country-year","kenya-paved-roads",2012,"value",5,diRamp("purple"))

ke_leaflet("country-year","kenya-pov-gap",2009,"value",5,diRamp("purple"))

ke_leaflet("country-year","kenya-rural-pop",2009,"value",5,diRamp("purple"))

ke_leaflet("country-year","kenya-urban-pop",2009,"value",5,diRamp("purple"))

