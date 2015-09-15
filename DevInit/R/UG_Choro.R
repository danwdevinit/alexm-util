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

ug_leaflet <- function(series,indicator, year = NA, value = "value", classes = 5, colors = "Blues") {

  districts <- readOGR(dsn = paste0(pathpre,"/git/digital-platform/shape-files/uganda/uganda.shp"), 
                       layer = "uganda", 
                       encoding = "UTF-8",
                       verbose = FALSE)
  
  
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
    #addTiles(urlTemplate = stamen_tiles,  
    #         attribution = stamen_attribution) %>%
    setView(32, 1, zoom = 6) %>%
    addPolygons(fillColor = ~pal(districts2[[indicator]]), 
                fillOpacity = 0.8, 
                color = "#BDBDC3", 
                weight = 1, 
                popup = popup)
}

##Syntax is:
#ug_leaflet(series,indicator,year,value,classes,colorRamp)
#Map appears in viewer, legend appears in plots along with count of data in those bins

#DPR
ug_leaflet("country-year","uganda-local-percent",2015,"value",c(1,2,3,4,10),diRamp("purple"))
ug_leaflet("country-year","uganda-donor-percent",2015,"value",c(1,2,3,4,10),diRamp("purple"))
ug_leaflet("country-year","uganda-educ-percent",2015,"value",c(30,40,50,60),diRamp("purple"))
ug_leaflet("country-year","uganda-agri-percent",2015,"value",c(3,4,5,6),diRamp("purple"))
ug_leaflet("country-year","uganda-health-percent",2015,"value",c(10,13,16,19),diRamp("purple"))

#Pov
ug_leaflet("country-year","uganda-poverty-headcount",NA,"value",c(30,50,70,90),diRamp("pink","red","darkred"))
ug_leaflet("country-year","uganda-deprivation-living",NA,"value",c(15,20,25,35,40),diRamp("pink","red","darkred"))
ug_leaflet("country-year","uganda-life-expectancy",NA,"value",c(35,42,44,46,48),diRamp("darkred","red","pink"))

#Pop
ug_leaflet("country-year","uganda-avg-house-size",NA,"value",c(4.3,4.6,4.9,5.2),diRamp("orange"))
ug_leaflet("country-year","uganda-pop-dens",NA,"value",c(35,70,140,210,280),diRamp("orange","white"))
ug_leaflet("country-year","uganda-urban-pop",NA,"value",c(5,8,15,20),diRamp("orange"))
ug_leaflet("country-year","uganda-dependency-ratio",NA,"value",c(100,110,115,120),diRamp("orange"))
ug_leaflet("country-year","uganda-total-pop",NA,"value",c(150000, 200000, 300000, 450000),diRamp("orange"))

#Educ
ug_leaflet("country-year","uganda-primary-educ-funding",2015,"value",c(35,40,45,50),diRamp("yellow"))
ug_leaflet("country-year","uganda-donor-educ-spend",2015,"value",NA,diRamp("yellow"))
ug_leaflet("country-year","uganda-primary-stu-teach-ratio",2013,"value",c(35,45,55,65),diRamp("yellow"))
ug_leaflet("country-year","uganda-primary-stu-teach-ratio-gov",2013,"value",c(35,45,55,65),diRamp("yellow"))
ug_leaflet("country-year","uganda-primary-sit-write",2013,"value",c(55,70,75,80),diRamp("yellow"))
ug_leaflet("country-year","uganda-primary-sit-write-gov",2013,"value",c(55,70,75,80),diRamp("yellow"))
ug_leaflet("country-year","uganda-primary-enrol",2013,"value",c(80,95,110,125),diRamp("yellow"))
ug_leaflet("country-year","uganda-leaving-exam-perf-rate",2013,"value",c(50,55,60,65),diRamp("yellow"))
ug_leaflet("country-year","uganda-secondary-stu-teach-ratio",2013,"value",c(20,25,30,35),diRamp("yellow"))
ug_leaflet("country-year","uganda-secondary-stu-teach-ratio-gov",2013,"value",c(20,25,30,35),diRamp("yellow"))
ug_leaflet("country-year","uganda-secondary-sit-write",2013,"value",c(85,90,95,97.5,100),diRamp("yellow"))
ug_leaflet("country-year","uganda-secondary-sit-write-gov",2013,"value",c(35,45,55,65),diRamp("yellow"))
ug_leaflet("country-year","uganda-secondary-enrol",2013,"value",c(10,20,30,40),diRamp("yellow"))

#Health
ug_leaflet("country-year","uganda-overall-health",NA,"value",c(65,70,75,80),diRamp("red","pink"))
ug_leaflet("country-year","uganda-hmis",NA,"value",c(6, 7, 8, 9),diRamp("red","pink"))
ug_leaflet("country-year","uganda-health-posts",NA,"value",c(55,65,75,85),diRamp("red","pink"))
ug_leaflet("country-year","uganda-tb-success",NA,"value",c(70,80,90,95),diRamp("red","pink"))
ug_leaflet("country-year","uganda-dpt3-coverage",NA,"value",c(80, 90, 100, 110),diRamp("red","pink"))
ug_leaflet("country-year","uganda-ipt2-coverage",NA,"value",c(40, 45, 55, 65),diRamp("red","pink"))
ug_leaflet("country-year","uganda-anc4-coverage",NA,"value",c(20, 25, 35, 45),diRamp("red","pink"))
ug_leaflet("country-year","uganda-health-funding",2015,"value",c(3,5,6,9),diRamp("red","pink"))

#WASH
ug_leaflet("country-year","uganda-rural-safe-water",NA,"value",c(50,65,75,85),diRamp("blue"))
ug_leaflet("country-year","uganda-rural-water-func",NA,"value",c(78,83,88,93),diRamp("blue"))
ug_leaflet("country-year","uganda-water-source-comm-func",NA,"value",c(50,65,75,90),diRamp("blue"))
ug_leaflet("country-year","uganda-household-san-cov",NA,"value",c(50,70,80,90),diRamp("blue"))
ug_leaflet("country-year","uganda-wash-perf-score",NA,"value",c(35,45,55,65),diRamp("blue"))




