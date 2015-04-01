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

### Function to create a Leaflet interactive map.

diRamp <- function(colorText1,colorText2=NA,colorText3=NA){
  colorRef <- list("red"="#BA0C2F")
  colorRef <- c(colorRef,"white"="#FFFFFF")
  colorRef <- c(colorRef,"black"="#000000")
  colorRef <- c(colorRef,"orange"="#EA7600")
  colorRef <- c(colorRef,"purple"="#93328E")
  colorRef <- c(colorRef,"blue"="#1B365D")
  colorRef <- c(colorRef,"lightblue"="#0095CB")
  colorRef <- c(colorRef,"yellow"=rgb(192,204,64,1,maxColorValue=255))
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

hub_leaflet <- function(series,indicator, year = NA, value = "value", classes = 5, colors = "Blues") {
  
  url <- "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip"
  
  tmp <- tempdir()
  
  file <- basename(url)
  
  if(!file.exists(file)){
    download.file(url, file)
  }
  unzip(file, exdir = tmp)
  
  countries <- readOGR(dsn = tmp, 
                       layer = "ne_50m_admin_0_countries", 
                       encoding = "UTF-8",
                       verbose = FALSE)
  
  
  datPath <- paste("C:/git/digital-platform/",series,"/",indicator,".csv",sep="")
  dat <- read.csv(datPath, header = TRUE,sep=",",na.strings="",check.names=FALSE,stringsAsFactors=FALSE,row.names=NULL)
  if(!is.na(year)){
    dat <- dat[which(dat$year==year),]
  }
  keep <- c("iso2c","year",indicator)
  names(dat)[names(dat) == "id"] <- "iso2c"
  names(dat)[names(dat) == value] <- indicator
  dat <- dat[keep]
  
  countries2 <- merge(countries, 
                      dat, 
                      by.x = "iso_a2", 
                      by.y = "iso2c",                    
                      sort = FALSE)
  countries2$country <- countries2$name
  
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
    conceptPath <- "C:/git/digital-platform/concepts.csv"
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
  
  country_popup <- paste0("<strong>Country: </strong>", 
                          countries2$country, 
                          "<br><strong>", 
                          indicator, 
                          ", ", 
                          as.character(countries2$year), 
                          ": </strong>", 
                          countries2[[indicator]])
  
  stamen_tiles <- "http://{s}.tile.stamen.com/toner-lite/{z}/{x}/{y}.png"
  
  stamen_attribution <- 'Map tiles by <a href="http://stamen.com">Stamen Design</a>, under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>. Data by <a href="http://openstreetmap.org">OpenStreetMap</a>, under <a href="http://www.openstreetmap.org/copyright">ODbL</a>.'

  val<-countries2[[indicator]]
  color<- pal(countries2[[indicator]])
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
  
  leaflet(data = countries2) %>%
    #addTiles(urlTemplate = stamen_tiles,  
    #         attribution = stamen_attribution) %>%
    setView(0, 0, zoom = 2) %>%
    addPolygons(fillColor = ~pal(countries2[[indicator]]), 
                fillOpacity = 0.8, 
                color = "#BDBDC3", 
                weight = 1, 
                popup = country_popup)
}

##Syntax is:
#hub_leaflet(series,indicator,year,value,classes,colorRamp)
#Map appears in viewer, legend appears in plots along with count of data in those bins

####Files that appear in Global Picture

###Poverty########################################################################################
#avg-income-of-extreme-poor
hub_leaflet("latest-year","avg-income-of-extreme-poor",NA,"value",NA,diRamp("red","white"))
#avg-income-of-extreme-poor 5 class
hub_leaflet("latest-year","avg-income-of-extreme-poor",NA,"value",5,diRamp("red","white"))
#avg-income-of-extreme-poor recommend
hub_leaflet("latest-year","avg-income-of-extreme-poor",NA,"value",c(0.75,1,1.1,1.2),diRamp("red","white"))

#poor-people
hub_leaflet("latest-year","poor-people",NA,"value",NA,diRamp("red"))

#poorest20pct
hub_leaflet("latest-year","poorest20pct",NA,"value",NA,diRamp("red"))

#poverty-125
hub_leaflet("latest-year","poverty-125",NA,"value",NA,diRamp("red"))
#poverty-125 5 class
hub_leaflet("latest-year","poverty-125",NA,"value",5,diRamp("red"))
#poverty-125 recommend
hub_leaflet("latest-year","poverty-125",NA,"value",c(5,20,40,60,80),diRamp("red"))

#poverty-200
hub_leaflet("latest-year","poverty-200",NA,"value",NA,diRamp("red"))

###Vulnerability########################################################################################
#climate-vulnerability
hub_leaflet("latest-year","climate-vulnerability",NA,"value",c(0.18,0.4,0.6,0.8,0.9),diRamp("orange","white","red"))
#climate-vulnerability 5 class
hub_leaflet("latest-year","climate-vulnerability",NA,"value",5,diRamp("orange","white","red"))
#climate-vulnerability recommend (GAIN green, white, GAIN red)
hub_leaflet("latest-year","climate-vulnerability",NA,"value",c(0.3,0.35,0.4,0.5,0.6),diRamp("#79ba51","white","#f85f5f"))
#climate-vulnerability recommend alternative (GAIN green, GAIN red)
hub_leaflet("latest-year","climate-vulnerability",NA,"value",c(0.3,0.35,0.4,0.5,0.6),diRamp("#79ba51","#f85f5f"))

#fragile-states
hub_leaflet("latest-year","fragile-states",NA,"value",c(1,2,3,4,5,6),diRamp("orange","white","red"))
#fragile-states recommend
hub_leaflet("latest-year","fragile-states",NA,"value",c(1,2,3,4,5,6),diRamp("red"))
#fragile-states recommend alternative
hub_leaflet("latest-year","fragile-states",NA,"value",c(1,2,3,4,5,6),diRamp("orange"))

#human-hazard
hub_leaflet("latest-year","human-hazard",NA,"value",NA,diRamp("orange","white","red"))
#human-hazard recommend
hub_leaflet("latest-year","human-hazard",NA,"value",c(2,4,6,8,9.9,10),diRamp("red"))
#human-hazard recommend alternative
hub_leaflet("latest-year","human-hazard",NA,"value",c(2,4,6,8,9.9,10),diRamp("orange"))

#natural-hazard
hub_leaflet("latest-year","natural-hazard",NA,"value",NA,diRamp("orange","white","red"))
#natural-hazard recommend
hub_leaflet("latest-year","natural-hazard",NA,"value",c(2,3,4,6,8),diRamp("red"))
#natural-hazard recommend alternative
hub_leaflet("latest-year","natural-hazard",NA,"value",c(2,3,4,6,8),diRamp("orange"))

###Domestic resources########################################################################################
#govtspend-pc
hub_leaflet("latest-year","govtspend-pc",NA,"value",NA,diRamp("red","white","orange"))
#govtspend-pc 5 class
hub_leaflet("latest-year","govtspend-pc",NA,"value",5,diRamp("red"))
#govtspend-pc recommend RW
hub_leaflet("latest-year","govtspend-pc",NA,"value",NA,diRamp("red"))
#govtspend-pc recommend RW alternative
hub_leaflet("latest-year","govtspend-pc",NA,"value",NA,diRamp("yellow"))
#govtspend-pc recommend AM
hub_leaflet("latest-year","govtspend-pc",NA,"value",c(250,500,5000,10000),diRamp("yellow"))
#govtspend-pc recommend AM alternative
hub_leaflet("latest-year","govtspend-pc",NA,"value",c(250,500,5000,10000),diRamp("red"))

#govtspend-USD
hub_leaflet("latest-year","govtspend-USD",NA,"value",NA,diRamp("red","white","orange"))
#govtspend-USD recommend
hub_leaflet("latest-year","govtspend-USD",NA,"value",NA,diRamp("red"))
#govtspend-USD recommend alternative
hub_leaflet("latest-year","govtspend-USD",NA,"value",NA,diRamp("yellow"))

#gov-revenue

###International resources########################################################################################
#fdi-pp
hub_leaflet("latest-year","fdi-pp",NA,"value",NA,diRamp("red","white","orange"))
#fdi-pp 5 class
hub_leaflet("latest-year","fdi-pp",NA,"value",5,diRamp("blue"))
#fdi-pp recommend
hub_leaflet("latest-year","fdi-pp",NA,"value",c(25,100,250,1000),diRamp("blue"))
#fdi-pp recommend alternative (colorBrewer Blues)
hub_leaflet("latest-year","fdi-pp",NA,"value",c(25,100,250,1000),"Blues")

#intlresources-total
hub_leaflet("latest-year","intlresources-total",NA,"value",NA,diRamp("blue"))
#intlresources-total 5 class
hub_leaflet("latest-year","intlresources-total",NA,"value",5,diRamp("blue"))
#intlresources-total recommend
hub_leaflet("latest-year","intlresources-total",NA,"value",c(300000000, 1500000000, 4000000000, 10000000000),diRamp("blue"))
#intlresources-total recommend
hub_leaflet("latest-year","intlresources-total",NA,"value",c(300000000, 1500000000, 4000000000, 10000000000),"Blues")

#largest-intl-flow
hub_leaflet("country-year","largest-intl-flow",2013,"value",c(1,2,3,4,5,6,7,8),diRamp("red"))
#Can't get colors right on this one since it's categorical 

#profits-pct-fdi
hub_leaflet("latest-year","profits-pct-fdi",NA,"value",NA,diRamp("orange","white","red"))
#profits-pct-fdi recommend
hub_leaflet("latest-year","profits-pct-fdi",NA,"value",NA,diRamp("blue"))
#profits-pct-fdi recommend alternative
hub_leaflet("latest-year","profits-pct-fdi",NA,"value",NA,"Blues")

#rems-pp
hub_leaflet("latest-year","rems-pp",NA,"value",NA,diRamp("red","white","orange"))
#rems-pp 5 class
hub_leaflet("latest-year","rems-pp",NA,"value",5,diRamp("blue"))
#rems-pp recommend
hub_leaflet("latest-year","rems-pp",NA,"value",c(5, 25, 100, 250),diRamp("blue"))
#rems-pp recommend alternative
hub_leaflet("latest-year","rems-pp",NA,"value",c(5, 25, 100, 250),"Blues")

###International official finance########################################################################################
#in-oda-gross
hub_leaflet("latest-year","in-oda-gross",NA,"value",NA,diRamp("red"))
#in-oda-gross recommend
hub_leaflet("latest-year","in-oda-gross",NA,"value",NA,diRamp("purple"))

#in-oda-net
hub_leaflet("latest-year","in-oda-net",NA,"value",NA,diRamp("red"))
#in-oda-net recommend
hub_leaflet("latest-year","in-oda-net",NA,"value",NA,diRamp("purple"))

#in-oof-gross
hub_leaflet("latest-year","in-oof-gross",NA,"value",NA,diRamp("red"))
#in-oof-gross 5 class
hub_leaflet("latest-year","in-oof-gross",NA,"value",5,diRamp("red"))
#in-oof-gross recommend
hub_leaflet("latest-year","in-oof-gross",NA,"value",c(0, 3000000, 30000000, 300000000, 3000000000),diRamp("purple"))

#out-oda-net
hub_leaflet("latest-year","out-oda-net",NA,"value",NA,diRamp("red"))
#out-oda-net 5 class
hub_leaflet("latest-year","out-oda-net",NA,"value",5,diRamp("red"))
#out-oda-net recommend
hub_leaflet("latest-year","out-oda-net",NA,"value",NA,diRamp("purple"))

#out-oof-net
hub_leaflet("latest-year","out-oof-net",NA,"value",NA,diRamp("red"))
#out-oof-net 5 class
hub_leaflet("latest-year","out-oof-net",NA,"value",5,diRamp("red"))
#out-oof-net recommend
hub_leaflet("latest-year","out-oof-net",NA,"value",c(0, 10000000, 400000000,1000000000),diRamp("purple"))

#oda-per-poor-person 5 class
hub_leaflet("latest-year","oda-per-poor-person",NA,"value",NA,diRamp("red"))
#oda-per-poor-person recommend
hub_leaflet("latest-year","oda-per-poor-person",NA,"value",c(100, 250, 1000, 5000),diRamp("purple"))

#oda-percent-gni 5 class
hub_leaflet("latest-year","oda-percent-gni",NA,"value",NA,diRamp("red"))
#oda-percent-gni recommend
hub_leaflet("latest-year","oda-percent-gni",NA,"value",c(0.05, 0.10, 0.25, 0.5),diRamp("purple"))

#oda-to-ldcs-pc-gni 5 class
hub_leaflet("latest-year","oda-to-ldcs-pc-gni",NA,"value",NA,diRamp("red"))
#oda-to-ldcs-pc-gni recommend
hub_leaflet("latest-year","oda-to-ldcs-pc-gni",NA,"value",c(0.02, 0.04, 0.08, 0.16),diRamp("purple"))

###Humanitarian########################################################################################
#in-ha
hub_leaflet("latest-year","in-ha",NA,"value",NA,diRamp("red"))
#in-ha 5 classes
hub_leaflet("latest-year","in-ha",NA,"value",5,diRamp("red"))
#in-ha recommend
hub_leaflet("latest-year","in-ha",NA,"value",c(2000000, 10000000, 50000000,100000000),diRamp("lightblue"))
