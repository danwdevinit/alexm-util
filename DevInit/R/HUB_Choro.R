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
  colorRef <- c(colorRef,"green"="#B7BF10")
  if(!is.na(colorText2)){
    if(!is.na(colorText3)){
      color1 <- colorRef[[colorText1]]
      color2 <- colorRef[[colorText2]]
      color3 <- colorRef[[colorText3]]
      colorRamp(c(color1,color2,color3), interpolate="linear")
    }else{
      color1 <- colorRef[[colorText1]]
      color2 <- colorRef[[colorText2]]
      colorRamp(c(color1,color2), interpolate="linear")
    }
  }else{
    color1 <- colorRef[["white"]]
    color2 <- colorRef[[colorText1]]
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
    if(!is.na(classes)){
      pal <- colorFactor(colors, NULL,levels=classes)
    }else{
      conceptPath <- "C:/git/digital-platform/concepts.csv"
      concepts <- read.csv(conceptPath, header = TRUE,sep=",",na.strings="",check.names=FALSE,stringsAsFactors=FALSE)
      range <- concepts[which(concepts$id==indicator&concepts$series=="country-year"),]$range
      if(!is.na(range)){
        classes <- as.numeric(strsplit(range,",")[[1]])
        if(!is.na(classes[1])){
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
          levels <- strsplit(range,",")[[1]]
          pal <- colorFactor(colors, NULL, levels = levels)
        }
      }else{
        warning("Error reading range from concepts.csv. Choosing 5 chucks instead.")
        pal <- colorQuantile(colors, NULL, n = 5)
      }  
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
###Poverty
#avg-income-of-extreme-poor
hub_leaflet("latest-year","avg-income-of-extreme-poor",NA,"value",NA,diRamp("red","white"))

#poor-people
hub_leaflet("latest-year","poor-people",NA,"value",NA,diRamp("red"))

#poorest20pct
hub_leaflet("latest-year","poorest20pct",NA,"value",NA,diRamp("red"))

#poorest20pct-percentages
hub_leaflet("latest-year","poorest20pct-percentages",NA,"value",NA,diRamp("red"))

#poverty-125
hub_leaflet("latest-year","poverty-125",NA,"value",NA,diRamp("red"))

#poverty-200
hub_leaflet("latest-year","poverty-200",NA,"value",NA,diRamp("red"))

#poverty-gap-125
hub_leaflet("latest-year","poverty-gap-125",NA,"value",NA,diRamp("red"))

###Vulnerability
#climate-vulnerability
hub_leaflet("latest-year","climate-vulnerability",NA,"value",c(0.18,0.4,0.6,0.8,0.9),diRamp("orange","white","red"))

#fragile-states
hub_leaflet("latest-year","fragile-states",NA,"value",NA,diRamp("red"))

#human-hazard
#natural-hazard


###Domestic resources
#govtspend-pc
#govtspend-USD

###International resources
#fdi-pp
#intl-flows-donors
#intl-flows-recipients
#intlresources-total
#largest-intl-flow
#profits-pct-fdi
#rems-pp


###International official finance
#in-oda-gross
#in-oda-net
#in-oof-gross
#oda
#oof
#out-oda-net
#out-oof-net

###Humanitarian
#in-ha




