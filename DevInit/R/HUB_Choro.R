###Thanks to Kyle Walker, http://rpubs.com/walkerke/wdi_leaflet
#install.packages('rgdal')
#install.packages('devtools')
#install.packages('plyr')
#library(devtools)
#devtools::install_github("rstudio/leaflet")
library(rgdal)
library(leaflet)
library(plyr)

### Function to create a Leaflet interactive map in RStudio from a World Bank indicator.

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
    range <- concepts[which(concepts$id==indicator),]$range
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
  legend <- ddply(legend,.(color),summarize,from=min(val),to=max(val),stringsAsFactors=FALSE)
  legend <- legend[order(legend$from),]
  legend$from.to <- paste(as.character(legend$from),as.character(legend$to),sep=" - ")
  legend$length <- legend$to-legend$from
  legend<- legend[c("color","from.to","length")]
  for(i in 1:length(legend$from.to)){
    if(legend$from.to[i]=="NA - NA"){
      legend$from.to[i]="NA"
    }
  }
  barplot(legend$length,
          legend.text=legend$from.to,
          beside=FALSE,
          col=legend$color,
          main = indicator)
  
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

## Example country-year
#hub_leaflet("country-year","adult-literacy",2012,"value",5,"Blues")

## Example latest-year
#hub_leaflet("latest-year","climate-vulnerability",NA,"value",5,"Reds")

## Example latest-year w/ range from concepts.csv
#hub_leaflet("latest-year","avg-income-of-extreme-poor",NA,"value",NA,"Greens")

## Example latest-year w/ defined range
hub_leaflet("latest-year","education-pc-transferred-oda",NA,"value",c(3,5,7,11),"Greens")

