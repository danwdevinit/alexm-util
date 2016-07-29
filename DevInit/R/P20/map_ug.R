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
library(raster)

setwd("D:/Documents/Data/WorldPop/pov-tifs/")
districts <- raster("p20.tif")
crs(districts) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
dValues <- values(districts)
dValues <- dValues[which(!is.na(dValues))]
pal <- colorNumeric(c("#FFFFFF", "#BA0C2F"), dValues, na.color = "transparent")

stamen_tiles <- "http://{s}.tile.stamen.com/toner-lite/{z}/{x}/{y}.png"

stamen_attribution <- 'Map tiles by <a href="http://stamen.com">Stamen Design</a>, under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>. Data by <a href="http://openstreetmap.org">OpenStreetMap</a>, under <a href="http://www.openstreetmap.org/copyright">ODbL</a>.'

leaflet() %>%
      addTiles(urlTemplate = stamen_tiles,  
               attribution = stamen_attribution) %>%
  setView(32, 1, zoom = 6) %>%
  addRasterImage(districts,colors=pal,opacity=0.99) %>%
  addLegend(pal = pal, values = dValues,
            title = "P20 Population")
