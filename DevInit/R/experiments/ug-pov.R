###Thanks to Kyle Walker, http://rpubs.com/walkerke/wdi_leaflet
#install.packages('rgdal')
#install.packages('devtools')
#install.packages('plyr')
#install.packages('raster')
#library(devtools)
#devtools::install_github("rstudio/leaflet")
library(raster)
library(rgdal)
library(leaflet)
library(plyr)

setwd("C:/Users/alexm/Documents/Rwork/GIS/UG-POV")

pov <- raster("uga10povcons125.tif")

pal <- colorNumeric(c("#B7BF10", "#EA7600", "#BA0C2F"), values(pov),
                    na.color = "transparent")

leaflet() %>%
  addTiles() %>%
  addRasterImage(pov,colors = pal,opacity=0.8) %>%
  setView(32, 1, zoom = 7)
