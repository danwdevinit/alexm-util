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

windows <- TRUE
if(windows){pathpre<-"C:"}else{pathpre<-"~"}

setwd(paste0(pathpre,"/git/digital-platform/country-year"))

#Load data
educInput <- read.csv("uganda-primary-educ-funding.csv")
names(educInput)[3] <- "funding"
educInput <- subset(educInput,year==2015)
educOutput <- read.csv("uganda-leaving-exam-perf-rate.csv")
names(educOutput)[3] <- "examPerf"
educOutput <- subset(educOutput,year==2013)

#Merge
educ <- merge(
  educInput
  ,educOutput
  ,by=c("id"))

#Examine
fit <- lm(examPerf~funding,data=educ)
summary(fit)
hist(educ$funding) #log-normal
hist(educ$examPerf #normal

#Reshape
educ <- transform(educ,lf=log(funding))
hist(educ$lf) #better
educ <- transform(educ,dmlf=lf-mean(lf,na.rm=TRUE))
hist(educ$dmlf)
educ <- transform(educ,dmep=examPerf-mean(examPerf,na.rm=TRUE))
hist(educ$dmep)

fit <- lm(examPerf~lf,data=educ)
summary(fit)
plot(examPerf~lf,data=educ)
abline(lm(examPerf~lf,data=educ))

#Vis
plot(dmep~dmlf,data=educ)

#categorize
quad <- function(xVector,yVector){
  quads <- character(length(xVector))
  for(i in 1:length(xVector)){
    x <- xVector[i]
    y <- yVector[i]
    if(!is.na(x)&!is.na(y)){
      if(x>0&y>0){
        quads[i]<-"i"
      } else if(x<0&y>0){
        quads[i]<-"ii"
      } else if(x<0&y<0){
        quads[i]<-"iii"
      } else if(x>0&y<0){
        quads[i]<-"iv"
      } else{
        quads[i]<-as.character(NA)
      }
    } else{
      quads[i]<-as.character(NA)
    }
  }
  return(quads)
}

educ <- transform(educ,quad=quad(dmlf,dmep))

#Map
districts <- readOGR(dsn = paste0(pathpre,"/git/digital-platform/shape-files/uganda/uganda.shp"), 
                     layer = "uganda", 
                     encoding = "UTF-8",
                     verbose = FALSE)
mapData <- merge(
  districts
  ,educ
  ,by=c("id")
  )

pal <- colorFactor("Blues", c("i","ii","iii","iv"))

popup <- paste0("<strong>District: </strong>", 
                mapData$name, 
                "<br><strong>", 
                "I/O Demeaned Quad", 
                ": </strong>", 
                mapData$quad)

leaflet(data = mapData) %>%
  #addTiles() %>%
  setView(32, 1, zoom = 7) %>%
  addPolygons(fillColor = ~pal(mapData$quad), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1, 
              popup = popup) %>%
  addLegend(pal=pal,values=mapData$quad)