# install.packages("data.table", type = "source",
#                  repos = "http://Rdatatable.github.io/data.table")

library(data.table)

wd <- "D:/Documents/Data/WorldPop/UGA-POP"
setwd(wd)

libs <- c("rgdal", "maptools", "gridExtra","rgeos","raster")
lapply(libs, require, character.only = TRUE)
if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit()

r <- raster("UGA_pph_v2b_2015_UNadj.tif")

data <- data.table(coordinates(r))
data$z <- getValues(r)
dat <- data
coordinates(data) <- ~x+y
ug <- readOGR(dsn = "../voronoi/voronoi.shp", layer = "voronoi")
proj4string(data) <- proj4string(ug)

ug.data <- over(data,ug[,c("DHSCLUST","p20","p50","gp50")])
data <- dat
ug.data <- cbind(data,ug.data)
ug.data <- data.table(ug.data)
ug.data.tab <- ug.data[,c("pp20","pp50","pgp50") := list(round(z*p20),round(z*p50),round(z*gp50))]

source("C:/git/alexm-util/DevInit/R/p20/poly.R")
p20 <- rasterFromXYZ(ug.data.tab[,c(1,2,8),with=FALSE])
p50 <- rasterFromXYZ(ug.data.tab[,c(1,2,9),with=FALSE])
gp50 <- rasterFromXYZ(ug.data.tab[,c(1,2,10),with=FALSE])
writeRaster(p20,"../pov-tifs/p20.tif")
writeRaster(p50,"../pov-tifs/p50.tif")
writeRaster(gp50,"../pov-tifs/gp50.tif")

p20Poly <- polygonizer("../pov-tifs/p20.tif")
proj4string(p20Poly) <- "+proj=longlat +datum=WGS84 +no_defs"
p20PolyTrans <- spTransform(p20Poly,CRS("+proj=utm +zone=36 +datum=WGS84 +units=m +no_defs"))
p20PolyTrans$pop <- (gArea(p20PolyTrans,byid=TRUE)/10000)*p20PolyTrans$DN
save(p20PolyTrans,file="../p20poly_backup.RData")
# load("../p20poly_backup.RData")
library(plyr)

tenthDots <- function(h,p20PolyTrans,last){
  tenth <- round(nrow(p20PolyTrans)/10)
  
  dataList <- list()
  dataIndex <- 1
  
  for(i in last:tenth){
    j <- i * 10
    if(j>=tenth*h){
      break
    }
    k <- 1+(10*(i-1)):j
    m <- k[which(k <= nrow(p20PolyTrans))]
    n <- m[which(p20PolyTrans$pop[m]>0 & p20PolyTrans$pop[m]<250000)]
    message(j,"/",nrow(p20PolyTrans))
    if(length(n)>0){
      p20.dot <- dotsInPolys(p20PolyTrans[n,], round(p20PolyTrans$pop[n]), f='random')
      dataList[[dataIndex]] <- p20.dot 
      dataIndex <- dataIndex + 1
    }
  }
  
  p20.dots <- do.call("rbind",dataList)
  writeOGR(p20.dots, ".", paste0("../points/p20_points",h), driver="ESRI Shapefile")
  return(i)
}

# last <- tenthDots(1,p20PolyTrans,1)
# last <- tenthDots(2,p20PolyTrans,last)
# last <- tenthDots(3,p20PolyTrans,last)
# last <- tenthDots(4,p20PolyTrans,last)
# last <- 6095
# last <- tenthDots(5,p20PolyTrans,last)
# last <- tenthDots(6,p20PolyTrans,last)
# last <- tenthDots(7,p20PolyTrans,last)
# last <- tenthDots(8,p20PolyTrans,last)
# last <- 12190
# last <- tenthDots(9,p20PolyTrans,last)
# last <- 13714
# last <- tenthDots(10,p20PolyTrans,last)

big.ones <- which(p20PolyTrans$pop>=250000)
# for(i in 1:length(big.ones)){
#   h <- i+10
#   j <- big.ones[i]
#   p20.dots <- dotsInPolys(p20PolyTrans[j,], round(p20PolyTrans$pop[j]), f='random')
#   writeOGR(p20.dots, ".", paste0("../points/p20_points",h), driver="ESRI Shapefile")
# }

bb <- bbox(p20PolyTrans[big.ones,])
cs <- c(3.28084,3.28084)*6000
cc <- bb[,1] + (cs/2)
cd <- ceiling(diff(t(bb))/cs)
grd <- GridTopology(cellcentre.offset=cc,cellsize=cs,cells.dim=cd)
sp_grd <- SpatialGridDataFrame(grd,data=data.frame(id=1:prod(cd)),proj4string=CRS(proj4string(p20PolyTrans)))

grd_shp <- as(sp_grd,"SpatialPolygonsDataFrame")

library("lattice")
spplot(sp_grd, "id",
       panel = function(...) {
         panel.gridplot(..., border="black")
         sp.polygons(p20PolyTrans[big.ones,])
         panel.text(...)
       })

# simplify the polgons a tad (tweak 0.00001 to your liking)
big.Simple <- gSimplify(p20PolyTrans[big.ones,], tol = 0.00001)

# this is a well known R / GEOS hack (usually combined with the above) to 
# deal with "bad" polygons
big.Simple <- gBuffer(big.Simple, byid=TRUE, width=0)

# any bad polys?
sum(gIsValid(big.Simple, byid=TRUE)==FALSE)

big.Simple <- as(big.Simple,"SpatialPolygonsDataFrame")
big.Simple$dummy <- NULL
big.Simple$DN <- p20PolyTrans[big.ones,]$DN
splitShapes <- intersect(big.Simple,grd_shp)
splitShapes$pop <- (gArea(splitShapes,byid=TRUE)/10000)*splitShapes$DN

fifth <- round(nrow(splitShapes)/25)

dataList <- list()
dataIndex <- 1

for(i in 1:fifth){
  j <- i * 25
  k <- 1+(25*(i-1)):j
  m <- k[which(k <= nrow(splitShapes))]
  n <- m[which(splitShapes$pop[m]>0)]
  message(j,"/",nrow(splitShapes))
  if(length(n)>0){
    p20.dot <- dotsInPolys(splitShapes[n,], round(splitShapes$pop[n]), f='random')
    dataList[[dataIndex]] <- p20.dot 
    dataIndex <- dataIndex + 1
  }
}

p20.dots <- do.call("rbind",dataList)
writeOGR(p20.dots, ".", paste0("../points/p20_points",11), driver="ESRI Shapefile")