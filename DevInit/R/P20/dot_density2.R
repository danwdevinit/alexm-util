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
p20.dots <- dotsInPolys(p20PolyTrans, p20PolyTrans$pop, f='random')
writeOGR(p20.dots, ".", "../points/p20_points", driver="ESRI Shapefile")