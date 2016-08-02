# install.packages("data.table", type = "source",
#                  repos = "http://Rdatatable.github.io/data.table")

library(data.table)

wd <- "D:/Documents/Data/WorldPop/UGA-POP"
setwd(wd)

libs <- c("rgdal", "maptools", "gridExtra","rgeos","raster")
lapply(libs, require, character.only = TRUE)
if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit()

# r <- raster("UGA_pph_v2b_2015_UNadj.tif")
# 
# data <- data.table(coordinates(r))
# data$z <- getValues(r)
# dat <- data
# coordinates(data) <- ~x+y
# ug <- readOGR(dsn = "../voronoi/voronoi.shp", layer = "voronoi")
# proj4string(data) <- proj4string(ug)
# 
# ug.data <- over(data,ug[,c("DHSCLUST","p20","p50","gp50")])
# data <- dat
# ug.data <- cbind(data,ug.data)
# ug.data <- data.table(ug.data)
# ug.data.tab <- ug.data[,c("pp20","pp50","pgp50") := list(round(z*p20),round(z*p50),round(z*gp50))]
# 
# p20 <- rasterFromXYZ(ug.data.tab[,c(1,2,8),with=FALSE])
# p50 <- rasterFromXYZ(ug.data.tab[,c(1,2,9),with=FALSE])
# gp50 <- rasterFromXYZ(ug.data.tab[,c(1,2,10),with=FALSE])
# writeRaster(p20,"../pov-tifs/p20.tif")
# writeRaster(p50,"../pov-tifs/p50.tif")
# writeRaster(gp50,"../pov-tifs/gp50.tif")

p20 <- raster("../pov-tifs/p20.tif")

r <- p20
resolution <- 1
bb <- bbox(r)
cs <- c(0.000833333,0.000833333)*resolution
cc <- bb[,1] + (cs/2)
cd <- ceiling(diff(t(bb))/cs)
grd <- GridTopology(cellcentre.offset=cc,cellsize=cs,cells.dim=cd)
sp_grd <- SpatialGridDataFrame(grd,data=data.frame(id=1:prod(cd)),proj4string="+proj=longlat +datum=WGS84 +no_defs")
rVals <- round(getValues(r))
valMat <- round(getValues(r,format="matrix"))
total.pop <- sum(rVals,na.rm=TRUE)

dots <- data.frame(
  x=double(total.pop)
  ,y=double(total.pop)
  )

dots$x <- NA
dots$y <- NA

set.seed(1234)
grd_bb <- bbox(sp_grd)
s1min <- grd_bb[1,1]
s2min <- grd_bb[2,1]

i <- 0
for(s2 in 0:(cd[2]-1)){
  message(s2,"/",cd[2])
  for(s1 in 0:(cd[1]-1)){
    one.grid.pop <- valMat[(cd[2]-s2),(s1+1)]
    if(!is.na(one.grid.pop)){
      if(one.grid.pop>0){
        this.s1min <- s1min+(s1*(0.000833333*resolution))
        this.s2min <- s2min+(s2*(0.000833333*resolution))
        rands1 <- runif(one.grid.pop,0,1)
        rands2 <- runif(one.grid.pop,0,1)
        dots[(1 + i):(i + one.grid.pop),1] <- this.s1min+(rands1*(0.000833333*resolution))
        dots[(1 + i):(i + one.grid.pop),2] <- this.s2min+(rands2*(0.000833333*resolution))
        i <- i + one.grid.pop
      }
    }
  }
}

dots <- dots[which(complete.cases(dots)),]
save(dots,file="dots.RData")
# load("dots.RData")
coordinates(dots) <- ~x+y
proj4string(dots) <- "+proj=longlat +datum=WGS84 +no_defs"
dots <- SpatialPointsDataFrame(dots, data.frame(dummy = rep(0,length(dots))))
writeOGR(dots, ".", "../better_dots/p20_points", driver="ESRI Shapefile")
