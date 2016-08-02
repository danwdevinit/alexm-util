libs <- c("rgdal", "maptools", "gridExtra","rgeos","raster")
lapply(libs, require, character.only = TRUE)
if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit()
# Function for turning raster into randomly distributed points by z axis (rounded to integer)
# Purpose is to make dot-density maps
# Arguments are a raster object from Raster library and a resolution
# Resolution is in relation to 100m x 100m square, so if your raster is in hectares, resolution is 1
# Much faster than other point functions (like dotsInPoly) because we're starting with a grid
# So we don't need to make any assumptions about shape
# Pretty sure this only works in WGS84 in map units for the moment, one would need to change cs var
# To make it work with meters
# sp_grd is just calculated to ensure our raster bounding-box perfectly fits the projection
# after which point, all we do is iterate through the data values and assign dots with
# random off-sets inside each grid square
raster2dots <- function(r,resolution){
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
  coordinates(dots) <- ~x+y
  proj4string(dots) <- "+proj=longlat +datum=WGS84 +no_defs"
  dots <- SpatialPointsDataFrame(dots, data.frame(dummy = rep(0,length(dots))))
  return(dots)
}