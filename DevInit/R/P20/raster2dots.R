libs <- c("rgdal", "maptools", "gridExtra","rgeos","raster","data.table")
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
# Evidently out must be relative?
raster2dots <- function(r,out,resolution=1){
  
  bb <- bbox(r)
  cs <- c(0.000833333,0.000833333)*resolution
  cc <- bb[,1] + (cs/2)
  cd <- ceiling(diff(t(bb))/cs)
  grd <- GridTopology(cellcentre.offset=cc,cellsize=cs,cells.dim=cd)
  sp_grd <- SpatialGridDataFrame(grd,data=data.frame(id=1:prod(cd)),proj4string="+proj=longlat +datum=WGS84 +no_defs")
  rVals <- round(getValues(r))
  valMat <- round(getValues(r,format="matrix"))
  total.pop <- sum(rVals,na.rm=TRUE)
  
  set.seed(1234)
  grd_bb <- bbox(sp_grd)
  s1min <- grd_bb[1,1]
  s2min <- grd_bb[2,1]
  
  pb <- winProgressBar(
    title="Raster2Dots"
    ,label=sprintf("0 out of %d rows converted to dots",cd[2])
    ,min=0
    ,max=1
    ,initial=0
    ,width=500
  )
  
  s2_last <- 0
  s2 <- 0
  break_count <- 0
  people <- 0
  
  while(s2<=(cd[2]-1)){
    x <- c()
    y <- c()
    for(s2 in s2_last:(cd[2]-1)){
      info <- sprintf("Rows: %d/%d; Vector length: %d; Dots saved: %d",s2+1,cd[2],length(x),people)
      setWinProgressBar(pb,(s2+1)/cd[2],label=info)
      for(s1 in 0:(cd[1]-1)){
        one.grid.pop <- valMat[(cd[2]-s2),(s1+1)]
        if(!is.na(one.grid.pop)){
          if(one.grid.pop>0){
            this.s1min <- s1min+(s1*(0.000833333*resolution))
            this.s2min <- s2min+(s2*(0.000833333*resolution))
            rands1 <- runif(one.grid.pop,0,1)
            rands2 <- runif(one.grid.pop,0,1)
            x <- c(x,this.s1min+(rands1*(0.000833333*resolution)))
            y <- c(y,this.s2min+(rands2*(0.000833333*resolution)))
          }
        }
      }
      if(length(x)>500000){
        break
      }
    }
    s2_last <- s2 + 1
    break_count <- break_count + 1
    people <- people + length(x)
    info <- sprintf("Making checkpoint %d...",break_count)
    setWinProgressBar(pb,(s2+1)/cd[2],label=info)
    dots <- data.frame(x,y)
    dots <- dots[which(complete.cases(dots)),]
    coordinates(dots) <- ~x+y
    proj4string(dots) <- "+proj=longlat +datum=WGS84 +no_defs"
    dots <- SpatialPointsDataFrame(dots, data.frame(dummy = rep(0,length(dots))))
    info <- sprintf("Saving shp%d...",break_count)
    setWinProgressBar(pb,(s2+1)/cd[2],label=info)
    writeOGR(dots, ".", paste0(out,break_count), driver="ESRI Shapefile")
  }
  
  close(pb)
}

# wd <- "D:/Documents/Data/WorldPop/UGA-POP"
# setwd(wd)

# p20 <- raster("../pov-tifs/p20.tif")
# 
# Rprof(line.profiling=TRUE)
# dots <- raster2dots(p20)
# Rprof(NULL)
# plot(dots)
# summaryRprof(lines="both")
