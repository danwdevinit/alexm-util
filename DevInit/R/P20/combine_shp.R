libs <- c("rgdal", "maptools", "gridExtra","rgeos","raster","data.table")
lapply(libs, require, character.only = TRUE)

combineShp <- function(infolder,outfile){
  dataList <- list()
  dataIndex <- 1
  
  shps <- list.files(path=infolder,pattern="*.shp",ignore.case=TRUE,full.names=TRUE)
  
  pb <- winProgressBar(
    title="Combining shapes"
    ,label=sprintf("0 out of %d shapefiles processed...",length(shps))
    ,min=0
    ,max=1
    ,initial=0
  )
  
  for(shp in shps){
    dat <- readShapeSpatial(shp)
    dataList[[dataIndex]] <- dat
    info <- sprintf("%d out of %d shapefiles processed...",dataIndex,length(shps))
    setWinProgressBar(pb,dataIndex/length(shps),label=info)
    dataIndex <- dataIndex + 1
  }
  info <- sprintf("Merging %d shapefiles...",length(shps))
  setWinProgressBar(pb,dataIndex/length(shps),label=info)
  merge <- do.call("rbind",dataList)
  info <- "Writing merged shapefile..."
  setWinProgressBar(pb,dataIndex/length(shps),label=info)
  writeOGR(merge, ".", outfile, driver="ESRI Shapefile")
  close(pb)
}

# wd <- "D:/Documents/Data/WorldPop/dots/"
# setwd(wd)
# combineShp("p20","p20_merge")