library(foreign)
library(Hmisc)
library(plyr)
library(data.table)

wd <- "D:/Documents/Data/LSMS/"
setwd(wd)

dat <- read.spss("TZA_2012_LSMS_v01_M_SPSS_English_labels/ConsumptionNPS3.sav")
# View(attributes(dat)$variable.labels)
df <- data.frame(dat)

ex.rate <- 72.3917/46152.5

df$pcexpm <- (df$expmR/12)/df$hhsize
df$pcexpppp <- df$pcexpm*ex.rate

weights <- read.spss("TZA_2012_LSMS_v01_M_SPSS_English_labels/Y3_weights.sav")
weights.df <- data.frame(weights)

df <- join(df,weights.df,by="y3_hhid")
weighted.mean(df$pcexpm,df$y3_panelweight)

geo <- read.spss("TZA_2012_LSMS_v01_M_SPSS_English_labels/HouseholdGeovars_Y3.sav")
# View(attributes(geo)$variable.labels)

libs <- c("rgdal", "maptools", "gridExtra","rgeos","raster")
lapply(libs, require, character.only = TRUE)

geodf <- data.frame(geo)
geodf <- join(geodf,df,by="y3_hhid")
coordinates(geodf) <- ~lon_dd_mod+lat_dd_mod
proj4string(geodf) <- "+proj=longlat +datum=WGS84 +no_defs"
writeOGR(geodf, ".", "TZA_2012_LSMS_v01_M_SPSS_English_labels/geo", driver="ESRI Shapefile")
