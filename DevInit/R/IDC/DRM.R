###By Alex Miller
#install.packages('plyr')
#require(devtools)
#install_github('ramnathv/rCharts')
library(plyr)
library(rCharts)
####Data####
#Set the working directory
wd <- "S:/Projects/Programme resources/Data/GHA calcs and analyses/April 2015/Datasets - do not edit/CRS/Other/CRS CSV files April 2015/"
setwd(wd)

#Define the datasets we want to work with
data <- read.csv("CRS 2013 data.csv",na.strings=FALSE)
