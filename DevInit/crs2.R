path <- "S:/Projects/Programme resources/Data/GHA calcs and analyses/April 2015/Datasets - do not edit/CRS/CRS 1973-94 data-enc.txt"

dat <- read.table(
  path
  ,header = TRUE
  ,sep = "|"
  ,fileEncoding="UTF-8"
  ,na.strings=NULL
  ,quote=""
  ,as.is=T
  ,fill=TRUE
)
