wd <- "D:/Documents/Data/WorldPop/vectors/pixel_vectors"
setwd(wd)

library(foreign)

data <- read.dbf("pixel_vectors.dbf")
sum(data$people)