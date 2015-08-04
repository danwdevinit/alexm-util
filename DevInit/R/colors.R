setwd("C:/git/digital-platform/reference/")
colors <- read.csv("color.csv")
colors <- transform(colors,r=col2rgb(value)[1,],g=col2rgb(value)[2,],b=col2rgb(value)[3,])
write.csv(colors,"color.csv")
