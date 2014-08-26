install.packages("ggplot2")
library(ggplot2)

set.seed(1410)

dsmall <-diamonds[sample(nrow(diamonds),100),]
View(dsmall)

X11()
qplot(x = carat, y=price,data=diamonds)
#or
qplot(carat,price,data=diamonds)

X11()
qplot(depth,price,data=diamonds)

#Data manipulation within function
X11()
qplot(x = log(carat), y=log(price),data=diamonds)

X11()
qplot(carat,x*y*z,data=diamonds)

#Adding color
X11()
qplot(carat,price,data=dsmall,color=color)

#Cut
X11()
qplot(carat,price,data=dsmall,shape=cut)

#Transparency
X11()
qplot(carat,price,data=diamonds,alpha=I(1/10))

#Splines
library(splines)
X11()
qplot(carat,price,data=dsmall,geom = c("point","smooth"),method="lm")
X11()
qplot(carat,price,data=dsmall,geom = c("point","smooth"),method="loess")

qplot(color,price/carat,data=diamonds,geom="jitter",alpha=I(1/50))

#Histogram
X11()
qplot(carat,data=diamonds,geom="histogram",binwidth=0.01,xlim=c(0,3))
X11()
qplot(carat,data=diamonds,geom="histogram",xlim=c(0,3),xlab="Carat",
      ylab="Count",main="Histogram",color=color)
X11()
qplot(carat,data=diamonds,geom="histogram",xlim=c(0,3),xlab="Carat",
      ylab="Count",main="Histogram",fill=color)
X11()
qplot(carat,data=diamonds,geom="density",xlim=c(0,3),xlab="Carat",
      ylab="Count",main="Histogram",color=color)
X11()
qplot(carat,data=diamonds,geom="density",xlim=c(0,3),xlab="Carat",
      ylab="Count",main="Histogram",fill=color)

X11()
qplot(hwy,cty,data=mpg)

#Change how qplots are displayed
X11()
qplot(displ,hwy,data=mpg, facets= . ~ year) + geom_smooth()

X11()
qplot(displ,hwy,data=mpg, facets= year~.) + geom_smooth()

X11()
qplot(carat,price,data=diamonds,facets=color~cut) + geom_smooth()

#Reshaping data
install.packages("reshape2")
library(reshape2)

names(airquality) <- tolower(names(airquality))
head(airquality)

#Wide to long
aql <- melt(airquality)
head(aql)
tail(aql)

aql <- melt(airquality,id.vars=c('month','day'))
head(aql)
aql <- melt(airquality,id.vars = c('month','day'),variable.name = "climate_variable",value.name="climate_value")
head(aql)

#Long to wide
aqw <- dcast(aql,month+day~variable)
head(aqw)
