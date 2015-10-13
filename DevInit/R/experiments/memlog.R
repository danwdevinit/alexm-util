wd <- "C:/Users/alexm/Documents"
setwd(wd)

#Read
dat <- read.csv("memlog.txt",header=FALSE,as.is=TRUE)
#Remove date-stamps without results
dat <- dat[c(1:2902,2904:2905,2911:nrow(dat)),]
odds <- dat[c(TRUE,FALSE),]
names(odds) <- c("datetime","freemem","blank1","blank2","blank3","blank4")
evens <- dat[c(FALSE,TRUE),]
names(evens) <- c("uptime","uptime2","users","loadavg.min","loadavg.5min","loadavg.15min")
dat <- cbind(odds,evens)
keep <- c(1:2,7:12)
dat <- dat[keep]

#Parse
dateFormat <- "%Y%m%d-%H%M%S"
dat <- transform(dat,datetime=strptime(datetime,dateFormat))
dat$freemem <- as.double(dat$freemem)
upSplit <- strsplit(dat$uptime," up ")
dat <- transform(dat,upDays=sapply(upSplit,"[[",2))
dat$upDays <- as.character(dat$upDays)
dat <- transform(dat,upDays=as.double(substr(upDays,1,nchar(upDays)-5)))
up2Split <- strsplit(dat$uptime2,":")
upHours <- double(length(up2Split))
upMins <- double(length(up2Split))
for(i in 1:length(up2Split)){
  hour = up2Split[[i]][1]
  if(grepl("min",hour)){
    min = as.double(substr(hour,1,nchar(hour)-4))
    hour = as.double(0)
  }else{
    hour = as.double(hour)
    min = as.double(up2Split[[i]][2])
  }
  upHours[i] = hour
  upMins[i] = min
}
dat$upHours <- upHours
dat$upMins <- upMins
keep <- c(1,9,10,11,2,5,6,7,8)
dat <- dat[keep]
dat <- transform(dat,users=substr(users,1,as.double(nchar(users)-5)))
dat <- transform(dat,loadavg.min = substr(loadavg.min,17,nchar(loadavg.min)))
dat$users <- as.double(dat$users)
dat$loadavg.min <- as.double(dat$loadavg.min)
dat$loadavg.5min <- as.double(dat$loadavg.5min)
dat$loadavg.15min <- as.double(dat$loadavg.15min)

#Analysis
summary(dat)
fit <- lm(loadavg.min~users,dat)
summary(fit)
plot(loadavg.min~users,dat)
boxplot(dat$loadavg.min)
boxplot(dat$loadavg.5min)
boxplot(dat$loadavg.15min)
require(ggplot2)
ggplot(aes(x = datetime, y = loadavg.15min), data = dat) + geom_line()
#install.packages("googleVis")
library(googleVis)
plot(
  gvisLineChart(dat,
                xvar="datetime",
                yvar="loadavg.15min",
                   options=list(
                     explorer="{actions: ['dragToZoom' 
                     'rightClickToReset'],
                     maxZoomIn:0.05}",
                     chartArea="{width:'85%',height:'80%'}",
                     hAxis="{title: 'Date', 
                     titleTextStyle: {color: '#000000'}}",
                     vAxis="{title: 'Rolling Load Average (15 min)', 
                     titleTextStyle: {color: '#000000'}}",
                     title="Load Average on server.developmentinitiatives.org",
                     width=1000, height=500,
                     legend="none"),
                   chartid="devinit")
)
