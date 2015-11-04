#Setwd
wd <- "C:/git/alexm-util/DevInit/START"
setwd(wd)
#Read glide
glide <- read.csv("glide_results.csv",as.is=TRUE,na.strings="")
#Drop indeterminate countries
glide <- subset(
  glide
  ,Country!="(Non-Localized)"
  )
#Split date
dateSplit <- strsplit(glide$Date,"/")
glide <- transform(
  glide
  ,year=as.integer(sapply(dateSplit,"[[",1))
  ,month=as.integer(sapply(dateSplit,"[[",2))
  ,day=as.integer(sapply(dateSplit,"[[",3))
  )
#Fix zeroes in date, set to jan 1
glide$month[which(glide$month<1)] <- 1
glide$day[which(glide$day<1)] <- 1
#Flip one American style date
glide[398,]$month <- 11
glide[398,]$day <- 30
#Parse date
glide <- transform(
  glide
  ,Date=as.Date(paste(year,month,day,sep="-"))
  )
#Drop extra vars
glide <- glide[c(1:4)]
#Extract ISO3 from Glide number
glideSplit <- strsplit(glide$GLIDE_number,"-")
glide <- transform(
  glide
  ,Iso3=as.character(sapply(glideSplit,"[[",4))
)
