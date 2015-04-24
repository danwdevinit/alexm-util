#install.packages('ggmap')
#install.packages('plyr')
library(plyr)
library(ggmap)

####Functions####

#Thanks jbryer
#https://github.com/jbryer/geocode
#' Returns the longitude and latitude of a geocoded point using the Yahoo
#' Maps API.
#' 
#' @param address the street address to geocode.
#' @param key the Yahoo Maps API key.
#' @references \url{http://developer.yahoo.com/geo/placefinder/guide/}
#' @return a geocoded point.
#' @export
geocode.yahoo <- function(address, key) {
  data(YahooAccuracyCodes)
  data(YahooStatusCodes)
  yahoo.url = "http://where.yahooapis.com/geocode?"
  u = url(URLencode( paste(yahoo.url, "q=", address, "&output=xml&appid=", key)))
  #print(u)
  r = readLines(u, n=-1, warn=FALSE)
  doc = xmlInternalTreeParse(r)
  root = xmlRoot(doc)
  lat = xmlValue(root[['Result']][["latitude"]][[1]])
  lon = xmlValue(root[['Result']][["longitude"]][[1]])
  close(u)
  sta = xmlValue(root[['Error']])
  acc = xmlValue(root[['Result']][['quality']])
  pt <- list(longitude=lon, 
             latitude=lat,
             address=address,
             accuracy.code=acc,
             accuracy=YahooAccuracyCodes[which(YahooAccuracyCodes$Value == acc),'Description'],
             status=YahooStatusCodes[which(YahooStatusCodes$Code == sta),'Code'],
             source='Yahoo'
  )
  class(pt) <- "geocode"
  return(pt)
}

####Data####

wd <- "C:/Users/alexm/Documents/Rwork"
setwd(wd)

data <- read.csv("Uganda Primary Leaving Exam Results 2014.csv"
                 , header = TRUE
                 ,sep=","
                 ,na.strings=""
                 ,check.names=TRUE
                 ,as.is=T
                 )

normalize <- function(school,district){
  split <- strsplit(school," ")[[1]]
  address <- paste(paste(split[c(2:(length(split)-2))],collapse=" "),"PRIMARY SCHOOL,",district,"UGANDA")
  return(address)
}

statMode <- function(x){
  temp <- table(x)
  names(temp)[temp==max(temp)]
}

res <- character(nrow(data))
for(j in 1:nrow(data)){
  school <- data[j,2]
  district <- data[j,1]
  res[j] <- normalize(school,district)
}
data$address <- res

schools <- count(data$address)
schools <- schools[order(-schools$freq),]
subset <- schools[1:100,]

lon <- numeric(nrow(subset))
lat <- numeric(nrow(subset))
for(i in 1:nrow(subset)){
  address <- as.character(subset[i,1])
  geo <- geocode(address)
  lon[i] <- geo[1]
  lat[i] <- geo[2]
}
subset$lon <- unlist(lon)
subset$lat <- unlist(lat)

geocoded <- merge(
  data
  ,subset
  ,by.x="address"
  ,by.y="x"
  )

geodata <- ddply(geocoded,.(address,lat,lon),summarize,mode=statMode(AGG))
