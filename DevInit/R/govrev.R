path<- "C:/git/digital-platform/country-year/"
setwd(path)

df <- read.csv("./domestic.csv",colClasses=c("character","numeric","character","character","character","character","character","character","character","numeric","numeric"), header = TRUE,sep=",",na.strings="",check.names=FALSE,stringsAsFactors=FALSE)
gdp <- read.csv("./gdp-current-ncu-fy.csv",colClasses=c("character","numeric","numeric"), header = TRUE,sep=",",na.strings="",check.names=FALSE,stringsAsFactors=FALSE)
id <- c()
year <- c()
value <- c()

for(i in 1:nrow(df)){
  row <- df[i,]
  dfid <- row[1][1,1]
  dfyear <- row[2][1,1]
  l1 <- row[4][1,1]
  l2 <- row[5][1,1]
  l3 <- row[6][1,1]
  dfvalue <- row[11][1,1]
  if(!is.na(l1) && !is.na(l2)){
    if(l1=="total-revenue-and-grants" && l2=="revenue" && is.na(l3)){
      if(dfyear<=2014){
        id <- c(id,dfid)
        year <- c(year,dfyear)
        thisGDP <- gdp[which(gdp$id==dfid),]
        thisGDP <- thisGDP[which(thisGDP$year==dfyear),]
        if(nrow(thisGDP)>0){
          if(is.na(thisGDP$value[[1]])){
            value <- c(value,NA)
          }else{
            value <- c(value,(dfvalue/thisGDP$value[[1]])*100)
          }
        }else{
          value <- c(value,NA)
          print(paste("No multiplier for:",dfid,dfyear))
        }
      }
    }
  }
}
newdf <- data.frame(id,year,value)
write.csv(newdf,"./gov-revenue-pc-gdp.csv",row.names=FALSE,na="")
