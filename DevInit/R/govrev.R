path<- "C:/git/digital-platform/country-year/"
setwd(path)

df <- read.csv("./domestic.csv", header = TRUE,sep=",",na.strings="",check.names=FALSE,stringsAsFactors=FALSE)
gdp <- read.csv("./gdp-current-ncu-fy.csv", header = TRUE,sep=",",na.strings="",check.names=FALSE,stringsAsFactors=FALSE)
id <- c()
year <- c()
value <- c()
estimate <- c()

for(i in 1:nrow(df)){
  row <- df[i,]
  dfid <- row[1][1,1]
  dfyear <- row[2][1,1]
  l1 <- row[5][1,1]
  l2 <- row[6][1,1]
  l3 <- row[7][1,1]
  dfvalue <- row[12][1,1]
  if(!is.na(l1) && !is.na(l2)){
    if(l1=="total-revenue-and-grants" && l2=="revenue" && is.na(l3)){
      id <- c(id,dfid)
      year <- c(year,dfyear)
      thisGDP <- gdp[which(gdp$id==dfid),]
      thisGDP <- thisGDP[which(thisGDP$year==dfyear),]
      if(nrow(thisGDP)>0){
        if(is.na(thisGDP$value[[1]])){
          value <- c(value,NA)
          estimate <- c(estimate,NA)
        }else{
          value <- c(value,dfvalue/thisGDP$value[[1]])
          estimate <- c(estimate,thisGDP$estimate[[1]])
        }
      }else{
        value <- c(value,NA)
        estimate <- c(estimate,NA)
        print(paste("No multiplier for:",dfid,dfyear))
      }
    }
  }
}
newdf <- data.frame(id,year,value,estimate)
write.csv(newdf,"./gov-revenue-pc-gdp.csv",row.names=FALSE,na="")
