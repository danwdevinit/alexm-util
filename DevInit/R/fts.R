#install.packages("jsonlite")
#install.packages("curl")
library(jsonlite)
setwd("C:/git/alexm-util/DevInit/R")

root <- "http://fts.unocha.org/api/v1/"

####Meta-data####
countries <- fromJSON(paste(root,"country.json",sep=""))
sectors <- fromJSON(paste(root,"sector.json",sep=""))
organizations <- fromJSON(paste(root,"organization.json",sep=""))
emergencies <- fromJSON(paste(root,"Emergency/year/2000.json",sep=""))
for(i in 2001:2015){
  emergencies <- rbind(emergencies, fromJSON(paste(root,"Emergency/year/",i,".json",sep="")))
}
appeals <- fromJSON(paste(root,"Appeal/year/2000.json",sep=""))
for(i in 2001:2015){
  appeals <- rbind(appeals, fromJSON(paste(root,"Appeal/year/",i,".json",sep="")))
}
clusters <- list()
i = 1
while(length(clusters)==0){
  clusters <- fromJSON(paste(root,"cluster/appeal/",appeals$id[i],".json",sep=""))
  i <- i+1
  print(appeals$id[i])
}