#install.packages("rsdmx")
#install.packages("curl")
#install.packages("plyr")
library(rsdmx)
setwd("C:/git/alexm-util/DevInit/R")

#Data url
indicator <- "CRS1"
dRoot <- "http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/"
t1dUrl <- paste(dRoot
                ,indicator
                ,"/20005+20001+801+1+2+301+68+3+18+4+5+40+20+21+6+701+742+22+7+820+8+76+9+69+61+50+10+11+12+302+20002+913+914+921+915+916+953+990+918+1311+811+1313+1312+901+905+909+912+988+958+976+104+951+978+971+959+948+974+967+963+923+964+966+928+20006+82+552+576+21600+1601.10100.1000.100.100.A.115.100/all?startTime=2004&endTime=2013"
                ,sep = "")
#Structure URL
sRoot <- "http://stats.oecd.org/restsdmx/sdmx.ashx/GetDataStructure/"
t1sUrl <- paste(sRoot
                ,indicator
                ,sep = "")

#Fetch data
t1dsdmx <- readSDMX(t1dUrl)
t1ssdmx <- readSDMX(t1sUrl)
#Convert to DF
t1 <- as.data.frame(t1dsdmx)
#get codelists
cls <- t1ssdmx@codelists
codelists <- sapply(cls@codelists, function(x) x@id)
#Recode
for(i in 1:length(codelists)){
  suffix <- paste("CL_",indicator,"_",sep="")
  clName <- substr(codelists[i],nchar(suffix)+1,nchar(codelists[i]))
  codelist <- cls@codelists[i][[1]]@Code
  for(j in 1:length(codelist)){
    id <- codelist[j][[1]]@id
    name <- codelist[j][[1]]@label$en
    if(clName %in% colnames(t1)){
      t1[clName][which(t1[clName]==id),] <- name
    }
  }
}
#get concepts
concepts <- as.data.frame(t1ssdmx@concepts)
