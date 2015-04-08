#install.packages("rsdmx")
#install.packages("curl")
#install.packages("plyr")
library(rsdmx)
setwd("C:/git/alexm-util/DevInit/R")

#Func####
OECD <- function(url,concept=FALSE){
  #Separate out data URL components
  dRoot <- "http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/"
  indicator <- strsplit(substr(url,nchar(dRoot)+1,nchar(url)),"/")[[1]][1]
  filter <- substr(url,nchar(dRoot)+1+nchar(indicator),nchar(url))
  #Structure URL
  sRoot <- "http://stats.oecd.org/restsdmx/sdmx.ashx/GetDataStructure/"
  t1sUrl <- paste(sRoot
                  ,indicator
                  ,sep = "")
  #Fetch data
  t1dsdmx <- readSDMX(url)
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
  if(concept){
    return(concepts)
  }else{
    return(t1)
  }
}

#E.g.####
url <- "http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/TABLE1/20005+20001+801+1+2+301+68+3+18+4+5+40+20+21+6+701+742+22+7+820+8+76+9+69+61+50+10+11+12+302+20002+918+20006+72+62+30+82+75+546+552+83+70+84+45+77+87+566+732+764+55+576+20007.1.1010.1140.A/all?startTime=2004&endTime=2013"
t1 <- OECD(url)
t1Concepts <- OECD(url,TRUE)
