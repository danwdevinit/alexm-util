povcal <- function(dhscc,year){
  library(curl)
  yearDict <- list(
    "1990"=1990
    ,"1991"=1990
    ,"1992"=1993
    ,"1993"=1993
    ,"1994"=1993
    ,"1995"=1996
    ,"1996"=1996
    ,"1997"=1996
    ,"1998"=1999
    ,"1999"=1999
    ,"2000"=1999
    ,"2001"=2002
    ,"2002"=2002
    ,"2003"=2002
    ,"2004"=2005
    ,"2005"=2005
    ,"2006"=2005
    ,"2007"=2008
    ,"2008"=2008
    ,"2009"=2008
    ,"2010"=2012
    ,"2011"=2011
    ,"2012"=2012
    ,"2013"=2012
    ,"2014"=2012
    ,"2015"=2012
    ,"2016"=2012
    ,"2017"=2012
  )
  isos <- read.csv("D:/Documents/Data/DHS map/isos.csv",as.is=TRUE)
  cuts <- read.csv("D:/Documents/Data/DHS map/cuts.full.csv",na.strings="",as.is=TRUE)
  sub <- subset(isos,cc==dhscc)
  #   subcuts <- subset(cuts,DHSYEAR==year)
  subcuts <- subset(cuts,DHSYEAR==yearDict[as.character(year)])
  if(nrow(sub)>0){
    iso3 <- sub$iso3[1]
  }else{
    iso3 <- readline(prompt=paste0("Enter ISO3 for ",dhscc,": "))
  }
  data <- c()
  steps <- names(cuts)[2:6]
  for(step in steps){
    cut <- subcuts[step][1,1]
    url <- paste0(
      "http://iresearch.worldbank.org/PovcalNet/Detail.aspx?Format=Detail&C0="
      ,iso3
      ,"_3"
      ,"&PPP0=0&PL0="
      ,cut
      ,"&Y0="
      ,yearDict[as.character(year)]
      ,"&NumOfCountries=1"
    )
    con <- curl(url)
    open(con)
    text <- readLines(curl(url))
    closeAllConnections()
    grepResults <- grep("Headcount(HC): ",text,fixed=TRUE)
    if(length(grepResults)>0){
      datum <- as.numeric(trimws(strsplit(text[grepResults[length(grepResults)]],":")[[1]][2]))
      if(datum>1){
        datum <- datum/100
      }
    }else{
      datum <- NA
    }
    data <- c(data,datum)
  }
  names(data) <- steps
  return(data)
}