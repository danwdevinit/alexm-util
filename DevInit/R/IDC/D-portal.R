library(jsonlite)
library(foreach)

gbs <- c(
  "GB-1"
  ,"GB-3"
  ,"GB-4"
  ,"GB-6"
  ,"GB-7"
  ,"GB-9"
  ,"GB-10"
  ,"XM-DAC-12-22"
  ,"GB-GOV-8"
  ,"GB-COH-RC000346"
  )

empty <- fromJSON("http://www.d-portal.org/q?from=act%2Ctrans%2Csector&limit=-1&select=sector_code%2Csum_of_percent_of_trans_usd&groupby=sector_code&trans_code=D%7CE&trans_day_gteq=2014-01-01&trans_day_lt=2015-01-01&reporting_ref=GB-1&view=publisher_sectors&_=1441789807497")[[1]][0,]
empty$year <- double(0)
empty$donor <- character(0)

data <- foreach(i=1:length(gbs),.packages=c("jsonlite")
                ,.combine=rbind) %do% {
  gb <- gbs[i]
  statsUrl <- paste0("http://www.d-portal.org/q?select=stats&from=act&reporting_ref=",gb,"&view=publisher&_=1441790527948")
  donorname <- fromJSON(statsUrl)[[1]]["MAX(reporting)"][1,1]
  message(donorname)
  
  url2013 <- paste0("http://www.d-portal.org/q?from=act%2Ctrans%2Csector&limit=-1&select=sector_code%2Csum_of_percent_of_trans_usd&groupby=sector_code&trans_code=D%7CE&trans_day_gteq=2013-01-01&trans_day_lt=2014-01-01&reporting_ref=",gb,"&view=publisher_sectors&_=1441789807495")
  dat2013 <- fromJSON(url2013)[[1]]
  dat2013$year <- 2013
  dat2013$donor <- donorname
  if(is.null(nrow(dat2013))){
    dat2013 <- empty
  }
  
  url2014 <- paste0("http://www.d-portal.org/q?from=act%2Ctrans%2Csector&limit=-1&select=sector_code%2Csum_of_percent_of_trans_usd&groupby=sector_code&trans_code=D%7CE&trans_day_gteq=2014-01-01&trans_day_lt=2015-01-01&reporting_ref=",gb,"&view=publisher_sectors&_=1441789807497")
  dat2014 <- fromJSON(url2014)[[1]]
  dat2014$year <- 2014
  dat2014$donor <- donorname
  if(is.null(nrow(dat2014))){
    dat2014 <- empty
  }
  
  url2015 <- paste0("http://www.d-portal.org/q?from=act%2Cbudget%2Csector&limit=-1&select=sector_code%2Csum_of_percent_of_budget_usd&budget_priority=1&groupby=sector_code&budget_day_end_gteq=2015-01-01&budget_day_end_lt=2016-01-01&reporting_ref=",gb,"&view=publisher_sectors&_=1441789807501")
  dat2015 <- fromJSON(url2015)[[1]]
  dat2015$year <- 2015
  dat2015$donor <- donorname
  names(dat2015)[2] <- "sum_of_percent_of_trans_usd"
  if(is.null(nrow(dat2015))){
    dat2015 <- empty
  }
  
  
  dat <- rbind(dat2013,dat2014,dat2015)
  return(dat)
}

totals <- ddply(data,.(donor,year),summarize,sum=sum(sum_of_percent_of_trans_usd,na.rm=TRUE))
