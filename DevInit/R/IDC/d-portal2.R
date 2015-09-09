library(jsonlite)
library(foreach)
library(plyr)

byCountry2014 <- fromJSON("http://www.d-portal.org/q?from=act%2Ctrans%2Ccountry&limit=-1&select=country_code%2Csum_of_percent_of_trans_usd&groupby=country_code&trans_code=D%7CE&trans_day_gteq=2014-01-01&trans_day_lt=2015-01-01&reporting_ref=GB-1&view=publisher_countries&_=1441812516302")[[1]] 
setwd("C:/git/alexm-util/DevInit/R/IDC/")
write.csv(byCountry2014,"byCountry2014.csv",row.names=FALSE,na="")

german2014 <- fromJSON("http://www.d-portal.org/q?from=act%2Ctrans%2Ccountry&limit=-1&select=country_code%2Csum_of_percent_of_trans_usd&groupby=country_code&trans_code=D%7CE&trans_day_gteq=2014-01-01&trans_day_lt=2015-01-01&reporting_ref=DE-1&view=publisher_countries&_=1441812516302")[[1]] 
write.csv(german2014,"german2014.csv",row.names=FALSE,na="")

usa2014 <- fromJSON("http://www.d-portal.org/q?from=act%2Ctrans%2Ccountry&limit=-1&select=country_code%2Csum_of_percent_of_trans_usd&groupby=country_code&trans_code=D%7CE&trans_day_gteq=2014-01-01&trans_day_lt=2015-01-01&reporting_ref=US&view=publisher_countries&_=1441812516302")[[1]] 
write.csv(usa2014,"usa2014.csv",row.names=FALSE,na="")