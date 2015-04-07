#install.packages("rsdmx")
#install.packages("curl")
#install.packages("plyr")
library(rsdmx)
setwd("C:/git/alexm-util/DevInit/R")

t1URL <- "http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/TABLE1/20005+20001+801+1+2+301+68+3+18+4+5+40+20+21+6+701+742+22+7+820+8+76+9+69+61+50+10+11+12+302+20002+918+20006+72+62+30+82+75+546+552+83+70+84+45+77+87+566+732+764+55+576+20007.1.5.1140.A/all?startTime=2004&endTime=2013"

t1sdmx <- readSDMX(t1URL)
t1 <- as.data.frame(t1sdmx)
cls <- t1@codelists
codelists <- sapply(cls@codelists, function(x) x@id)

head(t1)
