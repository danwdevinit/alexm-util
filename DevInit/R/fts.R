#install.packages("RJSONIO")
require(RJSONIO)
daturl = "http://fts.unocha.org/api/v1/country.json"
dat = RJSONIO::fromJSON(daturl, nullValue=NA)[[2]]
