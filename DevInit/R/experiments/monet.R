# install.packages("MonetDB.R")

library(MonetDB.R)

conn <- dbConnect(dbDriver("MonetDB"),"monetdb://localhost/demo")

data(iris)

monetNames <- function(df){
  x <- names(df)
  x <- tolower(x)
  x <- gsub(".","_",x,fixed=TRUE)
  return(x)
}

rNames <- function(df){
  x <- names(df)
  x <- gsub("_",".",x,fixed=TRUE)
  return(x)
}

names(iris) <- monetNames(iris)

dbWriteTable(conn,"iris",iris,overwrite=TRUE)

ds1 <- dbGetQuery(conn,"Select avg(sepal_width) FROM iris")
ds2 <- dbGetQuery(conn,"Select * FROM iris")

dbDisconnect(conn)


