# install.packages("RPostgreSQL")
# install.packages("sqldf")

library(RPostgreSQL)
library(sqldf)

db <- "tmp"
host <- "localhost"
username <- "postgres"
port <- 5432
# password <- ""

options(sqldf.RPostgreSQL.user = username, 
        sqldf.RPostgreSQL.password = password,
        sqldf.RPostgreSQL.dbname = db,
        sqldf.RPostgreSQL.host = host, 
        sqldf.RPostgreSQL.port = port)

# Establish connection to PoststgreSQL using RPostgreSQL
drv <- dbDriver("PostgreSQL")

# Full version of connection seetting
con <- dbConnect(drv, dbname=db,host=host,port=port,user=username,password=password )

dbExistsTable(con, "test")

sqldf("
 /* sql comments can be used*/
 drop table if exists test;
 create table test (a int, b int);
 insert into test values (1,4);
 insert into test values (2,5);
 insert into test values (3,6);
 ")

myTable <- dbReadTable(con, "test")
myTable <- sqldf("select * from test")

# overwrite=TRUE will change both data and table structure
# When row.name=TRUE then column named row.names will be added to the table
dbWriteTable(con, "test", value=myTable,overwrite=TRUE,row.names=FALSE)
  
dbWriteTable(con, "test", value=myTable,append=TRUE, row.names=FALSE)

data(mtcars)
dbWriteTable(con, "test", value=mtcars,overwrite=TRUE,row.names=FALSE)


sqldf("drop table if exists test;")

# Close PostgreSQL connection 
dbDisconnect(con)
