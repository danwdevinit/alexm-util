path <- "C:/Users/alexm/Downloads/data/AllData.csv"

keep <- c("Country.Name","Country.Code")

dat <- read.table(path
                ,fileEncoding="UTF-16"
                ,quote="\""
                ,sep=","
                ,header=TRUE
                ,fill=TRUE
                ,stringsAsFactors=FALSE
                ,comment="#"
                ,row.names=NULL)

#dat <- dat[which(dat$Units.of.Expenditures=="in million constant (2005) US$"),]

generalHealth <- dat[which(dat$Indicator.Name=="General government expenditure on health"),]
SS <- dat[which(dat$Indicator.Name=="Social security funds"),]
