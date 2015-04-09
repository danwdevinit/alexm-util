path <- "C:/Users/alexm/Downloads/data/AllData.csv"

keep <- c("Country.Name","Country.Code","Indicator.Name","Indicator.Unit","Units.of.Expenditures","Year","Value")

dat <- read.csv(path
                ,fileEncoding="UTF-16")[keep]

#dat <- dat[which(dat$Units.of.Expenditures=="in million constant (2005) US$"),]

generalHealth <- dat[which(dat$Indicator.Name=="General government expenditure on health"),]
SS <- dat[which(dat$Indicator.Name=="Social security funds"),]
