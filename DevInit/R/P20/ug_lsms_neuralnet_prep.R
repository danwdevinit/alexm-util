library(foreign)
library(data.table)
library(plyr)
library(Hmisc)

wd <- "D:/Documents/Data/LSMS/UGA_2011_UNPS_v01_M_STATA"

setwd(wd)

dat <- read.dta("UNPS 2011-12 Consumption Aggregate.dta")
var.labs <- data.frame(names(dat),attributes(dat)[7])
View(var.labs)

