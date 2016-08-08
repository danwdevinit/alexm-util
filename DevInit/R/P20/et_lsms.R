library(foreign)
library(Hmisc)

wd <- "D:/Documents/Data/LSMS/"
setwd(wd)

dat <- read.spss("TLSLS2SAV/consumptiontl07.sav")
# View(attributes(dat)$variable.labels)

weighted.mean(dat$pcexpm,dat$hhweight)

ex.rate <- 2.375866

dat$pcexpppp <- dat$pcexpm*ex.rate

weighted.mean(dat$pcexpppp,dat$hhweight)

dat$daily.consu <- (dat$pcexpppp/30.42)

weighted.mean(dat$daily.consu<2.38,dat$hhweight)
