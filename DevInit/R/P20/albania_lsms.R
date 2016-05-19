library(Hmisc)
library(ggplot2)
library(plyr)
library(foreign)
library(data.table)

wd <- "D:/Documents/P20/LSMS 2012_eng/Data_LSMS 2012"
setwd(wd)

df <- read.spss("Modul_1A_householdroster.sav",to.data.frame=TRUE)

hist(df$m1a_q05y,main="Histogram of household member age from Albania 2012 LSMS",xlab="Age",col="#BA0C2F",prob=TRUE)
describe(df$m1a_q05y)

educ.2a <- read.spss("Modul_2A_education.sav",to.data.frame=TRUE)
educ.2b <- read.spss("Modul_2B_education.sav",to.data.frame=TRUE)
educ.2c <- read.spss("Modul_2C_education.sav",to.data.frame=TRUE)

labor.4b <- read.spss("Modul_4B_labor.sav",to.data.frame=TRUE)
labor.4a <- read.spss("Modul_4A_labor.sav",to.data.frame=TRUE)

labor.4b <- join(
  labor.4b
  ,df
  ,by=c("psu","hh","idcode")
  )

labor <- join(
  labor.4a
  ,labor.4b
  ,by=c("psu","hh","idcode")
  )

table(labor$M4A_Q02,labor$m1a_q02,useNA="ifany")
table(labor$M4A_Q03,labor$m1a_q02,useNA="ifany")
table(labor$M4A_Q04,labor$m1a_q02,useNA="ifany")

table(labor$M4B_Q01,labor$m1a_q02,useNA="ifany")
table(labor$M4B_Q04,labor$m1a_q02,useNA="ifany")

table(labor$M4B_Q29,labor$m1a_q02,useNA="ifany")

labor.table <- data.table(labor)
workers.by.cluster <- labor.table[,.(working.men=sum(m1a_q02=="Male" & !is.na(M4B_Q11),na.rm=TRUE),working.women=sum(m1a_q02=="Female" & !is.na(M4B_Q11),na.rm=TRUE)), by=.(psu,hh)]
sum(workers.by.cluster$working.men&workers.by.cluster$working.women,na.rm=TRUE)