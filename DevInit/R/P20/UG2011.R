wd <- "D:/Documents/Data/DHS/UGIR60DT"
setwd(wd)
library(foreign)
library(Hmisc)

df <- read.dta("UGIR60FL.dta")

hist(df$v012,main="Histogram of respondent's age from Uganda 2011 DHS",xlab="Age",col="#BA0C2F",prob=TRUE)

by(df$v014,df$v013,describe)
summary(glm(v014~v012,data=df,family="binomial"))

b8 <- c(
  df$b8_01
  ,df$b8_02
  ,df$b8_03
  ,df$b8_04
  ,df$b8_05
  ,df$b8_06
  ,df$b8_07
  ,df$b8_08
  ,df$b8_09
  ,df$b8_10
  ,df$b8_11
  ,df$b8_12
  ,df$b8_13
  ,df$b8_14
  ,df$b8_15
  ,df$b8_16
  ,df$b8_17
  ,df$b8_18
  ,df$b8_19
  ,df$b8_20
)

describe(b8)
hist(b8,main="Histogram of respondent's child's age from Uganda 2011 DHS",xlab="Age",col="#BA0C2F",prob=TRUE)

#Child recode
wd <- "D:/Documents/Data/DHS/UGKR60DT"
setwd(wd)
ch <- read.dta("UGKR60FL.dta")
describe(ch$b8)
hist(ch$b8,main="Histogram of respondent's child's age from Uganda 2011 DHS",xlab="Age",col="#BA0C2F",prob=TRUE)
# Child recode restricts children to age 4 or less, even though main dataset has up to 24?

# Lets turn child section long...
bvars <- df[,134:453]
ch.long <- reshape(bvars)

#Household level
wd <- "D:/Documents/Data/DHS/UGHR60DT"
setwd(wd)
hh <- read.dta("UGHR60FL.dta")
hh <- transform(hh,left.out=hv009-(hv010+hv011))
hh <- transform(hh,de.jure.left.out=hv012-(hv010+hv011))
hh <- transform(hh,de.facto.left.out=hv013-(hv010+hv011))
describe(hh$left.out)
describe(hh$de.jure.left.out)
describe(hh$de.facto.left.out)
hist(hh$left.out)
hist(hh$de.facto.left.out)
sum(hh$left.out)
sum(hh$de.facto.left.out)

sh20 <- c(
  hh$sh20_01
  ,hh$sh20_02
  ,hh$sh20_03
  ,hh$sh20_04
  ,hh$sh20_05
  ,hh$sh20_06
  ,hh$sh20_07
  ,hh$sh20_08
  ,hh$sh20_09
  ,hh$sh20_10
  ,hh$sh20_11
  ,hh$sh20_12
  ,hh$sh20_13
  ,hh$sh20_14
  ,hh$sh20_15
  ,hh$sh20_16
  ,hh$sh20_17
  ,hh$sh20_18
  ,hh$sh20_19
  ,hh$sh20_20
  ,hh$sh20_21
  ,hh$sh20_22
  ,hh$sh20_23
  ,hh$sh20_24
  ,hh$sh20_25
  ,hh$sh20_26
  ,hh$sh20_27
  ,hh$sh20_28
  ,hh$sh20_29
  ,hh$sh20_30
  ,hh$sh20_31
  ,hh$sh20_32
  ,hh$sh20_33
  ,hh$sh20_34
  ,hh$sh20_35
  ,hh$sh20_36
  ,hh$sh20_37
)

#Household member
wd <- "D:/Documents/Data/DHS/UGPR60DT"
setwd(wd)
hhm <- read.dta("UGPR60FL.dta")
recodeAge <- function(x){
  if(x>95){
    return(NA)
  }else{
    return(x)
  }
}
hhm$hv105 <- sapply(hhm$hv105,recodeAge)
hist(hhm$hv105,main="Histogram of household member age from Uganda 2011 DHS",xlab="Age",col="#BA0C2F",prob=TRUE)
describe(hhm$hv105)
#Births
wd <- "D:/Documents/Data/DHS/UGBR60DT"
setwd(wd)
bir <- read.dta("UGBR60FL.dta")