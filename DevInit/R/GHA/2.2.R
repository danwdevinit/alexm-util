#Vis check for:
#2.2
#By Alex Miller

library(rJava)
library(xlsx)
library(rCharts)

path <- "//DIPR-DC01/data/Company Data/Projects/GHA/Phase II/Products/Reports/GHA-Report/2015/Chapters - 1st draft/Chapter 2/Charts to Diane/Fig 2.2 Funding and unmet needs, UN-coordinated appeals, 2005-2014.xlsx"
dat <- read.xlsx(path
                 ,1
                 ,rowIndex=8:11
                 ,colIndex=2:12
                 )
names(dat)[1] <- "dim"
dat <- reshape(dat,direction="long"
        ,varying=list(names(dat)[2:11])
        ,v.names="value"
        ,idvar=c("dim")
        ,timevar="year"
        ,times = 2005:2014)
