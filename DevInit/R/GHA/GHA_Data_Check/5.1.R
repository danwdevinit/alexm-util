#Data check for:
#5.1. Humanitarian funding channels 2013
#By Alex Miller

#Required packages: rJava, xlsx
#install.packages("xlsx")
#install.packages("rJava")
#Add jvm.dll to your PATH, w/ 64 bit java
library(rJava)
library(xlsx)

#Define chapter folder, pull in final numbers####
chapFolder <- "S:/Projects/Programme resources/Data/GHA calcs and analyses/April 2015/Channels of delivery/"
setwd(chapFolder)
AllDonorsMillions <- read.xlsx("All donors channels of delivery 2009-2013.xlsx"
                       ,sheetName="Summary"
                       ,colIndex = c(1,2,3,4,5,6)
                       ,startRow = 4
                       ,endRow = 10
                       )
AllDonorsPercent <- read.xlsx("All donors channels of delivery 2009-2013.xlsx"
                       ,sheetName="Summary"
                       ,colIndex = c(15,16,17,18,19,20)
                       ,startRow = 4
                       ,endRow = 10
)

GovDonorsMillions <- read.xlsx("All donors channels of delivery 2009-2013.xlsx"
                               ,sheetName="Summary"
                               ,colIndex = c(1,2,3,4,5,6)
                               ,startRow = 14
                               ,endRow = 20
)
GovDonorsPercent <- read.xlsx("All donors channels of delivery 2009-2013.xlsx"
                              ,sheetName="Summary"
                              ,colIndex = c(15,16,17,18,19,20)
                              ,startRow = 14
                              ,endRow = 20
)
