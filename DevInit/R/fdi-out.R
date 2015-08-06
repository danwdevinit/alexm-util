#install.packages("plyr")
library(plyr)

wd <- "C:/Users/alexm/AppData/Roaming/Skype/My Skype Received Files"
setwd(wd)

fdi <- read.csv("fdi-out.csv",as.is=TRUE,na.strings="")

wd <- "C:/git/digital-platform/country-year"
setwd(wd)

intlFlows <- read.csv("intl-flows-recipients.csv",as.is=TRUE,na.strings="",check.names=FALSE)

for(i in 1:nrow(fdi)){
  row <- fdi[i,]
  intlFlows[which(
    intlFlows[,1]==row[1,1] 
    & intlFlows[,2]==row[1,2]
    & intlFlows[,5]=="fdi-out"
  ),6] <- row$value[1]
}

write.csv(intlFlows,"intl-flows-recipients.csv",na="",row.names=FALSE)
