path <- "C:/Users/alexm/Downloads/CRS 2013 data/CRS 2013 data.txt"

dat <- read.table(
  path
  ,header = TRUE
  ,sep = "|"
  ,quote = "\""
  ,dec = "."
  ,fill = TRUE
  ,comment.char = ""
  ,fileEncoding="UTF-16"
  )


data <- dat[which(dat$flowname=="ODA Grants" | dat$flowname=="ODA Loans"),]
library(plyr)
pivot <- ddply(dat, .(donorname,purposecode),summarize,usd_sum=sum(usd_disbursement_defl))
pivot <- pivot[which(pivot$purposecode==998),]
write.csv(pivot,"pivot.csv",na="",row.names=FALSE)
getwd()
