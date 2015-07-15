wd <- "C:/Users/alexm/Downloads"
path<- "C:/git/digital-platform/country-year"
setwd(wd)

df <- read.csv("./District_rev_indicator010715.csv", header = TRUE,sep=",",na.strings="",check.names=FALSE,stringsAsFactors=FALSE)
idCol <- c()
yearCol <- c()
budgetCol <- c()
igfCol <- c()
centralCol <- c()
donorCol <- c()
totalCol <- c()

districts <- unique(df$id)
years <- unique(df$Year)
budgets <- unique(df[,'Budget-type'])
for(i in 1:length(districts)){
  id <- districts[i]
  for(j in 1:length(years)){
    year <- years[j]
    for(k in 1:length(budgets)){
      budget <- budgets[k]
      matches <- df[which(df$id==id & df$Year==year & df[,'Budget-type']==budget),]
      igf <- matches[which(matches$l2=="Locally Raised Revenues"),]$Sum[1]
      donor <- matches[which(matches$l2=="Donor Funding"),]$Sum[1]
      total <- matches[which(is.na(matches$l2)),]$Sum[1]
      central <- total-(igf+donor)
      idCol <- c(idCol,id)
      yearCol <- c(yearCol,year)
      budgetCol <- c(budgetCol,budget)
      igfCol <- c(igfCol,igf)
      centralCol <- c(centralCol,central)
      donorCol <- c(donorCol,donor)
      totalCol <- c(totalCol,total)
    }
  }
}

data <- data.frame(idCol,
                   yearCol,
                   budgetCol,
                   igfCol,
                   centralCol,
                   donorCol,
                   totalCol)

names(data) <- c("id","year","budget","igf","central","donor","total")
write.csv(data,"district-igf-donor.csv",row.names=FALSE,na="")
