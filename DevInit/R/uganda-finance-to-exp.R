path<- "C:/git/digital-platform/country-year"
setwd(path)

df <- read.csv("./uganda-finance.csv", header = TRUE,sep=",",na.strings="",check.names=FALSE,stringsAsFactors=FALSE)
pop <- read.csv("./uganda-total-pop.csv", header = TRUE,sep=",",na.strings="",check.names=FALSE,stringsAsFactors=FALSE)

idCol <- c()
yearCol <- c()
budgetCol <- c()
spendppCol <- c()
spendppncuCol <- c()

districts <- unique(df$id)
years <- unique(df$year)
budgets <- unique(df[,'budget-type'])
for(i in 1:length(districts)){
  id <- districts[i]
  for(j in 1:length(years)){
    year <- years[j]
    for(k in 1:length(budgets)){
      budget <- budgets[k]
      matches <- df[which(df$id==id & df$year==year & df[,'budget-type']==budget & df$l1=="expenditure"),]
      total <- sum(matches$value)
      totalncu <- sum(matches[,'value-ncu'])
      popul <- pop[which(pop$id==id & pop$year==(year-1)),]$value
      if(length(popul)>0){
        population <- popul[1]
        spendpp <- total/population
        spendppNCU <- totalncu/population
        idCol <- c(idCol,id)
        yearCol <- c(yearCol,(year-1))
        budgetCol <- c(budgetCol,budget)
        spendppCol <- c(spendppCol,spendpp)
        spendppncuCol <- c(spendppncuCol,spendppNCU)
      }
    }
  }
}

data <- data.frame(idCol,
                   yearCol,
                   budgetCol,
                   spendppCol,
                   spendppncuCol)

names(data) <- c("id","year","budget-type","value","value-ncu")
write.csv(data,"uganda-gov-spend-pp.csv",row.names=FALSE,na="")
