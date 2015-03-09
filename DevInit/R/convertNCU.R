path<- "C:/git/digital-platform"
setwd(path)

df <- read.csv("./country-year/domestic-revenue-finance-and-expenditure.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)
mult <- read.csv("./reference/current-ncu-to-constant-2012-usd-cy.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)

for(i in 1:nrow(df)){
  row = df[i,]
  id = row[1][1,1]
  year = row[2][1,1]
  value = row[12][1,1]
  multiplier = mult[which(mult$id==id),]
  multiplier = multiplier[which(multiplier$year==year),4]
  if(length(multiplier)<=0)
  {
    print(paste("No multiplier for:",id,year))
    df[i,12] = NA
  }
  else
  {
    value = value*multiplier
    df[i,12] = value
  }
}

write.csv(df,"./country-year/domestic-revenue-finance-and-expenditure.csv",row.names=FALSE,na="")
