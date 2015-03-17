path<- "C:/git/digital-platform"
setwd(path)

df <- read.csv("./country-year/domestic-sectors.csv", header = TRUE,sep=",",na.strings="",check.names=FALSE,stringsAsFactors=FALSE)
mult <- read.csv("./reference/current-ncu-to-constant-2012-usd-cy.csv", header = TRUE,sep=",",na.strings="",check.names=FALSE,stringsAsFactors=FALSE)

for(i in 1:nrow(df)){
  row = df[i,]
  id = row[1][1,1]
  year = row[2][1,1]
  value = row[9][1,1]
  multiplier = mult[which(mult$id==id),]
  multiplier = multiplier[which(multiplier$year==year),4]
  if(length(multiplier)<=0)
  {
    print(paste("No multiplier for:",id,year))
    df[i,9] = NA
  }
  else
  {
    value = value*multiplier
    df[i,9] = value
  }
}

write.csv(df,"./country-year/domestic-sectors.csv",row.names=FALSE,na="")