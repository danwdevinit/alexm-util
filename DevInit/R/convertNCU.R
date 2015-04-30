path<- "C:/git/digital-platform"
setwd(path)

df <- read.csv("./country-year/domestic-netlending.csv", header = TRUE,sep=",",na.strings="",check.names=FALSE,stringsAsFactors=FALSE)
mult <- read.csv("./reference/current-ncu-to-constant-2013-usd-cy.csv", header = TRUE,sep=",",na.strings="",check.names=FALSE,stringsAsFactors=FALSE)
if("value-ncu" %in% colnames(df)){
  names(df)[names(df)=="value-ncu"] <- "value.ncu"
  df$value <- df$value.ncu
}else{
  df$value.ncu <- df$value 
}

for(i in 1:nrow(df)){
  row = df[i,]
  id = row[1][1,1]
  year = row[2][1,1]
  value = row[11][1,1]
  multiplier = mult[which(mult$id==id),]
  multiplier = multiplier[which(multiplier$year==year),3]
  if(length(multiplier)<=0)
  {
    if(year!=2020){print(paste("No multiplier for:",id,year))}
    df[i,11] = NA
  }
  else
  {
    value = value*multiplier
    df[i,11] = value
  }
}
names(df)[names(df) == "value.ncu"] <- "value-ncu"
write.csv(df,"./country-year/domestic-netlending.csv",row.names=FALSE,na="")