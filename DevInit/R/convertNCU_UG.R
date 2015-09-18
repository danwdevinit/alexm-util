path<- "C:/git/digital-platform"
setwd(path)

df <- read.csv("./country-year/uganda-finance.csv", header = TRUE,sep=",",na.strings="",check.names=FALSE,stringsAsFactors=FALSE)
mult <- read.csv("./reference/current-ncu-to-constant-2012-usd-cy.csv", header = TRUE,sep=",",na.strings="",check.names=FALSE,stringsAsFactors=FALSE)
if("value-ncu" %in% colnames(df)){
  names(df)[names(df)=="value-ncu"] <- "value.ncu"
  df$value.ncu <- as.double(df$value.ncu)
  df$value <- df$value.ncu
}else{
  df$value <- as.double(df$value)
  df$value.ncu <- df$value 
}

for(i in 1:nrow(df)){
  row = df[i,]
  id = "UG"
  year = row[2][1,1]
  value = row[8][1,1]
  multiplier = mult[which(mult$id==id),]
  multiplier = multiplier[which(multiplier$year==year),3]
  if(length(multiplier)<=0)
  {
    if(year!=2020){print(paste("No multiplier for:",id,year))}
    df[i,8] = NA
  }
  else
  {
    value = value*multiplier
    df[i,8] = value
  }
}
names(df)[names(df) == "value.ncu"] <- "value-ncu"
write.csv(df,"./country-year/uganda-finance.csv",row.names=FALSE,na="")