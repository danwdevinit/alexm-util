library(foreign)
setwd("~/R/Work/GDELT/")
afrcountries <- read.csv("africanCountries.csv",header=FALSE)
fourdigitcodes <- data.frame()
data <- read.dta("GDELT.dta")
print(c(max(mtcars[,1])))
d1 <- data.frame()
for(i in 1:nrow(afrcountries))
{
  d1 <- rbind(d1,data[which(data$Actor1CountryCode==afrcountries[i,1]|data$Actor2CountryCode==afrcountries[i,1]),])
  print(afrcountries[i,1])
}
d2 <- d1[which(d1$EventCode=="092"|
                 d1$EventCode=="094"|
                 d1$EventCode=="0243"|
                 d1$EventCode=="1122"|
                 d1$EventCode=="1124"|
                 d1$EventCode=="1413"|
                 d1$EventCode=="1423"|
                 d1$EventCode=="1453"|
                 d1$EventCode=="1821"|
                 d1$EventCode=="1822"|),]
write.csv(d2,"protest_legacy_data.csv")