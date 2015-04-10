wd <- "C:/git/digital-platform/country-year/"
path <- "C:/git/alexm-util/DevInit/R/NHA Indicators.csv"
setwd(wd)
dat <- read.csv(path, header = TRUE,na.strings="",check.names=FALSE,stringsAsFactors=FALSE)
names(dat)[names(dat) == "iso2"] <- "id"
keep <- c("id","year","value","Indicators")
dat <- dat[keep]

#Convert
id <- c()
year <- c()
dat["value.ncu"] <- dat$value
mult <- read.csv("C:/git/digital-platform/reference/current-ncu-to-constant-2012-usd-cy.csv", header = TRUE,sep=",",na.strings="",check.names=FALSE,stringsAsFactors=FALSE)
for(i in 1:nrow(dat)){
  multiplier <- mult[which(
    mult$id==dat$id[i] &
      mult$year==dat$year[i]
    ),]
  if(nrow(multiplier)<=0)
  {
    print(paste("No multiplier for:",dat$id[i],dat$year[i]))
    dat$value[i] <- NA
    id <- c(id,dat$id[i])
    year <- c(year,dat$year[i])
  }
  else
  {
    dat$value[i] <- dat$value[i]*multiplier$value[1]
  }
}
missingConvert <- data.frame(id,year)
missingConvert <- unique(missingConvert)

genGov <- dat[which(dat$Indicators=="General government expenditure on health"),]
keep <- c("id","year","value","value.ncu")
genGov <- genGov[keep]

SS <- dat[which(dat$Indicators=="Social security funds"),]
SS <- SS[keep]
SS[is.na(SS)] <- 0

datWide <- merge(genGov
                 ,SS
                 ,suffixes=c(".genGov",".SS")
                 ,by=c("id","year")
                 ,all=TRUE)

datWide$value <- datWide$value.genGov - datWide$value.SS
datWide$value.ncu <- datWide$value.ncu.genGov - datWide$value.ncu.SS
genGovLessSS <- data.frame(datWide$id,datWide$year,datWide$value,datWide$value.ncu,stringsAsFactors=FALSE)
names(genGovLessSS)[names(genGovLessSS) == "datWide.id"] <- "id"
names(genGovLessSS)[names(genGovLessSS) == "datWide.year"] <- "year"
names(genGovLessSS)[names(genGovLessSS) == "datWide.value"] <- "value"
names(genGovLessSS)[names(genGovLessSS) == "datWide.value.ncu"] <- "value-ncu"


write.csv(genGovLessSS,"general-gov-health-exp.csv",na="",row.names=FALSE)
