#install.packages("rsdmx")
#install.packages("plyr")
#install.packages("dplyr")
#install.packages("httpuv")
#install.packages('WDI')
library(WDI)
library(rsdmx)
library(plyr)
suppressPackageStartupMessages(library("dplyr"))
library(httpuv)

#Configuration
setwd("C:/git/alexm-util/DevInit/R/GHA/Country_Profiles")
startYear <- "2000"
endYear <- "2014"

#OECD Developer API Function####
OECD <- function(url){
  #Fetch data
  t1dsdmx <- readSDMX(url)
  #Convert to DF
  t1 <- as.data.frame(t1dsdmx)
  #get codelists
  cls <- t1ssdmx@codelists
  codelists <- sapply(cls@codelists, function(x) x@id)
  #Recode
  for(i in 1:length(codelists)){
    suffix <- paste("CL_",indicator,"_",sep="")
    clName <- substr(codelists[i],nchar(suffix)+1,nchar(codelists[i]))
    codelist <- cls@codelists[i][[1]]@Code
    for(j in 1:length(codelist)){
      id <- codelist[j][[1]]@id
      name <- codelist[j][[1]]@label$en
      if(clName %in% colnames(t1)){
        t1[clName][which(t1[clName]==id),] <- name
      }
    }
  }
  #get concepts
  concepts <- as.data.frame(t1ssdmx@concepts)
  if(concept){
    return(concepts)
  }else{
    return(t1)
  }
}

#OECD Codelist Func####
OECDCode <- function(indicator){
  #Structure URL
  sRoot <- "http://stats.oecd.org/restsdmx/sdmx.ashx/GetDataStructure/"
  t1sUrl <- paste0(sRoot
                   ,indicator
  )
  #Fetch data
  t1ssdmx <- readSDMX(t1sUrl)
  #get codelists
  cls <- t1ssdmx@codelists
  codelists <- sapply(cls@codelists, function(x) x@id)
  #Flatten
  variable <- c()
  id <- c()
  name <- c()
  for(i in 1:length(codelists)){
    suffix <- paste("CL_",indicator,"_",sep="")
    clName <- substr(codelists[i],nchar(suffix)+1,nchar(codelists[i]))
    codelist <- cls@codelists[i][[1]]@Code
    for(j in 1:length(codelist)){
      ID <- codelist[j][[1]]@id
      NAME <- codelist[j][[1]]@label$en
      id <- c(id,ID)
      name <- c(name,NAME)
      variable <- c(variable,clName)
    }
    df <- data.frame(variable,id,name)
  }
  #return
  return(df)
}

#Data download####
dataUrl <- paste0("http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/TABLE1/20005+20001+801+1+2+301+68+3+18+4+5+40+20+21+6+701+742+22+7+820+8+76+9+69+61+50+10+11+12+302+20002+918+20006+72+62+30+82+75+546+613+552+83+70+84+45+77+87+566+732+764+55+576+20007+20003+301+4+5+6+701+12+302+20011+1+2+68+3+18+4+5+40+21+6+22+7+76+9+69+61+50+10+12+20004+1+2+68+3+18+4+5+40+21+6+22+7+76+9+69+61+50+10+12+918.1.5+1010+1015+1100+1110+1120+1200+1210+1211+1212+1213+1214+1220+1230+1300+1310+1311+1320+1330+1301+1400+1410+1420+1500+1510+1520+1600+1610+1611+1612+1613+1614+1620+1621+1622+1623+1624+1630+1640+1700+1800+1810+1820+1900+1999+99999+1901+1902+1903+1904+1905+1906+60+70+2000+2100+2101+2102+2103+2104+547+2105+2106+2107+2108+2110+2901+2902+230+235+240+265+266+294+291+292+293+280+287+300+301+302+310+303+295+299+298+102+325+326+327+795+800+805+786+330+332+340+345+353+384+751+752+753+386+756+761+388+389+103+359+415+425+420+207+1+2+3+4.1121+1122+1120+1130+1140+1151+1152+1150.A+D+N/all?startTime=",startYear,"&endTime=",endYear)
data <- OECD(dataUrl)

#Split by data type####
ncu <- subset(data,)

#Implied deflator####
deflator <- merge(current
                  ,constant
                  ,by=c("DONOR","obsTime")
)
deflator <- transform(deflator,obsValue=(obsValue.y/obsValue.x))
keep <- c("DONOR","obsTime","obsValue","REFERENCEPERIOD")
deflator <- deflator[keep]

#Fill in the blanks with WDI
wbcurrent <- WDI(country = "all", 
                 indicator = "NY.GDP.MKTP.CD", 
                 start = as.integer(startYear), 
                 end = as.integer(endYear),
                 extra = TRUE
)
wbconstant <- WDI(country = "all", 
                  indicator = "NY.GDP.MKTP.KD", 
                  start = as.integer(startYear), 
                  end = as.integer(endYear),
                  extra = TRUE
)
#Implied deflator####
wbdeflator <- merge(wbcurrent
                    ,wbconstant
                    ,by=intersect(names(wbcurrent),names(wbconstant))
)
wbdeflator <- transform(wbdeflator,base2005=(NY.GDP.MKTP.KD/NY.GDP.MKTP.CD))
keep <- c("iso2c","iso3c","country","year","base2005")
wbdeflator <- wbdeflator[keep]
refyear <- deflator$REFERENCEPERIOD[[1]]
wbdeflator <- ddply(wbdeflator,.(country),function(x){
  x$baseref <- x$base2005 / subset(x,year==refyear)$base2005
  return(x)
})
countries <- read.csv("countries.csv",na.strings="",as.is=TRUE)
wbdeflator <- merge(
  wbdeflator
  ,countries
  ,by.x="iso3c"
  ,by.y="iso3")
codes <- subset(OECDCode("TABLE2A"),variable=="RECIPIENT" | variable=="DONOR")
wbdeflator <- merge(
  wbdeflator
  ,codes
  ,by.x="oecd_code"
  ,by.y="id")
keep <- c("name.y","year","baseref")
wbdeflator <- wbdeflator[keep]
names(wbdeflator)<-c("DONOR","obsTime","obsValue")
wbdeflator$REFERENCEPERIOD <- refyear
deflator <- merge(
  deflator
  ,wbdeflator
  ,by=c("DONOR","obsTime","REFERENCEPERIOD")
  ,all = TRUE
)
secondIfNotFirst <- function(firsts,seconds){
  c <- c()
  for(i in 1:length(firsts)){
    first <- firsts[i]
    second <- seconds[i]
    if(!is.na(first)){c<-c(c,first)}
    else{c<-c(c,second)}
  }
  return(c)
}
deflator <- transform(deflator,obsValue=secondIfNotFirst(obsValue.x,obsValue.y))
keep <- c("DONOR","obsTime","REFERENCEPERIOD","obsValue")
deflator <- deflator[keep]
deflator <- ddply(deflator,.(DONOR,obsTime,REFERENCEPERIOD),summarize,obsValue=obsValue[1])
write.csv(deflator,"oecd_deflator.csv",na="",row.names=FALSE)
