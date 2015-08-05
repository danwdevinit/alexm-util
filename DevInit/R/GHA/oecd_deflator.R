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
setwd("C:/git/alexm-util/DevInit/R/GHA/")
startYear <- "2000"
endYear <- "2015"

#OECD Func####
OECD <- function(url,concept=FALSE){
  #Separate out data URL components
  dRoot <- "http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/"
  indicator <- strsplit(substr(url,nchar(dRoot)+1,nchar(url)),"/")[[1]][1]
  filter <- substr(url,nchar(dRoot)+1+nchar(indicator),nchar(url))
  #Structure URL
  sRoot <- "http://stats.oecd.org/restsdmx/sdmx.ashx/GetDataStructure/"
  t1sUrl <- paste(sRoot
                  ,indicator
                  ,sep = "")
  #Fetch data
  t1dsdmx <- readSDMX(url)
  t1ssdmx <- readSDMX(t1sUrl)
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

#Current####
currentUrl <- paste0("http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/TABLE2A/10100+10010+71+86+64+62+30+66+35+57+45+93+65+63+61+88+55+85+89+10001+10002+130+142+133+136+139+189+10003+225+236+227+287+228+230+229+231+232+233+234+247+235+274+237+245+271+238+239+240+241+243+244+248+249+251+252+253+255+256+257+258+259+275+260+261+266+276+268+269+270+272+273+218+279+278+280+282+283+285+288+265+289+298+10004+10005+376+377+373+328+329+352+331+388+386+336+338+378+340+342+381+347+349+351+354+358+385+361+364+366+382+383+384+375+387+380+389+10006+425+428+431+434+437+440+443+446+451+454+457+460+463+489+498+10007+10008+725+728+730+732+740+735+738+742+745+748+751+752+753+755+761+764+765+769+789+10009+625+610+611+666+630+612+645+650+613+614+655+635+660+665+640+615+616+617+619+679+689+10011+530+540+543+546+549+552+555+558+561+566+573+576+550+580+589+798+10012+831+832+840+836+859+860+845+850+856+858+861+862+880+866+868+870+872+854+876+889+9998.20005+20001+801+1+2+301+68+3+18+4+5+40+20+21+6+701+742+22+7+820+8+76+9+69+61+50+10+11+12+302+20002+1012+913+914+921+916+953+906+1011+1013+990+918+1311+811+1313+1312+944+901+905+912+988+903+958+976+812+104+951+978+971+959+948+974+967+963+923+964+960+966+928+20018+20006+72+62+30+82+75+546+552+83+70+84+45+77+87+566+732+764+55+576+20007+21600+1601+20003+301+4+5+6+701+12+302+20011+1+2+68+3+18+4+5+40+21+6+22+7+76+9+69+61+50+10+12+20004+1+2+68+3+18+4+5+40+21+6+22+7+76+9+69+61+50+10+12+918.1.206.A/all?startTime=",startYear,"&endTime=",endYear)
current <- OECD(currentUrl)
current <- ddply(current,.(DONOR,obsTime),summarize,obsValue=sum(obsValue,na.rm=TRUE))

#Constant####
constantUrl <- paste0("http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/TABLE2A/10100+10010+71+86+64+62+30+66+35+57+45+93+65+63+61+88+55+85+89+10001+10002+130+142+133+136+139+189+10003+225+236+227+287+228+230+229+231+232+233+234+247+235+274+237+245+271+238+239+240+241+243+244+248+249+251+252+253+255+256+257+258+259+275+260+261+266+276+268+269+270+272+273+218+279+278+280+282+283+285+288+265+289+298+10004+10005+376+377+373+328+329+352+331+388+386+336+338+378+340+342+381+347+349+351+354+358+385+361+364+366+382+383+384+375+387+380+389+10006+425+428+431+434+437+440+443+446+451+454+457+460+463+489+498+10007+10008+725+728+730+732+740+735+738+742+745+748+751+752+753+755+761+764+765+769+789+10009+625+610+611+666+630+612+645+650+613+614+655+635+660+665+640+615+616+617+619+679+689+10011+530+540+543+546+549+552+555+558+561+566+573+576+550+580+589+798+10012+831+832+840+836+859+860+845+850+856+858+861+862+880+866+868+870+872+854+876+889+9998.20005+20001+801+1+2+301+68+3+18+4+5+40+20+21+6+701+742+22+7+820+8+76+9+69+61+50+10+11+12+302+20002+1012+913+914+921+916+953+906+1011+1013+990+918+1311+811+1313+1312+944+901+905+912+988+903+958+976+812+104+951+978+971+959+948+974+967+963+923+964+960+966+928+20018+20006+72+62+30+82+75+546+552+83+70+84+45+77+87+566+732+764+55+576+20007+21600+1601+20003+301+4+5+6+701+12+302+20011+1+2+68+3+18+4+5+40+21+6+22+7+76+9+69+61+50+10+12+20004+1+2+68+3+18+4+5+40+21+6+22+7+76+9+69+61+50+10+12+918.1.206.D/all?startTime=",startYear,"&endTime=",endYear)
constant <- OECD(constantUrl)
constant <- ddply(constant,.(DONOR,obsTime,REFERENCEPERIOD),summarize,obsValue=sum(obsValue,na.rm=TRUE))

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
