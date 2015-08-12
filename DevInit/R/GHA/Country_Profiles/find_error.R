library(rsdmx)
library(plyr)
suppressPackageStartupMessages(library("dplyr"))
library(httpuv)

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

######
codes <- OECDCode("TABLE1")
recip_codes <- subset(codes,variable=="RECIPIENT")
recips <- "71+86+64+62+30+66+35+57+45+93+65+63+61+88+55+85+130+142+133+136+139+225+236+227+287+228+230+229+231+232+233+234+247+235+274+245+271+238+239+240+241+243+244+248+249+251+252+253+255+256+257+258+259+275+260+261+266+276+268+269+270+272+273+218+279+278+280+282+283+285+288+265+376+377+373+328+329+352+331+388+386+336+338+378+340+342+381+347+349+351+354+358+385+361+364+366+382+383+384+375+387+425+428+431+434+437+440+443+446+451+454+457+460+463+725+728+730+732+740+735+738+742+745+748+751+753+755+761+764+765+769+625+610+611+666+630+612+645+613+614+655+635+660+665+640+615+616+617+530+540+543+546+549+552+555+558+561+566+573+576+550+580+831+832+840+836+859+860+845+850+856+858+861+862+880+866+868+870+872+854+876+225+236+287+228+231+232+233+235+274+245+271+238+240+243+244+249+251+252+253+255+256+259+260+266+268+269+272+273+279+278+282+283+285+288+349+728+745+765+625+666+630+635+660+580+836+880+866+872+854+248+279+265+740+614+615+57+93+85+142+136+230+229+234+247+241+261+280+352+342+347+351+364+428+446+451+738+753+755+769+610+612+645+614+665+640+616+617+543+573+550+832+859+860+862+880+868+870+71+86+64+66+65+63+55+130+133+139+227+239+257+275+276+270+218+376+377+352+336+338+378+340+381+354+358+385+366+382+383+384+425+431+434+437+440+454+457+460+463+730+751+764+611+613+655+616+540+543+549+555+831+832+859+845+856+861+870+876+62+30+35+45+61+258+376+373+328+329+331+388+386+361+382+375+387+443+725+732+735+742+748+761+530+546+552+558+561+566+576+840+850+858+88+236+287+228+229+231+232+233+234+247+235+271+238+240+241+243+244+251+252+253+255+256+259+260+266+268+269+272+273+278+282+283+285+288+349+351+364+428+446+625+614+66+93+227+287+228+231+232+238+249+253+255+260+266+279+280+285+288+265+428+451+745+753+625+610+611+630+613+614+660+615+616+617+230+233+244+257+268+270+376+377+328+329+352+338+378+340+381+349+354+385+382+383+384+375+446+457+761+765+655+831+832+836+859+860+845+856+861+862+880+866+870+872+854+64+57+142+133+225+287+228+229+231+232+233+234+247+235+271+238+243+244+248+251+252+253+255+256+260+261+266+272+273+279+278+283+285+265+349+740+765+625+666+635+660+665+640+543+573+550+580+836+859+860+866+872+71+86+93+85+610+611+612+613+614+615+616+617+71+86+93+85+610+611+612+613+614+615+616+617+225+236+227+287+228+230+229+231+232+233+234+247+235+274+245+271+238+239+240+241+243+244+248+249+251+252+253+255+256+257+259+275+260+261+266+268+269+270+272+273+278+280+282+283+285+288+265+377+328+329+352+378+340+381+349+354+382+383+384+375+446+457+832+836+862+880+866+870+872+854+287+230+232+240+244+255+256+260+269.111+112+113+114+121+122+130+140+151+152+160+210+220+230+240+250+311+312+313+321+322+323+331+332+410+430+510+520+530+600+720+730+740+910+930+998"
recips <- strsplit(recips,"+",fixed=TRUE)[[1]]
for(i in 1:length(recips)){
  recip <- recips[i]
  url <- paste0("http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/CRS1/20001.",recip,".100.100.D.112.100/all?startTime=2013&endTime=2013")
  OECD(url)
}
