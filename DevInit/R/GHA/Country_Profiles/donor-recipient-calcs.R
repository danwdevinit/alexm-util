#install.packages("rsdmx")
#install.packages("plyr")
#install.packages("dplyr")
#install.packages("httpuv")
#install.packages("googlesheets")
#install.packages("rvest")
library(rsdmx)
library(plyr)
suppressPackageStartupMessages(library("dplyr"))
library(httpuv)
library(googlesheets)
library(rvest)

#Configuration
setwd("C:/git/alexm-util/DevInit/R/GHA/")
startYear <- "2000"
endYear <- "2015"
shares <- list(
  WFP = 0.913364375831843
  ,UNICEF = 0.246929100907661
  ,UNHCR = 1
  ,UNRWA = 1
)
  
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

message("Downloading tables")
#Donor Bilateral HA####
haUrl <- paste0("http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/TABLE2A/10200+10100+10010+71+86+64+62+30+66+35+57+45+93+65+63+61+88+55+85+89+10001+10002+130+142+133+136+139+189+10003+225+236+227+287+228+230+229+231+232+233+234+247+235+274+237+245+271+238+239+240+241+243+244+248+249+251+252+253+255+256+257+258+259+275+260+261+266+276+268+269+270+272+273+218+279+278+280+282+283+285+288+265+289+298+10004+10005+376+377+373+328+329+352+331+388+386+336+338+378+340+342+381+347+349+351+354+358+385+361+364+366+382+383+384+375+387+380+389+10006+425+428+431+434+437+440+443+446+451+454+457+460+463+489+498+10007+10008+725+728+730+732+740+735+738+742+745+748+751+752+753+755+761+764+765+769+789+10009+625+610+611+666+630+612+645+650+613+614+655+635+660+665+640+615+616+617+619+679+689+10011+530+540+543+546+549+552+555+558+561+566+573+576+550+580+589+798+10012+831+832+840+836+859+860+845+850+856+858+861+862+880+866+868+870+872+854+876+889+9998+10016+225+236+287+228+231+232+233+235+274+245+271+238+240+243+244+249+251+252+253+255+256+259+260+266+268+269+272+273+279+278+282+283+285+288+349+728+745+765+625+666+630+635+660+580+836+880+866+872+854+10017+248+279+265+740+614+615+10018+57+93+85+142+136+230+229+234+247+241+261+280+352+342+347+351+364+428+446+451+738+753+755+769+610+612+645+614+665+640+616+617+543+573+550+832+859+860+862+880+868+870+10019+71+86+64+66+65+63+55+130+133+139+227+239+257+275+276+270+218+376+377+352+336+338+378+340+381+354+358+385+366+382+383+384+425+431+434+437+440+454+457+460+463+730+751+764+611+613+655+616+540+543+549+555+831+832+859+845+856+861+870+876+10025+62+30+35+45+61+258+376+373+328+329+331+388+386+361+382+375+387+443+725+732+735+742+748+761+530+546+552+558+561+566+576+840+850+858+105+10024+88+89+189+237+289+298+380+389+489+498+752+789+650+619+679+689+589+798+889+9998+10030+236+287+228+229+231+232+233+234+247+235+271+238+240+241+243+244+251+252+253+255+256+259+260+266+268+269+272+273+278+282+283+285+288+349+351+364+428+446+625+614+10201+66+93+227+287+228+231+232+238+249+253+255+260+266+279+280+285+288+265+428+451+745+753+625+610+611+630+613+614+660+615+616+617+10202+230+233+244+257+268+270+376+377+328+329+352+338+378+340+381+349+354+385+382+383+384+375+446+457+761+765+655+831+832+836+859+860+845+856+861+862+880+866+870+872+854+10203+64+57+142+133+225+287+228+229+231+232+233+234+247+235+271+238+243+244+248+251+252+253+255+256+260+261+266+272+273+279+278+283+285+265+349+740+765+625+666+635+660+665+640+543+573+550+580+836+859+860+866+872+10150+913+914+916+915+910+906+917+918+919+901+905+904+909+912+988+903+907+902+927+989+816+975+900+959+974+967+963+964+966+10013+71+72+68+82+75+83+84+76+77+69+101+10014+86+93+85+610+611+612+613+614+615+616+617+87+102+10023+71+86+93+85+610+611+612+613+614+615+616+617+72+68+82+75+83+84+76+77+69+101+87+102+79+10040+225+236+227+287+228+230+229+231+232+233+234+247+235+274+245+271+238+239+240+241+243+244+248+249+251+252+253+255+256+257+259+275+260+261+266+268+269+270+272+273+278+280+282+283+285+288+265+377+328+329+352+378+340+381+349+354+382+383+384+375+446+457+832+836+862+880+866+870+872+854+10041+287+230+232+240+244+255+256+260+269+10152+996+990+878+98+1106+10026+10027.20005+20001+801+1+2+301+68+3+18+4+5+40+20+21+6+701+742+22+7+820+8+76+9+69+61+50+10+11+12+302+20002+1012+913+914+921+916+953+906+1011+1013+990+918+1311+811+1313+1312+944+901+905+912+988+903+958+976+812+104+951+978+971+959+948+974+967+963+923+964+960+966+928+20018+20006+72+62+30+82+75+546+552+83+70+84+45+77+87+566+732+764+55+576+20007+21600+1601+20003+301+4+5+6+701+12+302+20011+1+2+68+3+18+4+5+40+21+6+22+7+76+9+69+61+50+10+12+20004+1+2+68+3+18+4+5+40+21+6+22+7+76+9+69+61+50+10+12+918.1.216.D/all?startTime=",startYear,"&endTime=",endYear)
ha <- OECD(haUrl)
ha <- subset(ha, !grepl(", Total",DONOR))
#ha <- subset(ha, !(DONOR %in% c("UNHRC","UNRWA","UNICEF","WFP"))) Might need this?
ha <- subset(ha, !grepl(", Total",RECIPIENT))

#UN agencies as a donor####
unUrl <- paste0("http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/TABLE2A/10200+10100+10010+71+86+64+62+30+66+35+57+45+93+65+63+61+88+55+85+89+10001+10002+130+142+133+136+139+189+10003+225+236+227+287+228+230+229+231+232+233+234+247+235+274+237+245+271+238+239+240+241+243+244+248+249+251+252+253+255+256+257+258+259+275+260+261+266+276+268+269+270+272+273+218+279+278+280+282+283+285+288+265+289+298+10004+10005+376+377+373+328+329+352+331+388+386+336+338+378+340+342+381+347+349+351+354+358+385+361+364+366+382+383+384+375+387+380+389+10006+425+428+431+434+437+440+443+446+451+454+457+460+463+489+498+10007+10008+725+728+730+732+740+735+738+742+745+748+751+752+753+755+761+764+765+769+789+10009+625+610+611+666+630+612+645+650+613+614+655+635+660+665+640+615+616+617+619+679+689+10011+530+540+543+546+549+552+555+558+561+566+573+576+550+580+589+798+10012+831+832+840+836+859+860+845+850+856+858+861+862+880+866+868+870+872+854+876+889+9998+10016+225+236+287+228+231+232+233+235+274+245+271+238+240+243+244+249+251+252+253+255+256+259+260+266+268+269+272+273+279+278+282+283+285+288+349+728+745+765+625+666+630+635+660+580+836+880+866+872+854+10017+248+279+265+740+614+615+10018+57+93+85+142+136+230+229+234+247+241+261+280+352+342+347+351+364+428+446+451+738+753+755+769+610+612+645+614+665+640+616+617+543+573+550+832+859+860+862+880+868+870+10019+71+86+64+66+65+63+55+130+133+139+227+239+257+275+276+270+218+376+377+352+336+338+378+340+381+354+358+385+366+382+383+384+425+431+434+437+440+454+457+460+463+730+751+764+611+613+655+616+540+543+549+555+831+832+859+845+856+861+870+876+10025+62+30+35+45+61+258+376+373+328+329+331+388+386+361+382+375+387+443+725+732+735+742+748+761+530+546+552+558+561+566+576+840+850+858+105+10024+88+89+189+237+289+298+380+389+489+498+752+789+650+619+679+689+589+798+889+9998+10030+236+287+228+229+231+232+233+234+247+235+271+238+240+241+243+244+251+252+253+255+256+259+260+266+268+269+272+273+278+282+283+285+288+349+351+364+428+446+625+614+10201+66+93+227+287+228+231+232+238+249+253+255+260+266+279+280+285+288+265+428+451+745+753+625+610+611+630+613+614+660+615+616+617+10202+230+233+244+257+268+270+376+377+328+329+352+338+378+340+381+349+354+385+382+383+384+375+446+457+761+765+655+831+832+836+859+860+845+856+861+862+880+866+870+872+854+10203+64+57+142+133+225+287+228+229+231+232+233+234+247+235+271+238+243+244+248+251+252+253+255+256+260+261+266+272+273+279+278+283+285+265+349+740+765+625+666+635+660+665+640+543+573+550+580+836+859+860+866+872+10150+913+914+916+915+910+906+917+918+919+901+905+904+909+912+988+903+907+902+927+989+816+975+900+959+974+967+963+964+966+10013+71+72+68+82+75+83+84+76+77+69+101+10014+86+93+85+610+611+612+613+614+615+616+617+87+102+10023+71+86+93+85+610+611+612+613+614+615+616+617+72+68+82+75+83+84+76+77+69+101+87+102+79+10040+225+236+227+287+228+230+229+231+232+233+234+247+235+274+245+271+238+239+240+241+243+244+248+249+251+252+253+255+256+257+259+275+260+261+266+268+269+270+272+273+278+280+282+283+285+288+265+377+328+329+352+378+340+381+349+354+382+383+384+375+446+457+832+836+862+880+866+870+872+854+10041+287+230+232+240+244+255+256+260+269+10152+996+990+878+98+1106+10026+10027.967+963+964+966.1.206.D/all?startTime=",startYear,"&endTime=",endYear)
un <- OECD(unUrl)
un <- subset(un, !grepl(", Total",RECIPIENT))

#Donor contributions to UN agencies####
tounUrl <- paste0("http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/TABLE2A/967+963+964+966.20005+20001+801+1+2+301+68+3+18+4+5+40+20+21+6+701+742+22+7+820+8+76+9+69+61+50+10+11+12+302+20002+1012+913+914+921+916+953+906+1011+1013+990+918+1311+811+1313+1312+944+901+905+912+988+903+958+976+812+104+951+978+971+959+948+974+967+963+923+964+960+966+928+20018+20006+72+62+30+82+75+546+552+83+70+84+45+77+87+566+732+764+55+576+20007+21600+1601+20003+301+4+5+6+701+12+302+20011+1+2+68+3+18+4+5+40+21+6+22+7+76+9+69+61+50+10+12+20004+1+2+68+3+18+4+5+40+21+6+22+7+76+9+69+61+50+10+12+918.1.206.D/all?startTime=",startYear,"&endTime=",endYear)
toun <- OECD(tounUrl)
toun <- subset(toun, !grepl(", Total",DONOR))

#Donor EU contributions####
toeuUrl <- paste0("http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/TABLE1/20005+20001+801+1+2+301+68+3+18+4+5+40+20+21+6+701+742+22+7+820+8+76+9+69+61+50+10+11+12+302+20002+918+20006+72+62+30+82+75+546+552+83+70+84+45+77+87+566+732+764+55+576+20007+20003+301+4+5+6+701+12+302+20011+1+2+68+3+18+4+5+40+21+6+22+7+76+9+69+61+50+10+12+20004+1+2+68+3+18+4+5+40+21+6+22+7+76+9+69+61+50+10+12+918.1.2102.1140.D/all?startTime=",startYear,"&endTime=",endYear)
toeu <- OECD(toeuUrl)
toeu <- subset(toeu, !grepl(", Total",DONOR))

#Note, fromeu is contained within ha since I didn't restrict it.
fromeuUrl <- paste0("http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/TABLE2A/10200+10100+10010+71+86+64+62+30+66+35+57+45+93+65+63+61+88+55+85+89+10001+10002+130+142+133+136+139+189+10003+225+236+227+287+228+230+229+231+232+233+234+247+235+274+237+245+271+238+239+240+241+243+244+248+249+251+252+253+255+256+257+258+259+275+260+261+266+276+268+269+270+272+273+218+279+278+280+282+283+285+288+265+289+298+10004+10005+376+377+373+328+329+352+331+388+386+336+338+378+340+342+381+347+349+351+354+358+385+361+364+366+382+383+384+375+387+380+389+10006+425+428+431+434+437+440+443+446+451+454+457+460+463+489+498+10007+10008+725+728+730+732+740+735+738+742+745+748+751+752+753+755+761+764+765+769+789+10009+625+610+611+666+630+612+645+650+613+614+655+635+660+665+640+615+616+617+619+679+689+10011+530+540+543+546+549+552+555+558+561+566+573+576+550+580+589+798+10012+831+832+840+836+859+860+845+850+856+858+861+862+880+866+868+870+872+854+876+889+9998+10016+225+236+287+228+231+232+233+235+274+245+271+238+240+243+244+249+251+252+253+255+256+259+260+266+268+269+272+273+279+278+282+283+285+288+349+728+745+765+625+666+630+635+660+580+836+880+866+872+854+10017+248+279+265+740+614+615+10018+57+93+85+142+136+230+229+234+247+241+261+280+352+342+347+351+364+428+446+451+738+753+755+769+610+612+645+614+665+640+616+617+543+573+550+832+859+860+862+880+868+870+10019+71+86+64+66+65+63+55+130+133+139+227+239+257+275+276+270+218+376+377+352+336+338+378+340+381+354+358+385+366+382+383+384+425+431+434+437+440+454+457+460+463+730+751+764+611+613+655+616+540+543+549+555+831+832+859+845+856+861+870+876+10025+62+30+35+45+61+258+376+373+328+329+331+388+386+361+382+375+387+443+725+732+735+742+748+761+530+546+552+558+561+566+576+840+850+858+105+10024+88+89+189+237+289+298+380+389+489+498+752+789+650+619+679+689+589+798+889+9998+10030+236+287+228+229+231+232+233+234+247+235+271+238+240+241+243+244+251+252+253+255+256+259+260+266+268+269+272+273+278+282+283+285+288+349+351+364+428+446+625+614+10201+66+93+227+287+228+231+232+238+249+253+255+260+266+279+280+285+288+265+428+451+745+753+625+610+611+630+613+614+660+615+616+617+10202+230+233+244+257+268+270+376+377+328+329+352+338+378+340+381+349+354+385+382+383+384+375+446+457+761+765+655+831+832+836+859+860+845+856+861+862+880+866+870+872+854+10203+64+57+142+133+225+287+228+229+231+232+233+234+247+235+271+238+243+244+248+251+252+253+255+256+260+261+266+272+273+279+278+283+285+265+349+740+765+625+666+635+660+665+640+543+573+550+580+836+859+860+866+872+10150+913+914+916+915+910+906+917+918+919+901+905+904+909+912+988+903+907+902+927+989+816+975+900+959+974+967+963+964+966+10013+71+72+68+82+75+83+84+76+77+69+101+10014+86+93+85+610+611+612+613+614+615+616+617+87+102+10023+71+86+93+85+610+611+612+613+614+615+616+617+72+68+82+75+83+84+76+77+69+101+87+102+79+10040+225+236+227+287+228+230+229+231+232+233+234+247+235+274+245+271+238+239+240+241+243+244+248+249+251+252+253+255+256+257+259+275+260+261+266+268+269+270+272+273+278+280+282+283+285+288+265+377+328+329+352+378+340+381+349+354+382+383+384+375+446+457+832+836+862+880+866+870+872+854+10041+287+230+232+240+244+255+256+260+269+10152+996+990+878+98+1106+10026+10027.918.1.216.D/all?startTime=",startYear,"&endTime=",endYear)
fromeu <- OECD(fromeuUrl)
fromeu <- subset(fromeu, !grepl(", Total",RECIPIENT))

#To UN Imputed####
message("Calculating UN imputed")
cDONOR <- c()
cRECIPIENT <- c()
cAGENCY <- c()
cobsTime <- c()
cobsValue <- c()
years <- unique(toun$obsTime)
donors <- unique(toun$DONOR)
agencies <- unique(toun$RECIPIENT)
for(i in 1:length(years)){
  year <- years[i]
  for(j in 1:length(agencies)){
    agency <- agencies[j]
    share <- shares[[agency]]
    if(is.null(share)){share <- 1}
    totalUN <- sum(subset(toun, (RECIPIENT==agency & obsTime==year))$obsValue,na.rm=TRUE)
    for(k in 1:length(donors)){
      donor <- donors[k]
      toUN <- subset(toun, (DONOR==donor & RECIPIENT==agency & obsTime==year))$obsValue
      if(length(toUN)<1){toUN<-0}
      toUNpercent <- toUN / totalUN
      fromUNset <- subset(un, (DONOR==agency & obsTime==year))
      recipients <- unique(fromUNset$RECIPIENT)
      for(m in 1:length(recipients)){
        recipient <- recipients[m]
        fromUN <- subset(fromUNset, RECIPIENT==recipient)$obsValue
        UNimputed <- toUNpercent * fromUN * share
        cDONOR <- c(cDONOR,donor)
        cRECIPIENT <- c(cRECIPIENT,recipient)
        cAGENCY <- c(cAGENCY, agency)
        cobsTime <- c(cobsTime, year)
        cobsValue <- c(cobsValue, UNimputed)
      }
    }
  }
}
unFunding <- data.frame(cDONOR,cRECIPIENT,cAGENCY,cobsTime,cobsValue)
names(unFunding) <- c("DONOR","RECIPIENT","AGENCY","obsTime","obsValue")

#All UN funding calculation####
allunfunding <- ddply(unFunding,.(DONOR,RECIPIENT,obsTime),summarize,obsValue=sum(obsValue,na.rm=TRUE))

#Donor spending via EU####
message("Donor spending via EU")
cDONOR <- c()
cRECIPIENT <- c()
cobsTime <- c()
cobsValue <- c()
years <- unique(toeu$obsTime)
donors <- unique(toeu$DAC_DONOR)
for(i in 1:length(years)){
  year <- years[i]
  totalEU <- sum(subset(toeu, (obsTime==year))$obsValue,na.rm=TRUE)
  for(k in 1:length(donors)){
    donor <- donors[k]
    toEU <- subset(toeu, (DAC_DONOR==donor & obsTime==year))$obsValue
    if(length(toEU)<1){toEU<-0}
    toEUpercent <- toEU / totalEU
    fromEUset <- subset(fromeu, (obsTime==year))
    recipients <- unique(fromEUset$RECIPIENT)
    if(length(recipients)>0){
      for(m in 1:length(recipients)){
        recipient <- recipients[m]
        fromEU <- subset(fromEUset, RECIPIENT==recipient)$obsValue
        EUimputed <- toEUpercent * fromEU
        cDONOR <- c(cDONOR,donor)
        recipLen1 <- length(cRECIPIENT)
        cRECIPIENT <- c(cRECIPIENT,recipient)
        recipLen2 <- length(cRECIPIENT)
        if(recipLen1==recipLen2){sample <- recipient; sample2 <- m;sample3 <- recipients}
        cobsTime <- c(cobsTime, year)
        cobsValue <- c(cobsValue, EUimputed)
      }
    }
  }
}
eufunding <- data.frame(cDONOR,cRECIPIENT,cobsTime,cobsValue)
names(eufunding) <- c("DONOR","RECIPIENT","obsTime","obsValue")

#Donor spending via the CERF####
message("Donor spending via the CERF")
if(as.integer(startYear)>=2006){cerfStart <- as.integer(startYear)}else{cerfStart <- 2006}
CERFyears <- c(cerfStart:as.integer(endYear))
fromUrl <- "https://cerf.unocha.org/admin/Webservices/SummaryFundingadv.aspx?type=country&year="
toUrl <- "http://www.unocha.org/cerf/our-donors/funding/pledges-and-contributions/"

if(exists("fromCERF")){rm(fromCERF)}
if(exists("toCERF")){rm(toCERF)}

for(i in 1:length(CERFyears)){
  year <- CERFyears[i]
  #From
  page <- html(paste0(fromUrl,year))
  fromCERFtmp <- page %>%
    html_node("table") %>%
    html_table(fill=TRUE)
  fromCERFtmp <- fromCERFtmp[,c(2,3)]
  fromCERFtmp$obsTime <- year
  fromCERFtmp$DONOR <- "Central Emergency Response Fund"
  names(fromCERFtmp) <- c("RECIPIENT","obsValue","obsTime","DONOR")
  fromCERFtmp$obsValue <- as.numeric(gsub(",","", fromCERFtmp$obsValue))
  fromCERFtmp <- subset(fromCERFtmp,!is.na(RECIPIENT))
  fromCERFtmp <- subset(fromCERFtmp, !grepl("Country",RECIPIENT))
  fromCERFtmp <- subset(fromCERFtmp, !grepl("Total",RECIPIENT))
  fromCERFtmp <- subset(fromCERFtmp, !grepl(as.character(year),RECIPIENT))
  if(exists("fromCERF")){fromCERF <- rbind(fromCERF,fromCERFtmp)}else{fromCERF <- fromCERFtmp}
  rm(page)
  #To
  page <- html(paste0(toUrl,year))
  toCERFtmp <- page %>%
    html_node("table") %>%
    html_table(fill=TRUE)
  if("Donors" %in% names(toCERFtmp)){
    toCERFtmp <- toCERFtmp[,c(2,3)]
    toCERFtmp$obsTime <- year
    toCERFtmp$RECIPIENT <- "Central Emergency Response Fund"
    names(toCERFtmp) <- c("DONOR","obsValue","obsTime","RECIPIENT")
    toCERFtmp$obsValue <- as.numeric(gsub(",","", toCERFtmp$obsValue))
    toCERFtmp <- subset(toCERFtmp,!is.na(DONOR))
    if(exists("toCERF")){toCERF <- rbind(toCERF,toCERFtmp)}else{toCERF <- toCERFtmp}
  }
  rm(page)
}

cDONOR <- c()
cRECIPIENT <- c()
cobsTime <- c()
cobsValue <- c()
years <- unique(toCERF$obsTime)
donors <- unique(toCERF$DONOR)
for(i in 1:length(years)){
  year <- years[i]
  totalCERF <- sum(subset(toCERF, (obsTime==year))$obsValue,na.rm=TRUE)
  for(k in 1:length(donors)){
    donor <- donors[k]
    donortoCERF <- subset(toCERF, (DONOR==donor & obsTime==year))$obsValue
    if(length(donortoCERF)<1){donortoCERF<-0}
    toCERFpercent <- donortoCERF / totalCERF
    fromCERFset <- subset(fromCERF, (obsTime==year))
    recipients <- unique(fromCERFset$RECIPIENT)
    if(length(recipients)>0){
      for(m in 1:length(recipients)){
        recipient <- recipients[m]
        fromCERFnum <- subset(fromCERFset, RECIPIENT==recipient)$obsValue
        if(length(fromCERFnum)<1){fromCERFnum<-0}
        if(is.na(fromCERFnum)){fromCERFnum<-0}
        CERFimputed <- toCERFpercent * fromCERFnum
        cDONOR <- c(cDONOR,donor)
        cRECIPIENT <- c(cRECIPIENT,recipient)
        cobsTime <- c(cobsTime, year)
        cobsValue <- c(cobsValue, CERFimputed)
      }
    }
  }
}
cerffunding <- data.frame(cDONOR,cRECIPIENT,cobsTime,cobsValue)
names(cerffunding) <- c("CERF_DONOR","CERF_RECIPIENT","obsTime","obsValue")

#Convert cerffunding names... Would be nice to make this more robust in the future
oecd_codes <- read.csv("oecd_codes.csv",na.strings="",stringsAsFactors=FALSE)
cerffundingcurrent <- merge(cerffunding
                     ,oecd_codes
                     ,by.x = "CERF_DONOR"
                     ,by.y = "cerf_names"
                     )
cerffundingcurrent <- cerffundingcurrent[,c(1:5)]
names(cerffundingcurrent)[5] <- "DONOR"
cerffundingcurrent <- merge(cerffundingcurrent
                     ,oecd_codes
                     ,by.x = "CERF_RECIPIENT"
                     ,by.y = "cerf_names"
                     )
cerffundingcurrent <- cerffundingcurrent[,c(1:6)]
names(cerffundingcurrent)[6] <- "RECIPIENT"

####Calculate implied price deflator from OECD####
#Use oecd_deflator.R
#Unfortunately we only get 44 donors out of it...
#Get the rest out of WDI...
deflator <- read.csv("oecd_deflator.csv",na.strings="",as.is=TRUE)
keep <- c(3,4,5,6)
cerffundingcurrent <- cerffundingcurrent[,keep]
cerffundingconstant <- merge(
    cerffundingcurrent
    ,deflator
    ,by=c("DONOR","obsTime")
  )

cerffundingconstant <- transform(cerffundingconstant,obsValue=obsValue.x*obsValue.y)
keep <- c("DONOR","RECIPIENT","obsTime","obsValue")
cerffundingconstant <- cerffundingconstant[keep]
cerffundingconstant <- cerffundingconstant[complete.cases(cerffundingconstant),]
cerffundingconstant <- transform(cerffundingconstant,obsValue=obsValue/1000000)

#Let's add everything together
ha$obsValue <- as.numeric(ha$obsValue)
allunfunding$obsValue <- as.numeric(allunfunding$obsValue)
eufunding$obsValue <- as.numeric(eufunding$obsValue)
cerffundingconstant$obsValue <- as.numeric(cerffundingconstant$obsValue)

donorRecipientCalc <- merge(
  ha
  ,allunfunding
  ,by=c("DONOR","RECIPIENT","obsTime")
  ,suffixes=c(".ha",".un")
  ,all.x=TRUE
)
EuCerf <- merge(
  eufunding
  ,cerffundingconstant
  ,by=c("DONOR","RECIPIENT","obsTime")
  ,suffixes=c(".eu",".cerf")
  ,all=TRUE
)
donorRecipientCalc <- merge(
  donorRecipientCalc
  ,EuCerf
  ,by=c("DONOR","RECIPIENT","obsTime")
  ,suffixes=c(".ha2",".ha3")
  ,all.x=TRUE
)
add <- c("obsValue.ha","obsValue.un","obsValue.eu","obsValue.cerf")
add <- which(names(donorRecipientCalc) %in% add)
donorRecipientCalc <- transform(donorRecipientCalc,obsValue.all=rowSums(donorRecipientCalc[,c(add)],na.rm=TRUE))
write.csv(donorRecipientCalc,"donor-recipient-calc.csv",na="",row.names=FALSE)
