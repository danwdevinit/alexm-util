#install.packages("rsdmx")
#install.packages("plyr")
#install.packages("dplyr")
#install.packages("httpuv")
#install.packages("googlesheets")
#install.packages("foreach")
library("foreach")
library(plyr)
library(rsdmx)
library(plyr)
suppressPackageStartupMessages(library("dplyr"))
library(httpuv)
library(googlesheets)

#Configuration
setwd("C:/git/alexm-util/DevInit/R/GHA/Country_Profiles")
startYear <- "2000"
endYear <- "2015"
includeEUinRecipientTotals <- TRUE
timeVarName <- paste0("Total ",startYear,"-",endYear)

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

#Define sector groupings####
findMeta <- function(sectorVar){
  sectors <- c(
    "This Space Intentionally Left Blank"
    ,"IX. Administrative Costs of Donors, Total"  
    ,"I.1.a. Education, Level Unspecified, Total"              
    ,"I.1.b. Basic Education, Total"                           
    ,"I.1.c. Secondary Education, Total"                       
    ,"I.1.d. Post-Secondary Education, Total"                  
    ,"I.2.a. Health, General, Total"                           
    ,"I.2.b. Basic Health, Total"                              
    ,"I.3. Population Pol./Progr. & Reproductive Health, Total"
    ,"I.4. Water Supply & Sanitation, Total"                   
    ,"I.5.a. Government & Civil Society-general, Total"        
    ,"I.5.b. Conflict, Peace & Security, Total"                
    ,"I.6. Other Social Infrastructure & Services, Total"      
    ,"II.1. Transport & Storage, Total"                        
    ,"II.2. Communications, Total"                             
    ,"II.3. Energy, Total"                                     
    ,"II.4. Banking & Financial Services, Total"               
    ,"II.5. Business & Other Services, Total"                  
    ,"III.1.a. Agriculture, Total"                             
    ,"III.1.b. Forestry, Total"                                
    ,"III.1.c. Fishing, Total"                                 
    ,"III.2.a. Industry, Total"                                
    ,"III.2.b. Mineral Resources & Mining, Total"              
    ,"III.2.c. Construction, Total"                            
    ,"III.3.a. Trade Policies & Regulations, Total"            
    ,"III.3.b. Tourism, Total"                                 
    ,"IV.1. General Environment Protection, Total"             
    ,"IV.2. Other Multisector, Total"
    ,"XII. Unallocated / Unspecified, Total"
    ,"XI. Refugees in Donor Countries, Total"
    ,"VI.1. General Budget Support, Total"                     
    ,"VI.2. Dev. Food Aid/Food Security Ass., Total"           
    ,"VI.3. Other Commodity Ass., Total"                       
    ,"VII. Action Relating to Debt, Total"                     
    ,"VIII.1. Emergency Response, Total"                       
    ,"VIII.2. Reconstruction Relief & Rehabilitation, Total"   
    ,"VIII.3. Disaster Prevention & Preparedness, Total"                            
  )
  publicServiceProvision <- c(
    sectors[3]
    ,sectors[4]
    ,sectors[5]
    ,sectors[6]
    ,sectors[7]
    ,sectors[8]
    ,sectors[9]
    ,sectors[10]
  )
  governancePeaceSecurity <- c(
    sectors[11]
    ,sectors[12]
  )
  otherCountryProgrammableAid <- c(
    sectors[13]
    ,sectors[14]
    ,sectors[15]
    ,sectors[16]
    ,sectors[17]
    ,sectors[18]
    ,sectors[19]
    ,sectors[20]
    ,sectors[21]
    ,sectors[22]
    ,sectors[23]
    ,sectors[24]
    ,sectors[25]
    ,sectors[26]
    ,sectors[27]
    ,sectors[28]
  )
  humanitarianAid <- c(
    sectors[35]
    ,sectors[36]
    ,sectors[37]
  )
  otherNonCountryProgrammableAid <- c(
    sectors[2]
    ,sectors[29]
    ,sectors[30]
    ,sectors[31]
    ,sectors[32]
    ,sectors[33]
  )
  newSectors <- c()
  for(i in 1:length(sectorVar)){
    sector <- sectorVar[i]
    if(sector %in% publicServiceProvision){
      newSector <- "Public service provision"
    }
    else if(sector %in% governancePeaceSecurity){
      newSector <- "Governance, peace and security"
    }
    else if(sector %in% otherCountryProgrammableAid){
      newSector <- "Other country programmable aid"
    }
    else if(sector %in% humanitarianAid){
      newSector <- "Humanitarian aid"
    }
    else if(sector %in% otherNonCountryProgrammableAid){
      newSector <- "Other non-country programmable aid"
    }
    else{newSector <- "ERR"}
    newSectors <- c(newSectors,newSector)
  }
  return(newSectors)
}

####OECD DAC DONORS####
#Define DAC CRS URL, Constant price Gross Disbursements###
CRSUrl <- paste0("http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/CRS1/801+1+2+301+68+3+18+4+5+40+20+21+6+701+742+22+7+820+8+76+9+69+61+50+10+11+12+302+918+301+4+5+6+701+12+302+1+2+68+3+18+4+5+40+21+6+22+7+76+9+69+61+50+10+12+1+2+68+3+18+4+5+40+21+6+22+7+76+9+69+61+50+10+12+918.10100.111+112+113+114+121+122+130+140+151+152+160+210+220+230+240+250+311+312+313+321+322+323+331+332+410+430+510+520+530+600+720+730+740+910+930+998.100.100.D.112.100/all?startTime=",startYear,"&endTime=",endYear)
IMOUrl <- paste0("http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/TABLE2A/10100.801+1+2+301+68+3+18+4+5+40+20+21+6+701+742+22+7+820+8+76+9+69+61+50+10+11+12+302+913+914+921+916+953+906+1011+990+918+1311+811+1313+1312+944+901+905+912+988+903+958+976+812+104+951+978+971+959+948+974+967+963+923+964+960+966+928+20018+72+62+30+82+75+546+552+83+70+84+45+77+87+566+732+764+55+576+20007+1601+301+4+5+6+701+12+302+1+2+68+3+18+4+5+40+21+6+22+7+76+9+69+61+50+10+12+1+2+68+3+18+4+5+40+21+6+22+7+76+9+69+61+50+10+12+918.1.106.D/all?startTime=",startYear,"&endTime=",endYear)

#Download data###
t1 <- OECD(CRSUrl)
IMO <- OECD(IMOUrl)

#Add metaSector and remove Action Relating to Debt###
t1 <- transform(t1, metaSector = findMeta(SECTOR))
t1 <- t1[t1$metaSector!="ERR",]

#Final Transformations###
donorIMO <- ddply(IMO,.(DONOR),summarize,total=sum(obsValue,na.rm=TRUE))
donorIMO$metaSector <- "Multilateral ODA"
donorIMO <- donorIMO[,c(1,3,2)]
donorBySector <- ddply(t1,.(DONOR,metaSector),summarize,total=sum(obsValue,na.rm=TRUE))
donorBySector <- rbind(donorBySector,donorIMO)
donorBySector <- donorBySector[order(donorBySector$DONOR),]
names(donorBySector)[which(names(donorBySector)=="total")] <- timeVarName

####OECD DAC RECIP####
#Define DAC CRS URL, Constant price Gross Disbursements####
recips <- "71+86+64+62+30+66+35+57+45+93+65+63+61+88+55+85+130+142+133+136+139+225+236+227+287+228+230+229+231+232+233+234+247+235+274+245+271+238+239+240+241+243+244+248+249+251+252+253+255+256+257+258+259+275+260+261+266+276+268+269+270+272+273+218+279+278+280+282+283+285+288+265+376+377+373+328+329+352+331+388+386+336+338+378+340+342+381+347+349+351+354+358+385+361+364+366+382+383+384+375+387+425+428+431+434+437+440+443+446+451+454+457+460+463+725+728+730+732+740+735+738+742+745+748+751+753+755+761+764+765+769+625+610+611+666+630+612+645+613+614+655+635+660+665+640+615+616+617+530+540+543+546+549+552+555+558+561+566+573+576+550+580+831+832+840+836+859+860+845+850+856+858+861+862+880+866+868+870+872+854+876+225+236+287+228+231+232+233+235+274+245+271+238+240+243+244+249+251+252+253+255+256+259+260+266+268+269+272+273+279+278+282+283+285+288+349+728+745+765+625+666+630+635+660+580+836+880+866+872+854+248+279+265+740+614+615+57+93+85+142+136+230+229+234+247+241+261+280+352+342+347+351+364+428+446+451+738+753+755+769+610+612+645+614+665+640+616+617+543+573+550+832+859+860+862+880+868+870+71+86+64+66+65+63+55+130+133+139+227+239+257+275+276+270+218+376+377+352+336+338+378+340+381+354+358+385+366+382+383+384+425+431+434+437+440+454+457+460+463+730+751+764+611+613+655+616+540+543+549+555+831+832+859+845+856+861+870+876+62+30+35+45+61+258+376+373+328+329+331+388+386+361+382+375+387+443+725+732+735+742+748+761+530+546+552+558+561+566+576+840+850+858+88+236+287+228+229+231+232+233+234+247+235+271+238+240+241+243+244+251+252+253+255+256+259+260+266+268+269+272+273+278+282+283+285+288+349+351+364+428+446+625+614+66+93+227+287+228+231+232+238+249+253+255+260+266+279+280+285+288+265+428+451+745+753+625+610+611+630+613+614+660+615+616+617+230+233+244+257+268+270+376+377+328+329+352+338+378+340+381+349+354+385+382+383+384+375+446+457+761+765+655+831+832+836+859+860+845+856+861+862+880+866+870+872+854+64+57+142+133+225+287+228+229+231+232+233+234+247+235+271+238+243+244+248+251+252+253+255+256+260+261+266+272+273+279+278+283+285+265+349+740+765+625+666+635+660+665+640+543+573+550+580+836+859+860+866+872+71+86+93+85+610+611+612+613+614+615+616+617+71+86+93+85+610+611+612+613+614+615+616+617+225+236+227+287+228+230+229+231+232+233+234+247+235+274+245+271+238+239+240+241+243+244+248+249+251+252+253+255+256+257+259+275+260+261+266+268+269+270+272+273+278+280+282+283+285+288+265+377+328+329+352+378+340+381+349+354+382+383+384+375+446+457+832+836+862+880+866+870+872+854+287+230+232+240+244+255+256+260+269"
recips <- strsplit(recips,"+",fixed=TRUE)[[1]]
years <- c(as.integer(startYear):as.integer(endYear))
recipBySector <- foreach(i=1:length(recips),.combine=rbind) %do% {
recip <- recips[i]
t1r <- foreach(j=1:length(years),.combine=rbind) %do% {
  if(exists("t1r1")){rm(t1r1)}
  year <- years[j]
  message(paste(recip,year))
  CRSUrl <- paste0("http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/CRS1/20001.",recip,".111+112+113+114+121+122+130+140+151+152+160+210+220+230+240+250+311+312+313+321+322+323+331+332+410+430+510+520+530+600+720+730+740+910+930+998.100.100.D.112.100/all?startTime=",year,"&endTime=",year)
  CRSEUUrl <- paste0("http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/CRS1/918.",recip,".111+112+113+114+121+122+130+140+151+152+160+210+220+230+240+250+311+312+313+321+322+323+331+332+410+430+510+520+530+600+720+730+740+910+930+998.100.100.D.112.100/all?startTime=",year,"&endTime=",year)
  
  #Download data####
  t1r1 <- OECD(CRSUrl)
  if(!exists("t1r1")){t1r1 <- t1[0,]}
  if(includeEUinRecipientTotals){
    t1r1 <- rbind(t1r1,OECD(CRSEUUrl)) 
  }
  t1r1
}

#Add metaSector and remove Action Relating to Debt####
t1r <- transform(t1r, metaSector = findMeta(SECTOR))
t1r <- t1r[t1r$metaSector!="ERR",]

#Final Transformations####
recipBySector <- ddply(t1r,.(RECIPIENT,metaSector),summarize,total=sum(obsValue,na.rm=TRUE))
recipBySector <- recipBySector[order(recipBySector$RECIPIENT),]
names(recipBySector)[which(names(recipBySector)=="total")] <- timeVarName
if(nrow(recipBySector[which(recipBySector$RECIPIENT=="Côte d'Ivoire"),])>0){
  recipBySector[which(recipBySector$RECIPIENT=="Côte d'Ivoire"),]$RECIPIENT <- "Cote d'Ivoire"
}
recipBySector}

#Let's save static files...
write.csv(recipBySector,paste0("recipBySector ",timeVarName,".csv"),na="",row.names=FALSE)
write.csv(donorBySector,paste0("donorBySector ",timeVarName,".csv"),na="",row.names=FALSE)

#Automatic example
# uniqueDonors <- unique(donorBySector$DONOR)
# uniqueRecips <- unique(recipBySector$RECIPIENT)
# for(i in 1:length(uniqueDonors)){
#   uniqueDonor <- uniqueDonors[i]
#   update <- donorBySector[which(donorBySector$DONOR==uniqueDonor),][,c(2,3)]
#   sheet <- tryCatch({
#       attempt <- gs_title(uniqueDonor)
#     },warning = function(war){
#       message(war)
#       message("\n")
#     },error = function(err){
#       message(err)
#       message("\n")
#     })
#   if(!is.null(sheet)){
#     #It found the sheet, we can write it
#     gs_edit_cells(sheet,ws="Graph 2", input=update, col_names=FALSE)
#     gs_edit_cells(sheet,ws="Graph 2", input=paste0("Total official development assistance (ODA) expenditure type, ",startYear,"-",endYear," (US$m, constant 2012 prices)"),anchor="B8")
#     rm(sheet)
#     rm(attempt)
#   }else{
#     #We didn't find it. Create a new one?
#   }
# }
# for(i in 1:length(uniqueRecips)){
#   uniqueRecip <- uniqueRecips[i]
#   update <- recipBySector[which(recipBySector$RECIPIENT==uniqueRecip),][,c(2,3)]
#   sheet <- tryCatch({
#       attempt <- gs_title(uniqueRecip)
#     },warning = function(war){
#       message(war)
#       message("\n")
#     },error = function(err){
#       message(err)
#       message("\n")
#     })
#   if(!is.null(sheet)){
#     #It found the sheet, we can write it
#     gs_edit_cells(sheet,ws="Graph 2", input=update, col_names=FALSE)
#     gs_edit_cells(sheet,ws="Graph 2", input=paste0("Total official development assistance (ODA) expenditure type, ",startYear,"-",endYear," (US$m, constant 2012 prices)"),anchor="B9")
#     rm(sheet)
#     rm(attempt)
#   }else{
#     #We didn't find it. Create a new one?
#   }
# }
