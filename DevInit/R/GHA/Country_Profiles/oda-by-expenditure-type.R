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
donorIMO <- ddply(IMO,.(DONOR,obsTime),summarize,total=sum(obsValue,na.rm=TRUE))
donorIMO$metaSector <- "Multilateral ODA"
donorIMO <- donorIMO[,c(1,3,2)]
donorBySector <- ddply(t1,.(DONOR,metaSector,obsTime),summarize,total=sum(obsValue,na.rm=TRUE))
donorBySector <- rbind(donorBySector,donorIMO)
donorBySector <- donorBySector[order(donorBySector$DONOR),]
names(donorBySector)[which(names(donorBySector)=="total")] <- timeVarName

####OECD DAC RECIP####
#Define DAC CRS URL, Constant price Gross Disbursements####
recips <- "625+130+666+287+228+247+231+232+437+740+235+238+243+349+738+543+549+248+555+251+255+635+660+260+261+665+755+272+273+279+640+278+573+285+85+550+580+265"
recips <- strsplit(recips,"+",fixed=TRUE)[[1]]
recipBySectorTotal <- foreach(i=1:length(recips),.packages=c("plyr","rsdmx")
                              ,.combine=rbind) %do% 
  {
  recip <- recips[i]
  message(recip)
  CRSUrl <- paste0("http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/CRS1/20001+918.",recip,".1000+450+100+110+111+11110+11120+11130+11182+112+11220+11230+11240+113+11320+11330+114+11420+11430+120+121+12110+12181+12182+12191+122+12220+12230+12240+12250+12261+12262+12263+12281+130+13010+13020+13030+13040+13081+140+14010+14015+14020+14021+14022+14030+14031+14032+14040+14050+14081+150+151+15110+15111+15112+15113+15130+15150+15151+15152+15153+15160+15170+152+15210+15220+15230+15240+15250+15261+160+16010+16020+16030+16040+16050+16061+16062+16063+16064+200+210+21010+21020+21030+21040+21050+21061+21081+220+22010+22020+22030+22040+230+23010+23020+23030+23040+23050+23061+23062+23063+23064+23065+23066+23067+23068+23069+23070+23081+23082+240+24010+24020+24030+24040+24081+250+25010+25020+300+310+311+31110+31120+31130+31140+31150+31161+31162+31163+31164+31165+31166+31181+31182+31191+31192+31193+31194+31195+312+31210+31220+31261+31281+31282+31291+313+31310+31320+31381+31382+31391+320+321+32110+32120+32130+32140+32161+32162+32163+32164+32165+32166+32167+32168+32169+32170+32171+32172+32182+322+32210+32220+32261+32262+32263+32264+32265+32266+32267+32268+323+32310+331+33110+33120+33130+33140+33150+33181+332+33210+400+410+41010+41020+41030+41040+41050+41081+41082+430+43010+43030+43040+43050+43081+43082+500+510+51010+520+52010+530+53030+53040+600+60010+60020+60030+60040+60061+60062+60063+700+720+72010+72040+72050+730+73010+740+74010+910+91010+930+93010+998+99810+99820.100.100.D.112.100/all?startTime=",startYear,"&endTime=",endYear)
  
  #Download data####
  t1r <-OECD(CRSUrl)
  
  #Add metaSector and remove Action Relating to Debt####
  t1r <- transform(t1r, metaSector = findMeta(SECTOR))
  t1r <- t1r[t1r$metaSector!="ERR",]
  
  #Final Transformations####
  recipBySector <- ddply(t1r,.(RECIPIENT,metaSector,obsTime),summarize,total=sum(obsValue,na.rm=TRUE))
  recipBySector <- recipBySector[order(recipBySector$RECIPIENT),]
  names(recipBySector)[which(names(recipBySector)=="total")] <- timeVarName
  if(nrow(recipBySector[which(recipBySector$RECIPIENT=="Côte d'Ivoire"),])>0){
    recipBySector[which(recipBySector$RECIPIENT=="Côte d'Ivoire"),]$RECIPIENT <- "Cote d'Ivoire"
  }
  recipBySector
  }

#Let's save static files...
write.csv(recipBySectorTotal,paste0("recipBySector ",timeVarName,".csv"),na="",row.names=FALSE)
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
