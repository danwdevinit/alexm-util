#install.packages("rsdmx")
#install.packages("curl")
#install.packages("plyr")
#install.packages("xlsx")
#install.packages("rJava")
#Add jvm.dll to your PATH, w/ 64 bit java
library(rsdmx)
library(rJava)
library(xlsx)
setwd("C:/git/alexm-util/DevInit/R")

#Func####
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

#Table 1 Constant 2012, All donors####
#Define URL
constantUrl <- "http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/TABLE1/20005+20001+801+1+2+301+68+3+18+4+5+40+20+21+6+701+742+22+7+820+8+76+9+69+61+50+10+11+12+302+20002+918+20006+72+62+30+82+75+546+552+83+70+84+45+77+87+566+732+764+55+576+20007+20003+301+4+5+6+701+12+302+20011+1+2+68+3+18+4+5+40+21+6+22+7+76+9+69+61+50+10+12+20004+1+2+68+3+18+4+5+40+21+6+22+7+76+9+69+61+50+10+12+918.1.1015.1140.D/all?startTime=1990&endTime=2014"

#Download data
t1Constant <- OECD(constantUrl)

#Sort
t1Constant <- t1Constant[order(t1Constant$DAC_DONOR,t1Constant$obsTime),]
keep <- c("DAC_DONOR","obsTime","obsValue")

#Reshape wide
t1ConstantWide <- reshape(t1Constant[keep],idvar="DAC_DONOR",timevar="obsTime",direction="wide")

#Rename wide varnames
colnames(t1ConstantWide)[which(substr(colnames(t1ConstantWide),1,3)=="obs")] <- substr(colnames(t1ConstantWide)[which(substr(colnames(t1ConstantWide),1,3)=="obs")],10,13)

#Download concepts
t1ConstantConcepts <- OECD(constantUrl,TRUE)

#Write xlsx
file <- "Table 1 Constant.xlsx"
wb <- createWorkbook()
dataSheet <- createSheet(wb,sheetName="data")
addDataFrame(t1Constant,dataSheet,row.names=FALSE)
dataWideSheet <- createSheet(wb,sheetName="datawide")
addDataFrame(t1ConstantWide,dataWideSheet,row.names=FALSE)
conceptSheet <- createSheet(wb,sheetName="concepts")
addDataFrame(t1ConstantConcepts,conceptSheet,row.names=FALSE)
saveWorkbook(wb, file)

#ETC####
#Define URL
url <- "http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/CRS1/20005+20001+801+1+2+301+68+3+18+4+5+40+20+21+6+701+742+22+7+820+8+76+9+69+61+50+10+11+12+302+20002+1012+913+914+921+915+916+953+1011+990+918+1311+811+1313+1312+901+905+909+912+988+903+958+976+104+951+978+971+959+948+974+967+963+923+964+966+928+20006+82+552+576+21600+1601.10100.1000.100.100.A.115.100/all?startTime=2004&endTime=2013"

#Download data
dat <- OECD(url)

#Save
write.xlsx(dat,"test.xlsx")
