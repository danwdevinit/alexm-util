####Data Tech and the GHA####
#By Alex Miller

#Set working directory
wd <- "C:/git/alexm-util/DevInit/R/GHA/presentation/tmp"
root <- "C:/git/alexm-util/DevInit/R/GHA/presentation/"
setwd(wd)

#Delete everything in tmp
unlink(dir(wd, full.names = TRUE),recursive=TRUE)
toRemove <- list.files(root,pattern="*.jpg|*.xml",full.names=TRUE)
for(i in 1:length(toRemove)){
  fileToRemove <- toRemove[i]
  file.remove(fileToRemove)
}

#Slide 4 - Writing####
data <- read.csv('../donorBySector.csv')
countries <- unique(data$DONOR)
for(i in 1:length(countries)){
  sub <- subset(data,DONOR==countries[i]) 
  write.csv(sub,paste0(countries[i],'.csv'),row.names=FALSE)
}

#Slide 5 - Renaming####
filenames <- list.files(wd,pattern="*.csv",full.names=TRUE)
for(i in 1:length(filenames)){
  bname <- tolower(basename(filenames[i]))
  bname <- gsub(" ","_",bname)
  file.rename(filenames[i],bname)
}

#Slide 6 - Downloading GDELT####
#Source URL is http://data.gdeltproject.org/events/index.html

#Static
zipname <-'http://data.gdeltproject.org/events/20150831.export.CSV.zip'
zip <- tempfile()
download.file(zipname,zip)
unzip(zip)

#Dynamic
for(i in 1:5){
  yesterday <- format(Sys.Date()-i, "%Y%m%d")
  yestwd <- paste(wd,yesterday,sep="/")
  message(yesterday)
  if(file.exists(yestwd)){
    setwd(yestwd)
    filename <- paste(yesterday,".export.CSV",sep="")
  }
  else{
    dir.create(paste(wd,yesterday,sep="/"))
    setwd(yestwd)
    filename <- paste(yesterday,".export.CSV",sep="")
    zipname <- paste("http://data.gdeltproject.org/events/",
                     filename,".zip",sep="")
    zip <- tempfile()
    download.file(zipname,zip)
    unzip(zip, files = NULL, list = FALSE, overwrite = TRUE,
          junkpaths = FALSE, exdir = ".", unzip = "internal",
          setTimes = FALSE)
  }
  setwd(wd)
}

#Slide 8 - "Big" Data Manipulation####
#Read
data <- read.csv("../crs2000.csv")
print(paste("This dataset is",ncol(data),"columns wide and",nrow(data),"rows long."))
View(data)
#Subset
afg <- subset(data,recipientname=="Afghanistan")
View(afg)
#Transform - Let's find where commitments exceed disbursements
pdiff <- function(xVector,yVector){
  diffs <- double(length(xVector))
  for(i in 1:length(xVector)){
    x <- xVector[i]
    y <- yVector[i]
    if(is.na(x)){
      x <- 0
    }
    if(is.na(y)){
      y <- 0
    }
    diff <- x-y
    if(diff<0){diff<-0}
    diffs[i] <- diff
  }
  return(diffs)
}
afg <- transform(afg,undisbursed_defl=pdiff(usd_commitment_defl,usd_disbursement_defl))
View(afg)
#Reshape - Which donors didn't uphold their commitments to Afghanistan in 2000?
library(plyr)
afg <- ddply(afg,.(donorname),summarize,undisbursed_total_defl=sum(undisbursed_defl,na.rm=TRUE))
View(afg)
afg <- afg[order(-afg$undisbursed_total_defl),]
View(afg)
afg <- transform(afg,donorname=reorder(donorname,-undisbursed_total_defl))
library(ggplot2)
d <- ggplot(afg,aes(x=factor(donorname),y=undisbursed_total_defl)) + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle=-90))
d

#Slide 9 - PDF scraping####
setwd(root)
system("python scrape_trends.py")
setwd(wd)
pdfData <- read.csv("global_trends.csv",check.names=FALSE,na.strings="-")

#Slide 10 - Web scraping####

#OECD Examples#
library(rsdmx)
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
#Grab url from https://stats.oecd.org/Index.aspx?DataSetCode=TABLE1
url <- "http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/TABLE1/12.1.1010.1140.A/all?startTime=2005&endTime=2014"
table1 <- OECD(url)

#World Bank Examples#
library(WDI)
indicator <- "NY.GDP.MKTP.KD" #GDP CONSTANT 2005 USD
wbconstant <- WDI(country = "all", 
           indicator = indicator, 
           start = 2000, 
           end = 2015,
           extra = TRUE
)
indicator <- "NY.GDP.MKTP.CD" #GDP CURRENT USD
wbcurrent <- WDI(country = "all", 
             indicator = indicator, 
             start = 2000, 
             end = 2015,
             extra = TRUE
)

#FTS Examples#
library(jsonlite)
root <- "http://fts.unocha.org/api/v1/"
appeals <- fromJSON(paste0(root,"Appeal/year/2015.json"))

projects <- fromJSON(paste(root,"Project/appeal/",appeals$id[1],".json",sep=""))
for(i in 2:nrow(appeals)){
  projects <- rbind(projects, fromJSON(paste(root,"Project/appeal/",appeals$id[i],".json",sep="")))
  print(paste("Pulling projects for appeal ==",appeals$id[i]))
}

contrib_appeal <- fromJSON(paste(root,"Contribution/appeal/",appeals$id[1],".json",sep=""))
for(i in 2:nrow(appeals)){
 contrib_appeal <- rbind(contrib_appeal, fromJSON(paste(root,"Contribution/appeal/",appeals$id[i],".json",sep="")))
 print(paste("Pulling contributions for appeal ==",appeals$id[i]))
}

#Difficult CERF Example#
library(rvest)
CERFyears <- c(2013:2014)
toUrl <- "http://www.unocha.org/cerf/our-donors/funding/pledges-and-contributions/"

if(exists("toCERF")){rm(toCERF)}

for(i in 1:length(CERFyears)){
  year <- CERFyears[i]
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

#Slide 11 - Merge####
wbdeflator <- merge(wbcurrent
                    ,wbconstant
                    ,by=intersect(names(wbcurrent),names(wbconstant))
)
wbdeflator <- transform(wbdeflator,base2005=(NY.GDP.MKTP.KD/NY.GDP.MKTP.CD))
keep <- c("iso2c","iso3c","country","year","base2005")
wbdeflator <- wbdeflator[keep]
refyear <- 2013
wbdeflator <- ddply(wbdeflator,.(country),function(x){
  x$baseref <- x$base2005 / subset(x,year==refyear)$base2005
  return(x)
})

#Slide 11 - ddply####
library(plyr)
pivot <- ddply(data,.(donorname),summarize,total=sum(usd_disbursement_defl,na.rm=TRUE))

#For more visuals, see GHA/scaled_pies.R, DevInit/UG_Choro.R