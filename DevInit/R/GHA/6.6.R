###6.6  Top 10 recipients of DPP 2010-2013 mapped against indicator of risk/disaster
###Data source: OECD CRS, via CRS.py, INFORM scraped via inform.js
###Date of download: 14 April 2015, 24 March 2015
###By Alex Miller
#install.packages('plyr')
#require(devtools)
#install_github('ramnathv/rCharts')
#install.packages("xlsx")
#install.packages("rJava")
#Add jvm.dll to your PATH, w/ 64 bit java
library(rJava)
library(xlsx)
library(plyr)
library(rCharts)
####Data####
dacPath <- "C:/git/alexm-util/DevInit/R/GHA/dac.csv"
dac <- read.csv(dacPath,header=T,as.is=T)

#Parallel sum for creating new var with NA's present
psum <- function(..., na.rm=FALSE) { 
  x <- list(...)
  rowSums(matrix(unlist(x), ncol=length(x)), na.rm=na.rm)
}

#Set the working directory
wd <- "S:/Projects/Programme resources/Data/GHA calcs and analyses/April 2015/Datasets - do not edit/CRS/Other/CRS CSV files April 2015/"
setwd(wd)

#Define the datasets we want to work with
datasets <- c("CRS 2013 data.csv"
              ,"CRS 2012 data.csv"
              ,"CRS 2011 data.csv"
              ,"CRS 2010 data.csv"
)
#Define the purposecodes we want to filter by
purposes <- c(74010)

#Define the flownames we want to filter by
flownames <- c("ODA Grants","ODA Grant-Like", "ODA Loans", "Equity Investment")

#Set up empty variables
Year <- c()
recipientname <- c()
usd_disbursement_defl <- c()
purposecode <- c()
purposename <- c()

#Iterate through the datasets
for(i in 1:length(datasets)){
  dataset <- datasets[i]
  #Read it in
  dat <- read.csv(dataset,stringsAsFactors=FALSE,encoding="latin1")
  #Keep only those rows which purposecode == purposes
  dat <- dat[which(dat$purposecode %in% purposes),]
  #Keep only those rows which flowname == flownames
  dat <- dat[which(dat$flowname %in% flownames),]
  #Keep only those rows which donorname == dac
  dat <- dat[which(dat$donorname %in% dac$donorname),]
  #Keep only those rows which have usd_disbursement_defl
  dat <- dat[complete.cases(dat$usd_disbursement_defl),]
  #Append to our blank variables
  Year <- c(Year,dat$Year)
  recipientname <- c(recipientname,dat$recipientname)
  usd_disbursement_defl <- c(usd_disbursement_defl,dat$usd_disbursement_defl)
  purposecode <- c(purposecode,dat$purposecode)
  purposename <- c(purposename,dat$purposename)
  #Repeat
}

#Create a dataframe out of our new variables
dat <- data.frame(Year
                  ,recipientname
                  ,usd_disbursement_defl
                  ,purposecode
                  ,purposename
                  ,stringsAsFactors=FALSE)

#Pull in ISO3 list of High risk countries
highPath <- "C:/git/alexm-util/DevInit/R/GHA/high-iso3.csv"
highs <- read.csv(highPath,as.is=T,header=T)

#Pull in ISO3 list
iso3Path <- "C:/git/alexm-util/DevInit/R/GHA/CRS-iso3.csv"
iso3s <- read.csv(iso3Path,as.is=T,header=T)

#Merge Iso3 list with dataframe
dat <- merge(dat,
             iso3s,
             by="recipientname",
             all.x=TRUE)

#Pull in INFORM
informPath <- "S:/Projects/Programme resources/Data/GHA calcs and analyses/April 2015/Datasets - do not edit/INFORM/Trends in INFORM, 2011-2015.xlsx"
inform <- read.xlsx(informPath,sheetName="INFORM 2011-2015",header=T,stringsAsFactors=FALSE)
names(inform)[which(names(inform)=="ISO3")] <- "Iso3"
names(inform)[which(names(inform)=="INFORM.Risk.Index")] <- "InfoRM Risk Index"
keep <- c("Iso3","Year","InfoRM Risk Index")
inform <- inform[keep]
inform["InfoRM Risk Index"] <- lapply(inform["InfoRM Risk Index"],as.numeric)

#Merge inform list with dataframe
dat <- merge(dat,
             inform,
             by=c("Iso3","Year"),
             all.x=TRUE,
             suffixes=c(".crs",".inform"))

#Pull in INFORM categorical
informPath <- "S:/Projects/Programme resources/Data/GHA calcs and analyses/April 2015/What is it Spent on/6.5 & 6.6/Inform.xlsx"
inform2 <- read.xlsx(informPath,sheetName="Sheet2",header=T,stringsAsFactors=FALSE)
names(inform2)[which(names(inform2)=="ISO3")] <- "Iso3"

#Merge inform2 with dataframe
dat <- merge(dat,
             inform2,
             by=c("Iso3"),
             all.x=TRUE,
             suffixes=c(".crs",".inform"))


#Set the working directory to a different folder
wd <- "S:/Projects/Programme resources/Data/GHA calcs and analyses/April 2015/What is it Spent on/6.5 & 6.6/"
setwd(wd)

#Create a pivot table, grouping by recipientname to calculate top 10
#And making a new variable named usd_sum, which is the sum of
#usd_disbursement_defl
pivot <- ddply(dat
               ,.(recipientname)
               ,summarize,usd_sum=sum(usd_disbursement_defl,na.rm=TRUE))

#Sort
pivot <- pivot[order(-pivot$usd_sum),]

#Export as csv
write.csv(pivot,"2010-2013 Top recipients CRS DPP.csv",row.names=FALSE,na="")

#Calculate top ten recipients, excluding unspecified and regions
recips <- pivot$recipientname[which(!grepl(", regional",pivot$recipientname))]
recips <- recips[which(!grepl(", unspecified",recips))]
#Remove former yugoslavia
recips <- recips[which(recips!="States Ex-Yugoslavia")]
top10 <- recips[1:10]

#Create another pivot table, grouping by Year and recipientname
#And making a new variable named usd_sum, which is the sum of
#usd_disbursement_defl
pivot2 <- ddply(dat
               ,.(Iso3, recipientname,Year)
               ,summarize,usd_sum=sum(usd_disbursement_defl,na.rm=TRUE))

#Merge with informWide
pivot2 <- merge(pivot2,
             inform,
             by=c("Iso3","Year"),
             all.x=TRUE,
             suffixes=c(".crs",".inform"))
pivot2$top10 <- pivot2$recipientname %in% top10
pivot2$highRisk <- pivot2$Iso3 %in% highs$Iso3

#Export as csv
write.csv(pivot2,"2010-2013 CRS and INFORM.csv",row.names=FALSE,na="")

#Create a third pivot table, grouping by Natural Hazard Category
#And making a new variable named usd_sum, which is the sum of
#usd_disbursement_defl
pivot3 <- ddply(dat
                ,.(Year,natCat)
                ,summarize,usd_sum=sum(usd_disbursement_defl,na.rm=TRUE),usd_avg=mean(usd_disbursement_defl,na.rm=TRUE))

pivot3 <- pivot3[which(!is.na(pivot3$natCat)),]
pivot3 <- pivot3[which(pivot3$natCat!="Zero"),]


#Export as csv
write.csv(pivot3,"2010-2013 CRS by Natural Cat.csv",row.names=FALSE,na="")

####Define DI colors####
diColors <- c("#ba0c2f" #Red
              ,"#1b365d" #blue
              ,"#ea7600" #Orange
              ,"#93328e" #purple
              ,"#0095c8" #lightblue
              ,"#b7bf10" #Yellow
)
diColorsLong <- c('#00688c'
                  ,'#be84bb'
                  ,'#662363'
                  ,'#b7bf10'
                  ,'#0c192d'
                  ,'#1b365d'
                  ,'#cceaf4'
                  ,'#f2ad66'
                  ,'#ea7600'
                  ,'#0095c8'
                  ,'#595d07'
                  ,'#66bfde'
                  ,'#471845'
                  ,'#e9d6e8'
                  ,'#820820'
                  ,'#93328e'
                  ,'#004862'
                  ,'#f7f2cf'
                  ,'#80850b'
                  ,'#f7ced5'
                  ,'#d4d970'
                  ,'#d1d7df'
                  ,'#122541'
                  ,'#76869e'
                  ,'#5b0516'
                  ,'#fbe4cc'
                  ,'#ba0c2f'
                  ,'#723900'
                  ,'#d66d82'
                  ,'#a35200'
)

rPlotColors <- paste0("{color: {scale: {type: gradient, lower: "
                      ,"#f7ced5"
                      ,", upper:" 
                      ,"#820820"
                      ,"}}}"
)

#defaultCol <- paste0("#!d3.scale.ordinal().range(["
#                     ,substr(paste("'",diColors,"',",collapse="",sep=""),1,nchar(paste("'",diColors,"',",collapse="",sep=""))-1)
#                     ,"])!#")

#defaultColLong <- paste0("#!d3.scale.ordinal().range(["
#                     ,substr(paste("'",diColorsLong,"',",collapse="",sep=""),1,nchar(paste("'",diColors,"',",collapse="",sep=""))-1)
#                     ,"])!#")

####Graphs####
## Scatter All Recipients nvd3
scatterDat <- pivot2
scatterDat$recipientname <- iconv(scatterDat$recipientname,to="utf8")
scatterDat <- scatterDat[complete.cases(scatterDat["InfoRM Risk Index"]),]
scatterDat <- subset(scatterDat, usd_sum > 0)
scatterDat$usd_sum <- log(scatterDat$usd_sum*1000000)
p1 <- nPlot(y = "usd_sum"
            ,x = "InfoRM Risk Index"
            , data = scatterDat
            , type = 'scatterChart'
            #, group = 'Year'
)
#p1$chart(size = '#! function(d){return 100} !#')
p1$chart(color = "#! function(d){
          var col = d3.scale.ordinal();
          col.domain(['2011','2012','2013']);
          col.range(['#ba0c2f','#93328e','#0095c8']);
          return col(d.key);
         } !#")
p1$xAxis(axisLabel = "InfoRM Risk Index")
p1$yAxis(tickFormat = "#!function (x) {
          var output = Math.round(100*Math.pow(Math.E,x))/100,
          num = d3.formatPrefix(output).scale(output),
          sym = d3.formatPrefix(output).symbol;
          return d3.round(num)+sym;
         }!#")
p1$yAxis(axisLabel = "Deflated USD")
p1$chart(tooltipContent = "#! function(key, x, y, e){ 
  return 'Country: ' + e.point.recipientname + '<br/>Year: ' + e.point.Year 
} !#")
p1$addFilters("Year")
p1$chart(forceY = c(1, log(110000000)))
p1$chart(forceX = c(0, 10))
#p1$set(disabled = c(F, T, T))
#p1$chart(showControls = FALSE)
#p1

## Scatter All Recipients dimple
scatterDat <- pivot2
scatterDat$recipientname <- iconv(scatterDat$recipientname,to="utf8")
scatterDat <- scatterDat[complete.cases(scatterDat["InfoRM Risk Index"]),]
scatterDat <- subset(scatterDat, usd_sum > 0)
scatterDat$usd_sum <- scatterDat$usd_sum*1000000
d1 <- dPlot(
  y = "usd_sum",
  x = "InfoRM Risk Index",
  groups = c("recipientname",rep("")),
  data = subset(scatterDat,Year == 2013),
  type = "bubble"
)
d1$defaultColors(diColors)
d1$xAxis( type = "addMeasureAxis")
d1$yAxis( type = "addLogAxis")
d1$setTemplate(afterScript = "
               <script>
                myChart.draw()
                myChart.axes[1].titleShape.text('Deflated USD')
                myChart.svg.append('text')
                .attr('x', 120)
                .attr('y', 20)
                .text('Receipts of Disaster Preparedness and Prevention ODA against InfoRM Risk Index (2013)')
                .style('text-anchor','beginning')
                .style('font-size', '100%')
                .style('font-family','sans-serif')
                </script>               
                ")
#d1

## Scatter Top Recipients nvd3
scatterDat <- pivot2[which(pivot2$recipientname %in% top10),]
scatterDat$recipientname <- iconv(scatterDat$recipientname,to="utf8")
scatterDat <- scatterDat[complete.cases(scatterDat["InfoRM Risk Index"]),]
scatterDat <- subset(scatterDat, usd_sum > 0)
scatterDat$usd_sum <- log(scatterDat$usd_sum*1000000)
p2 <- nPlot(y = "usd_sum"
            ,x = "InfoRM Risk Index"
            , data = scatterDat
            , type = 'scatterChart'
            #, group = 'Year'
)
#p2$chart(size = '#! function(d){return 100} !#')
p2$chart(color = "#! function(d){
         var col = d3.scale.ordinal();
         col.domain(['2011','2012','2013']);
         col.range(['#ba0c2f','#93328e','#0095c8']);
         return col(d.key);
         } !#")
p2$xAxis(axisLabel = "InfoRM Risk Index")
p2$yAxis(tickFormat = "#!function (x) {
         var output = Math.round(100*Math.pow(Math.E,x))/100,
         num = d3.formatPrefix(output).scale(output),
         sym = d3.formatPrefix(output).symbol;
         return d3.round(num)+sym;
         }!#")
p2$yAxis(axisLabel = "Deflated USD")
p2$chart(tooltipContent = "#! function(key, x, y, e){ 
         return 'Country: ' + e.point.recipientname + '<br/>Year: ' + e.point.Year 
         } !#")
p2$addFilters("Year")
p2$chart(forceY = c(1, log(110000000)))
p2$chart(forceX = c(0, 10))
#p2$set(disabled = c(F, T, T))
#p2$chart(showControls = FALSE)
#p2

## Scatter top Recipients
scatterDat <- pivot2[which(pivot2$recipientname %in% top10),]
scatterDat$recipientname <- iconv(scatterDat$recipientname,to="utf8")
scatterDat <- scatterDat[complete.cases(scatterDat["InfoRM Risk Index"]),]
scatterDat <- subset(scatterDat, usd_sum > 0)
scatterDat$usd_sum <- scatterDat$usd_sum*1000000
d2 <- dPlot(
  y = "usd_sum",
  x = "InfoRM Risk Index",
  groups = c("recipientname",rep("")),
  data = subset(scatterDat, Year == 2013),
  type = "bubble"
)
d2$defaultColors(diColors)
d2$xAxis( type = "addMeasureAxis",overrideMin=3)
d2$yAxis( type = "addMeasureAxis")
d2$setTemplate(afterScript = "
               <script>
               myChart.draw()
               myChart.axes[1].titleShape.text('Deflated USD')
               myChart.svg.append('text')
               .attr('x', 80)
               .attr('y', 20)
               .text('Top 10 Recipients of Disaster Preparedness and Prevention ODA against InfoRM Risk Index (2013)')
               .style('text-anchor','beginning')
               .style('font-size', '100%')
               .style('font-family','sans-serif')
               </script>               
               ")
#d2

## Scatter Very high Recipients
scatterDat <- pivot2[which(pivot2$Iso3 %in% highs$Iso3),]
scatterDat$recipientname <- iconv(scatterDat$recipientname,to="utf8")
scatterDat <- scatterDat[complete.cases(scatterDat["InfoRM Risk Index"]),]
scatterDat <- subset(scatterDat, usd_sum > 0)
scatterDat$usd_sum <- log(scatterDat$usd_sum*1000000)
p3 <- nPlot(y = "usd_sum"
            ,x = "InfoRM Risk Index"
            , data = scatterDat
            , type = 'scatterChart'
            #, group = 'Year'
)
#p3$chart(size = '#! function(d){return 100} !#')
p3$chart(color = "#! function(d){
         var col = d3.scale.ordinal();
         col.domain(['2011','2012','2013']);
         col.range(['#ba0c2f','#93328e','#0095c8']);
         return col(d.key);
         } !#")
p3$xAxis(axisLabel = "InfoRM Risk Index")
p3$yAxis(tickFormat = "#!function (x) {
         var output = Math.round(100*Math.pow(Math.E,x))/100,
         num = d3.formatPrefix(output).scale(output),
         sym = d3.formatPrefix(output).symbol;
         return d3.round(num)+sym;
         }!#")
p3$yAxis(axisLabel = "Deflated USD")
p3$chart(tooltipContent = "#! function(key, x, y, e){ 
         return 'Country: ' + e.point.recipientname + '<br/>Year: ' + e.point.Year 
         } !#")
p3$addFilters("Year")
p3$chart(forceY = c(1, log(73000000)))
p3$chart(forceX = c(0, 10))
#p3$set(disabled = c(F, T, T))
#p3$chart(showControls = FALSE)
#p3

## Scatter Very high Recipients
scatterDat <- pivot2[which(pivot2$Iso3 %in% highs$Iso3),]
scatterDat$recipientname <- iconv(scatterDat$recipientname,to="utf8")
scatterDat <- scatterDat[complete.cases(scatterDat["InfoRM Risk Index"]),]
scatterDat <- subset(scatterDat, usd_sum > 0)
scatterDat$usd_sum <- scatterDat$usd_sum*1000000
d3 <- dPlot(
  y = "usd_sum",
  x = "InfoRM Risk Index",
  groups = c("recipientname",rep("")),
  data = subset(scatterDat, Year == 2013),
  type = "bubble"
)
d3$defaultColors(diColors)
d3$xAxis( type = "addMeasureAxis",overrideMin=3)
d3$yAxis( type = "addMeasureAxis")
d3$setTemplate(afterScript = "
               <script>
               myChart.draw()
               myChart.axes[1].titleShape.text('Deflated USD')
               myChart.svg.append('text')
               .attr('x', 80)
               .attr('y', 20)
               .text('Receipts of Disaster Preparedness and Prevention ODA against Very High InfoRM Risk Index (2013)')
               .style('text-anchor','beginning')
               .style('font-size', '100%')
               .style('font-family','sans-serif')
               </script>               
               ")
#d3

## Bar natural cat
scatterDat <- pivot3
scatterDat <- subset(scatterDat, usd_sum > 0)
scatterDat$usd_sum <- scatterDat$usd_sum*1000000
scatterDat$usd_avg <- scatterDat$usd_avg*1000000
d4 <- dPlot(
  y = "usd_sum",
  x = "natCat",
  groups = c(rep("")),
  data = subset(scatterDat,Year == 2013),
  type = "bar"
)
d4$defaultColors(diColors)
d4$xAxis( type = "addCategoryAxis")
d4$yAxis( type = "addMeasureAxis")
d4$setTemplate(afterScript = "
               <script>
                myChart.draw()
                myChart.axes[0].titleShape.text('InfoRM Natural Hazard Category')
                myChart.axes[1].titleShape.text('Deflated USD')
                myChart.svg.append('text')
                .attr('x', 120)
                .attr('y', 20)
                .text('Total Disaster Preparedness and Prevention ODA against Natural Hazard (2013)')
                .style('text-anchor','beginning')
                .style('font-size', '100%')
                .style('font-family','sans-serif')
                </script>               
                ")
#d4

## Scatter All Recipients by category
scatterDat <- ddply(dat,.(recipientname,Year,natCat),summarize,usd_sum=sum(usd_disbursement_defl,na.rm=TRUE))
scatterDat$recipientname <- iconv(scatterDat$recipientname,to="utf8")
scatterDat <- scatterDat[complete.cases(scatterDat[c("natCat","usd_sum")]),]
scatterDat <- subset(scatterDat, usd_sum > 0)
scatterDat$usd_sum <- scatterDat$usd_sum*1000000
d5 <- dPlot(
  y = "usd_sum",
  x = "natCat",
  groups = c("recipientname",rep("")),
  data = subset(scatterDat,Year == 2013),
  type = "bubble"
)
d5$defaultColors(diColors)
d5$xAxis( type = "addCategoryAxis")
d5$yAxis( type = "addMeasureAxis")
d5$setTemplate(afterScript = "
               <script>
                myChart.draw()
                myChart.axes[0].titleShape.text('InfoRM Natural Hazard Category')
                myChart.axes[1].titleShape.text('Deflated USD')
                myChart.svg.append('text')
                .attr('x', 120)
                .attr('y', 20)
                .text('Total Disaster Preparedness and Prevention ODA against Natural Hazard (2013)')
                .style('text-anchor','beginning')
                .style('font-size', '100%')
                .style('font-family','sans-serif')
                </script>               
                ")
#d5

## GeoMean Investigation
scatterDat <- ddply(dat,.(recipientname,Year,Iso3)
                    ,summarize
                    ,usd_sum=sum(usd_disbursement_defl,na.rm=TRUE)
                    ,geoMean =mean(geoMean,na.rm=TRUE)
                    ,natCat = max(natCat))
scatterDat$recipientname <- iconv(scatterDat$recipientname,to="utf8")
scatterDat <- scatterDat[complete.cases(scatterDat[c("geoMean","usd_sum")]),]
scatterDat <- subset(scatterDat, usd_sum > 0)
scatterDat$usd_sum <- scatterDat$usd_sum*1000000
d6 <- dPlot(
  y = "usd_sum",
  x = "geoMean",
  groups = c("recipientname",rep("")),
  data = subset(scatterDat,Year == 2013),
  type = "bubble"
)
d6$defaultColors(diColors)
d6$xAxis( type = "addMeasureAxis")
d6$colorAxis(type='addColorAxis',colorSeries="natCat")
d6$yAxis( type = "addMeasureAxis")
d6$setTemplate(afterScript = "
               <script>
               myChart.draw()
               myChart.axes[0].titleShape.text('InfoRM Natural Risk Index')
               myChart.axes[1].titleShape.text('Deflated USD')
               myChart.svg.append('text')
               .attr('x', 120)
               .attr('y', 20)
               .text('Total Disaster Preparedness and Prevention ODA against Natural Risk Index (2013)')
               .style('text-anchor','beginning')
               .style('font-size', '100%')
               .style('font-family','sans-serif')
               </script>               
               ")
d6

####Export them####
charts <- c(d1
            ,p1
            ,d2
            ,p2
            ,d3
            ,p3
            ,d4
            ,d5)

for(i in 1:length(charts)){
  chart <- charts[[i]]
  chart$save(paste0('//dipr-dc01/home$/AlexM/My Documents/GHA/6.6/chart',i,'.html'), cdn = TRUE)
}

names(dat)[names(dat)=="InfoRM Risk Index"] <- "Inform.Risk.Index"
collapsedDat <- ddply(dat,.(Iso3,Year,recipientname,purposecode,purposename,Inform.Risk.Index,natCat,nat,vuln,lackOfCoping,geoMean),summarize,usd_disbursement_defl=sum(usd_disbursement_defl,na.rm=TRUE))
collapsedDat <- transform(collapsedDat,top10=recipientname %in% top10)
collapsedDat <- transform(collapsedDat,vHighOrHigh=Iso3 %in% highs$Iso3)
write.csv(collapsedDat,"final_data.csv",row.names=FALSE,na="")
