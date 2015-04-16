###6.5 2009-2013DAC donors bilateral humanitarian assistance by expenditure type, 2008-2013
###Data source: OECD CRS, via CRS.py
###Date of download: 14 April 2015
###By Alex Miller
#install.packages('plyr')
#require(devtools)
#install_github('ramnathv/rCharts')
#install.packages('base64enc')
library(plyr)
library(rCharts)
####Data####

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
              ,"CRS 2009 data.csv"
              #,"CRS 2008 data.csv"
              )
#Define the purposecodes we want to filter by
purposes <- c(72010,72040,72050,73010,74010)

#Set up empty variables
Year <- c()
donorname <- c()
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
  #Keep only those rows which have usd_disbursement_defl
  dat <- dat[complete.cases(dat$usd_disbursement_defl),]
  #Append to our blank variables
  Year <- c(Year,dat$Year)
  donorname <- c(donorname,dat$donorname)
  usd_disbursement_defl <- c(usd_disbursement_defl,dat$usd_disbursement_defl)
  purposecode <- c(purposecode,dat$purposecode)
  purposename <- c(purposename,dat$purposename)
  #Repeat
}

#Create a dataframe out of our new variables
dat <- data.frame(Year
  ,donorname
  ,usd_disbursement_defl
  ,purposecode
  ,purposename
  ,stringsAsFactors=FALSE)

#Create a pivot table, grouping by Year, donorname, & purposecode
#And making a new variable named usd_sum, which is the sum of
#usd_disbursement_defl
pivot <- ddply(dat
   ,.(Year,donorname,purposecode,purposename)
   ,summarize,usd_sum=sum(usd_disbursement_defl,na.rm=TRUE))

#Reshape wide
pivotlong <- pivot
pivot <- reshape(pivot,idvar=c("donorname","purposecode","purposename"),timevar="Year",direction="wide")
#Add sum
pivot <- transform(pivot, usd_sum.total=psum(usd_sum.2009, usd_sum.2010, usd_sum.2011, usd_sum.2012, usd_sum.2013, na.rm=TRUE))
#Sort
pivot <- pivot[order(-pivot$usd_sum.total,pivot$donorname),]
#Rename variables
names(pivot)[which(substr(names(pivot),0,3)=="usd")] <-
  substr(names(pivot)[which(substr(names(pivot),0,3)=="usd")],9,nchar(names(pivot)[which(substr(names(pivot),0,3)=="usd")]))

#Export as csv
write.csv(pivot,"../Subsets/6.5/2009-2013 Donor by expenditure type by year.csv",row.names=FALSE,na="")

#Create another pivot table, grouping by donorname, & purposecode
#And making a new variable named usd_sum, which is the sum of
#usd_disbursement_defl
pivot2 <- ddply(dat
               ,.(donorname,purposecode,purposename)
               ,summarize,usd_sum=sum(usd_disbursement_defl,na.rm=TRUE))

#Sort to get wide vars in right order
pivot2 <- pivot2[order(pivot2$purposecode),]
#Reshape wide
pivot2long <- pivot2
pivot2 <- reshape(pivot2[c("donorname","purposecode","usd_sum")],idvar="donorname",timevar="purposecode",direction="wide")
#Add sum
pivot2 <- transform(pivot2, usd_sum.total=psum(usd_sum.73010, usd_sum.72010, usd_sum.72040, usd_sum.72050, usd_sum.74010, na.rm=TRUE))
#Sort
pivot2 <- pivot2[order(-pivot2$usd_sum.total),]
#Rename variables
names(pivot2)[which(substr(names(pivot2),0,3)=="usd")] <-
  substr(names(pivot2)[which(substr(names(pivot2),0,3)=="usd")],9,nchar(names(pivot2)[which(substr(names(pivot2),0,3)=="usd")]))
#Write
write.csv(pivot2,"../Subsets/6.5/2009-2013 Donor by expenditure type.csv",row.names=FALSE,na="")

#Create a third pivot table, grouping by donorname (filtered by purposecode)
#And making a new variable named usd_sum, which is the sum of
#usd_disbursement_defl
pivot3 <- ddply(dat
                ,.(donorname,Year)
                ,summarize,usd_sum=sum(usd_disbursement_defl,na.rm=TRUE))
#Sort to get wide vars in right order
pivot3 <- pivot3[order(pivot3$Year),]
#Reshape wide
pivot3long <- pivot3
pivot3 <- reshape(pivot3[c("donorname","Year","usd_sum")],idvar="donorname",timevar="Year",direction="wide")
#Add sum
pivot3 <- transform(pivot3, usd_sum.total=psum(usd_sum.2009, usd_sum.2010, usd_sum.2011, usd_sum.2012, usd_sum.2013, na.rm=TRUE))
#Sort
pivot3 <- pivot3[order(-pivot3$usd_sum.total),]
#Rename variables
names(pivot3)[which(substr(names(pivot3),0,3)=="usd")] <-
  substr(names(pivot3)[which(substr(names(pivot3),0,3)=="usd")],9,nchar(names(pivot3)[which(substr(names(pivot3),0,3)=="usd")]))
#Write
write.csv(pivot3,"../Subsets/6.5/2009-2013 Donor by Year.csv",row.names=FALSE,na="")

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
## Bar (just codes)
barDat <- ddply(dat,.(purposecode,purposename),summarize,total=sum(usd_disbursement_defl,na.rm=TRUE))
d0 <- dPlot(
  y = "purposename",
  x = "total",
  type = "bar",
  data = barDat,
  bounds = list(x = 300, y = 50, height = 300, width = 300)
)
d0$xAxis(type = "addMeasureAxis")
d0$yAxis(type = "addCategoryAxis")
d0$defaultColors(diColors)
d0$setTemplate(afterScript = "
  <script>
    myChart.draw()
    myChart.axes[0].titleShape.text('Deflated USD (millions)')
    myChart.axes[1].titleShape.text('Purpose Code')
    myChart.svg.append('text')
        .attr('x', 40)
        .attr('y', 20)
        .text('Total DAC Disbursements to Humanitarian Purpose Codes (2009-2013)')
        .style('text-anchor','beginning')
        .style('font-size', '100%')
        .style('font-family','sans-serif')
  </script>               
")
#d0

## Bar (top 10)
top <- pivot3$donorname[1:10]
barDat <- pivot3[which(pivot3$donorname %in% top),][c("donorname","total")]
leftovers <- pivot3[which(!pivot3$donorname %in% top),][c("donorname","total")]
total <- sum(leftovers$total,na.rm=TRUE)
donorname <- rep("Other Donors")
leftovers <- data.frame(donorname,total)
barDat <- rbind(barDat,leftovers)
d1 <- dPlot(
  x = "donorname",
  y = "total",
  data = barDat,
  type = "bar",
  bounds = list(x = 50, y = 50, height = 240, width = 600)
)
d1$defaultColors(diColors)
d1$setTemplate(afterScript = "
  <script>
    myChart.draw()
    myChart.axes[0].titleShape.text('Donor')
    myChart.axes[1].titleShape.text('Deflated USD (millions)')
    myChart.svg.append('text')
        .attr('x', 40)
        .attr('y', 20)
        .text('Total Disbursements to Humanitarian Purpose Codes (2009-2013)')
        .style('text-anchor','beginning')
        .style('font-size', '100%')
        .style('font-family','sans-serif')
  </script>               
")
#d1

## Bar (top 5)
top <- pivot3$donorname[1:5]
barDat <- pivot3[which(pivot3$donorname %in% top),][c("donorname","total")]
leftovers <- pivot3[which(!pivot3$donorname %in% top),][c("donorname","total")]
total <- sum(leftovers$total,na.rm=TRUE)
donorname <- rep("Other Donors")
leftovers <- data.frame(donorname,total)
barDat <- rbind(barDat,leftovers)
d2 <- dPlot(
  x = "donorname",
  y = "total",
  data = barDat,
  type = "bar"
)
d2$defaultColors(diColors)
d2$setTemplate(afterScript = "
  <script>
    myChart.draw()
    myChart.axes[0].titleShape.text('Donor')
    myChart.axes[1].titleShape.text('Deflated USD (millions)')
    myChart.svg.append('text')
        .attr('x', 40)
        .attr('y', 20)
        .text('Total Disbursements to Humanitarian Purpose Codes (2009-2013)')
        .style('text-anchor','beginning')
        .style('font-size', '100%')
        .style('font-family','sans-serif')
  </script>               
")
#d2

## vertical grouped bar (top 10)
top <- pivot3$donorname[1:10]
gBarDat <- pivot2long[which(pivot2long$donorname %in% top),]
leftovers <- pivot2long[which(!pivot2long$donorname %in% top),]
leftovers <- ddply(leftovers,.(purposecode,purposename),summarize,usd_sum=sum(usd_sum,na.rm=TRUE))
leftovers$donorname <- rep("Other Donors")
gBarDat <- rbind(gBarDat,leftovers)
d3 <- dPlot(
  x = c("donorname","purposename"),
  y = "usd_sum",
  groups = "purposename",
  data = gBarDat,
  type = "bar",
  bounds = list(x = 50, y = 50, height = 240, width = 600),
  color = diColors
)
d3$legend(
  x = 350,
  y = 50,
  width = 400,
  height = 100,
  horizontalAlign = "right"
)
d3$defaultColors(diColors)
d3$setTemplate(afterScript = "
  <script>
    myChart.draw()
    myChart.axes[0].titleShape.text('Donor')
    myChart.axes[1].titleShape.text('Deflated USD (millions)')
    myChart.svg.append('text')
        .attr('x', 40)
        .attr('y', 20)
        .text('Total Disbursements to Humanitarian Purpose Codes (2009-2013)')
        .style('text-anchor','beginning')
        .style('font-size', '100%')
        .style('font-family','sans-serif')
  </script>               
")
#d3

## vertical grouped bar (top 5)
top <- pivot3$donorname[1:5]
gBarDat <- pivot2long[which(pivot2long$donorname %in% top),]
leftovers <- pivot2long[which(!pivot2long$donorname %in% top),]
leftovers <- ddply(leftovers,.(purposecode,purposename),summarize,usd_sum=sum(usd_sum,na.rm=TRUE))
leftovers$donorname <- rep("Other Donors")
gBarDat <- rbind(gBarDat,leftovers)
d4 <- dPlot(
  x = c("donorname","purposename"),
  y = "usd_sum",
  groups = "purposename",
  data = gBarDat,
  type = "bar"
)
d4$legend(
  x = 350,
  y = 50,
  width = 400,
  height = 100,
  horizontalAlign = "right"
)
d4$defaultColors(diColors)
d4$setTemplate(afterScript = "
  <script>
    myChart.draw()
    myChart.axes[0].titleShape.text('Donor')
    myChart.axes[1].titleShape.text('Deflated USD (millions)')
    myChart.svg.append('text')
        .attr('x', 40)
        .attr('y', 20)
        .text('Total Disbursements to Humanitarian Purpose Codes (2009-2013)')
        .style('text-anchor','beginning')
        .style('font-size', '100%')
        .style('font-family','sans-serif')
  </script>               
")
#d4

## vertical stacked bar (top 10)
top <- pivot3$donorname[1:10]
gBarDat <- pivot2long[which(pivot2long$donorname %in% top),]
leftovers <- pivot2long[which(!pivot2long$donorname %in% top),]
leftovers <- ddply(leftovers,.(purposecode,purposename),summarize,usd_sum=sum(usd_sum,na.rm=TRUE))
leftovers$donorname <- rep("Other Donors")
gBarDat <- rbind(gBarDat,leftovers)
d5 <- dPlot(
  x = "donorname",
  y = "usd_sum",
  groups = "purposename",
  data = gBarDat,
  type = "bar",
  bounds = list(x = 50, y = 50, height = 240, width = 600)
)
d5$legend(
  x = 350,
  y = 50,
  width = 400,
  height = 100,
  horizontalAlign = "right"
)
d5$defaultColors(diColors)
d5$setTemplate(afterScript = "
  <script>
    myChart.draw()
    myChart.axes[0].titleShape.text('Donor')
    myChart.axes[1].titleShape.text('Deflated USD (millions)')
    myChart.svg.append('text')
        .attr('x', 40)
        .attr('y', 20)
        .text('Total Disbursements to Humanitarian Purpose Codes (2009-2013)')
        .style('text-anchor','beginning')
        .style('font-size', '100%')
        .style('font-family','sans-serif')
  </script>               
")
#d5

## vertical stacked bar (top 5)
top <- pivot3$donorname[1:5]
gBarDat <- pivot2long[which(pivot2long$donorname %in% top),]
leftovers <- pivot2long[which(!pivot2long$donorname %in% top),]
leftovers <- ddply(leftovers,.(purposecode,purposename),summarize,usd_sum=sum(usd_sum,na.rm=TRUE))
leftovers$donorname <- rep("Other Donors")
gBarDat <- rbind(gBarDat,leftovers)
d6 <- dPlot(
  x = "donorname",
  y = "usd_sum",
  groups = "purposename",
  data = gBarDat,
  type = "bar"
)
d6$legend(
  x = 350,
  y = 50,
  width = 400,
  height = 100,
  horizontalAlign = "right"
)
d6$defaultColors(diColors)
d6$setTemplate(afterScript = "
               <script>
               myChart.draw()
               myChart.axes[0].titleShape.text('Donor')
               myChart.axes[1].titleShape.text('Deflated USD (millions)')
               myChart.svg.append('text')
               .attr('x', 40)
               .attr('y', 20)
               .text('Total Disbursements to Humanitarian Purpose Codes (2009-2013)')
               .style('text-anchor','beginning')
               .style('font-size', '100%')
               .style('font-family','sans-serif')
               </script>               
               ")
#d6

## vertical stacked bar 100% (top 10)
top <- pivot3$donorname[1:10]
gBarDat <- pivot2long[which(pivot2long$donorname %in% top),]
leftovers <- pivot2long[which(!pivot2long$donorname %in% top),]
leftovers <- ddply(leftovers,.(purposecode,purposename),summarize,usd_sum=sum(usd_sum,na.rm=TRUE))
leftovers$donorname <- rep("Other Donors")
gBarDat <- rbind(gBarDat,leftovers)
d7 <- dPlot(
  x = "donorname",
  y = "usd_sum",
  groups = "purposename",
  data = gBarDat,
  type = "bar",
  bounds = list(x = 60, y = 100, height = 190, width = 580)
)
d7$legend(
  x = 65,
  y = 40,
  width = 700,
  height = 30,
  horizontalAlign = "left"
)
d7$yAxis(type = "addPctAxis")
d7$defaultColors(diColors)
d7$setTemplate(afterScript = "
               <script>
               myChart.draw()
               myChart.axes[0].titleShape.text('Donor')
               myChart.axes[1].titleShape.text('Deflated USD (millions)')
               myChart.svg.append('text')
               .attr('x', 40)
               .attr('y', 20)
               .text('Total Disbursements to Humanitarian Purpose Codes (2009-2013)')
               .style('text-anchor','beginning')
               .style('font-size', '100%')
               .style('font-family','sans-serif')
               </script>               
               ")
#d7

## vertical stacked bar 100% (top 5)
top <- pivot3$donorname[1:5]
gBarDat <- pivot2long[which(pivot2long$donorname %in% top),]
leftovers <- pivot2long[which(!pivot2long$donorname %in% top),]
leftovers <- ddply(leftovers,.(purposecode,purposename),summarize,usd_sum=sum(usd_sum,na.rm=TRUE))
leftovers$donorname <- rep("Other Donors")
gBarDat <- rbind(gBarDat,leftovers)
d8 <- dPlot(
  x = "donorname",
  y = "usd_sum",
  groups = "purposename",
  data = gBarDat,
  type = "bar",
  bounds = list(x = 60, y = 100, height = 190, width = 580)
)
d8$legend(
  x = 65,
  y = 40,
  width = 700,
  height = 30,
  horizontalAlign = "left"
)
d8$yAxis(type = "addPctAxis")
d8$defaultColors(diColors)
d8$setTemplate(afterScript = "
               <script>
               myChart.draw()
               myChart.axes[0].titleShape.text('Donor')
               myChart.axes[1].titleShape.text('Deflated USD (millions)')
               myChart.svg.append('text')
               .attr('x', 40)
               .attr('y', 20)
               .text('Total Disbursements to Humanitarian Purpose Codes (2009-2013)')
               .style('text-anchor','beginning')
               .style('font-size', '100%')
               .style('font-family','sans-serif')
               </script>               
               ")
#d8

##Bubble Matrix (top 10)
top <- pivot3$donorname[1:10]
gBarDat <- pivot2long[which(pivot2long$donorname %in% top),]
leftovers <- pivot2long[which(!pivot2long$donorname %in% top),]
leftovers <- ddply(leftovers,.(purposecode,purposename),summarize,usd_sum=sum(usd_sum,na.rm=TRUE))
leftovers$donorname <- rep("Other Donors")
gBarDat <- rbind(gBarDat,leftovers)
d9 <- dPlot(
  x = "donorname",
  y = "purposename",
  z = "usd_sum",
  data = gBarDat,
  type = "bubble",
  aggregate = "dimple.aggregateMethod.max",
  bounds = list(x = 300, y = 50, height = 240, width = 400)
)
d9$xAxis( type = "addCategoryAxis" )
d9$yAxis( type = "addCategoryAxis" )
d9$zAxis( type = "addMeasureAxis")
d9$defaultColors(diColors)
d9$setTemplate(afterScript = "
               <script>
               myChart.draw()
               myChart.axes[0].titleShape.text('Donor')
               myChart.axes[1].titleShape.text('Deflated USD (millions)')
               myChart.svg.append('text')
               .attr('x', 150)
               .attr('y', 20)
               .text('Total Disbursements to Humanitarian Purpose Codes (2009-2013)')
               .style('text-anchor','beginning')
               .style('font-size', '100%')
               .style('font-family','sans-serif')
               </script>               
               ")
#d9

##Bubble Matrix (top 5)
top <- pivot3$donorname[1:5]
gBarDat <- pivot2long[which(pivot2long$donorname %in% top),]
leftovers <- pivot2long[which(!pivot2long$donorname %in% top),]
leftovers <- ddply(leftovers,.(purposecode,purposename),summarize,usd_sum=sum(usd_sum,na.rm=TRUE))
leftovers$donorname <- rep("Other Donors")
gBarDat <- rbind(gBarDat,leftovers)
d10 <- dPlot(
  x = "donorname",
  y = "purposename",
  z = "usd_sum",
  data = gBarDat,
  type = "bubble",
  aggregate = "dimple.aggregateMethod.max",
  bounds = list(x = 300, y = 50, height = 240, width = 400)
)
d10$xAxis( type = "addCategoryAxis" )
d10$yAxis( type = "addCategoryAxis" )
d10$zAxis( type = "addMeasureAxis")
d10$defaultColors(diColors)
d10$setTemplate(afterScript = "
               <script>
               myChart.draw()
               myChart.axes[0].titleShape.text('Donor')
               myChart.axes[1].titleShape.text('Deflated USD (millions)')
               myChart.svg.append('text')
               .attr('x', 150)
               .attr('y', 20)
               .text('Total Disbursements to Humanitarian Purpose Codes (2009-2013)')
               .style('text-anchor','beginning')
               .style('font-size', '100%')
               .style('font-family','sans-serif')
               </script>               
               ")
#d10

## Heatmap (top 10)
top <- pivot3$donorname[1:10]
gBarDat <- pivot2long[which(pivot2long$donorname %in% top),]
leftovers <- pivot2long[which(!pivot2long$donorname %in% top),]
leftovers <- ddply(leftovers,.(purposecode,purposename),summarize,usd_sum=sum(usd_sum,na.rm=TRUE))
leftovers$donorname <- rep("Other")
gBarDat <- rbind(gBarDat,leftovers)
gBarDat$donorname[which(gBarDat$donorname=="EU Institutions")] <- rep("EU")
gBarDat$donorname[which(gBarDat$donorname=="United Kingdom")] <- rep("UK")
gBarDat$donorname[which(gBarDat$donorname=="United States")] <- rep("US")
d11 <- rPlot(x = 'donorname'
             , y = 'purposename'
             , color = 'usd_sum'
             , data = gBarDat
             , type = 'tile')
d11$set(width = 1000)
d11$set(title = 'Total Disbursements to Humanitarian Purpose Codes (2009-2013)')
d11$guides(color=list(numticks=10,title="USD defl (millions)",scale=list(type="gradient",lower="white",upper=diColors[4]))
           ,y=list(title="Purpose")
           ,x=list(title="Donor",numticks=11))
#d11

## Heatmap (top 5)
top <- pivot3$donorname[1:5]
gBarDat <- pivot2long[which(pivot2long$donorname %in% top),]
leftovers <- pivot2long[which(!pivot2long$donorname %in% top),]
leftovers <- ddply(leftovers,.(purposecode,purposename),summarize,usd_sum=sum(usd_sum,na.rm=TRUE))
leftovers$donorname <- rep("Other Donors")
gBarDat <- rbind(gBarDat,leftovers)
d12 <- rPlot(x = 'donorname'
      , y = 'purposename'
      , color = 'usd_sum'
      , data = gBarDat
      , type = 'tile')
d12$set(width = 1000)
d12$set(title = 'Total Disbursements to Humanitarian Purpose Codes (2009-2013)')
d12$guides(color=list(numticks=10,title="USD defl (millions)",scale=list(type="gradient",lower="white",upper=diColors[4]))
           ,y=list(title="Purpose")
           ,x=list(title="Donor"))
#d12

## Line over time
lineDat <- ddply(dat,.(Year),summarize,total=sum(usd_disbursement_defl,na.rm=TRUE))
d13 <- dPlot(
  total ~ Year,
  data = lineDat,
  type = "line"
)
d13$xAxis(type = "addCategoryAxis", orderRule = "Date")
d13$yAxis(type = "addMeasureAxis")
d13$defaultColors(diColors)
d13$setTemplate(afterScript = "
               <script>
                myChart.draw()
                myChart.axes[0].titleShape.text('Donor')
                myChart.axes[1].titleShape.text('Deflated USD (millions)')
                myChart.svg.append('text')
                .attr('x', 150)
                .attr('y', 20)
                .text('Total Disbursements to Humanitarian Purpose Codes')
                .style('text-anchor','beginning')
                .style('font-size', '100%')
                .style('font-family','sans-serif')
                </script>               
                ")
#d13

## Bar over time
lineDat <- ddply(dat,.(Year),summarize,total=sum(usd_disbursement_defl,na.rm=TRUE))
d14 <- dPlot(
  total ~ Year,
  data = lineDat,
  type = "bar"
)
d14$xAxis(type = "addCategoryAxis", orderRule = "Date")
d14$yAxis(type = "addMeasureAxis")
d14$defaultColors(diColors)
d14$setTemplate(afterScript = "
               <script>
                myChart.draw()
                myChart.axes[0].titleShape.text('Donor')
                myChart.axes[1].titleShape.text('Deflated USD (millions)')
                myChart.svg.append('text')
                .attr('x', 150)
                .attr('y', 20)
                .text('Total Disbursements to Humanitarian Purpose Codes')
                .style('text-anchor','beginning')
                .style('font-size', '100%')
                .style('font-family','sans-serif')
                </script>               
                ")
#d14

## Area over time by donor
top <- pivot3$donorname[1:5]
lineDat <- ddply(dat,.(Year,donorname),summarize,total=sum(usd_disbursement_defl,na.rm=TRUE))
areaDat <- lineDat[which(lineDat$donorname %in% top),]
leftovers <- lineDat[which(!lineDat$donorname %in% top),]
leftovers <- ddply(leftovers,.(Year),summarize,total=sum(total,na.rm=TRUE))
leftovers$donorname <- rep("Other Donors")
areaDat <- rbind(areaDat,leftovers)
d15 <- dPlot(
  total ~ Year,
  data = areaDat,
  groups = "donorname",
  type = "area",
  bounds = list(x = 50, y = 50, height = 300, width = 700)
)
d15$xAxis(type = "addCategoryAxis", orderRule = "Date")
d15$yAxis(type = "addMeasureAxis")
d15$legend(
  x = 60,
  y = 30,
  width = 700,
  height = 30,
  horizontalAlign = "right"
)
d15$defaultColors(diColors)
d15$setTemplate(afterScript = "
               <script>
                myChart.draw()
                myChart.axes[0].titleShape.text('Donor')
                myChart.axes[1].titleShape.text('Deflated USD (millions)')
                myChart.svg.append('text')
                .attr('x', 170)
                .attr('y', 20)
                .text('Total Disbursements to Humanitarian Purpose Codes')
                .style('text-anchor','beginning')
                .style('font-size', '100%')
                .style('font-family','sans-serif')
                </script>               
                ")
#d15

## Stacked bar over time by donor
top <- pivot3$donorname[1:5]
lineDat <- ddply(dat,.(Year,donorname),summarize,total=sum(usd_disbursement_defl,na.rm=TRUE))
areaDat <- lineDat[which(lineDat$donorname %in% top),]
leftovers <- lineDat[which(!lineDat$donorname %in% top),]
leftovers <- ddply(leftovers,.(Year),summarize,total=sum(total,na.rm=TRUE))
leftovers$donorname <- rep("Other Donors")
areaDat <- rbind(areaDat,leftovers)
d16 <- dPlot(
  total ~ Year,
  data = areaDat,
  groups = "donorname",
  type = "bar",
  bounds = list(x = 50, y = 50, height = 300, width = 700)
)
d16$xAxis(type = "addCategoryAxis", orderRule = "Date")
d16$yAxis(type = "addMeasureAxis")
d16$legend(
  x = 60,
  y = 30,
  width = 700,
  height = 30,
  horizontalAlign = "right"
)
d16$defaultColors(diColors)
d16$setTemplate(afterScript = "
               <script>
                myChart.draw()
                myChart.axes[0].titleShape.text('Donor')
                myChart.axes[1].titleShape.text('Deflated USD (millions)')
                myChart.svg.append('text')
                .attr('x', 170)
                .attr('y', 20)
                .text('Total Disbursements to Humanitarian Purpose Codes')
                .style('text-anchor','beginning')
                .style('font-size', '100%')
                .style('font-family','sans-serif')
                </script>               
                ")
#d16

## Stacked bar over time by purposecode
lineDat <- ddply(dat,.(Year,purposecode,purposename),summarize,total=sum(usd_disbursement_defl,na.rm=TRUE))
d17 <- dPlot(
  total ~ Year,
  data = lineDat,
  groups = "purposename",
  type = "bar",
  bounds = list(x = 50, y = 80, height = 280, width = 700)
)
d17$xAxis(type = "addCategoryAxis", orderRule = "Date")
d17$yAxis(type = "addMeasureAxis")
d17$legend(
  x = 150,
  y = 30,
  width = 700,
  height = 30,
  horizontalAlign = "left"
)
d17$defaultColors(diColors)
d17$setTemplate(afterScript = "
               <script>
                myChart.draw()
                myChart.axes[0].titleShape.text('Donor')
                myChart.axes[1].titleShape.text('Deflated USD (millions)')
                myChart.svg.append('text')
                .attr('x', 210)
                .attr('y', 20)
                .text('Total Disbursements to Humanitarian Purpose Codes')
                .style('text-anchor','beginning')
                .style('font-size', '100%')
                .style('font-family','sans-serif')
                </script>               
                ")
#d17

####Export them####
charts <- c(d0
           ,d1
           ,d2
           ,d3
           ,d4
           ,d5
           ,d6
           ,d7
           ,d8
           ,d9
           ,d10
           ,d11
           ,d12
           ,d13
           ,d14
           ,d15
           ,d16
           ,d17)

for(i in 1:length(charts)){
  chart <- charts[[i]]
  chart$save(paste0('//dipr-dc01/home$/AlexM/My Documents/GHA/6.5/chart',i,'.html'), standalone = TRUE)
}
