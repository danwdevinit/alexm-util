library(rCharts)
library(plyr)

wd <-"C:/Users/alexm/Documents/RWork/GDELT/"
setwd(wd)
colnames <- read.csv("CSV.header.dailyupdates.csv", header = FALSE,
                     sep="\t",colClasses="character")
for(i in 1:60){
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
  data <- read.csv(filename, header = FALSE,
                   sep="\t",colClasses="character",
                   col.names = colnames)
  country_subset <- subset(data,(Actor1CountryCode=="CAR" | Actor2CountryCode=="CAR"))
  if(exists("reports")){
    reports <- rbind(country_subset,reports)
  }
  else{
    reports <- country_subset
  }
}

df <- ddply(reports,.(SQLDATE,EventRootCode)
            ,function(x){
              return(nrow(x))
            })
names(df)[names(df)=="V1"] <- "value"

#Plot
diColors <- c("#ba0c2f" #Red
              ,"#1b365d" #blue
              ,"#ea7600" #Orange
              ,"#93328e" #purple
              ,"#0095c8" #lightblue
              ,"#b7bf10" #Yellow
)
dat <- subset(df,as.numeric(substr(SQLDATE,1,8))>=as.numeric(yesterday))
dat <- ddply(dat,.(SQLDATE),function(x){
  provide_aid <- 0
  other <- 0
  for(i in 1:nrow(x)){
    if(x$EventRootCode[i]=="07"){
      provide_aid <- provide_aid + x$value[i]
    }
#     else if(x$EventRootCode[i]=="19"){
#       military <- military + x$value[i]
#     }
    else{
      other <- other + x$value[i]
    }
  }
  y <- data.frame(provide_aid,other)
  return(y)
})
dat <- transform(dat,percentAid=(provide_aid/(other+provide_aid))*100)
names(dat)[names(dat)=="SQLDATE"] <- "date"
dat <- dat[c("date","percentAid")]
longdf <- reshape(dat
                  ,idvar="date"
                  ,direction="long"
                  ,varying=c("percentAid")
                  ,times = c("Reports on Providing Aid")
                  ,v.names="value"
)
names(longdf)[which(names(longdf)=="time")]<-"type"
d1 <- dPlot(
  x = "date",
  y = "value",
  groups = "type",
  data = longdf,
  type = "bar",
  height = 400,
  width = 600,
  bounds = list(x=60,y=20,width=500,height=300)
)
d1$xAxis(
  type = "addTimeAxis",
  inputFormat = "%Y%m%d",
  outputFormat = "%b %d"
)
d1$legend(
  x = 65,
  y = 40,
  width = 700,
  height = 30,
  horizontalAlign = "left"
)
d1$defaultColors(diColors)
d1$setTemplate(afterScript = "
               <script>
               myChart.draw()
               myChart.axes[0].titleShape.text('Date')
               myChart.axes[1].titleShape.text('Percent of All Reports')
               myChart.svg.append('text')
               .attr('x', 60)
               .attr('y', 15)
               .text('News Reports on CAR over Time')
               .style('text-anchor','beginning')
               .style('font-size', '100%')
               .style('font-family','sans-serif')
               </script>               
               ")
d1
