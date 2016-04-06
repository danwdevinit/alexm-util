#install.packages("ggplot2")
#install.packages("dplyr")
library(ggplot2)
library(plyr)
library(scales)

input <- "urbn"
percentMax <- 40

windows <- TRUE
if(windows){pathpre<-"C:"}else{pathpre<-"~"}
wd <- paste0(pathpre,"/git/alexm-util/Finance/")
setwd(wd)
now <- Sys.Date()
dateFolder <- paste0("data/",input,"_",now,"/")
dir.create(dateFolder)

infilename <- paste0("data/",input,".csv")
raw <- read.csv(
  infilename
  ,header=FALSE
  ,skip=7
  ,as.is=TRUE
)
splitdate <- function(x){
  return(gsub("___","_",gsub("[ ()]","_",strsplit(x,"100")[[1]][1]),fixed=TRUE))
}
dates <- unique(raw[,1])
dates <- subset(dates,dates!="")
dates <- sapply(dates,splitdate)

setClass("myDate")
setAs("character","myDate", function(from) as.Date(from, format="%m/%d/%y"))

if(exists("panelAverages")){rm(panelAverages)}
for(i in 1:length(dates)){
  outfilename <- paste0(dateFolder,dates[i],input,".csv")
  command <- paste("node ha_tos2.js",infilename,(i-1),outfilename,percentMax)
  system(command)
  df <- read.csv(outfilename
                 ,header=TRUE
                 ,colClasses=c("myDate","numeric","character","numeric","numeric","numeric"))
  if(nrow(df)>0){
    if(!exists("panelAverages")){
      panelAverages <- ddply(df,.(date,state,strike),summarize,avg=mean(prob),sd=sd(prob),count=length(prob))
      
    }else{
      panelAverage <- ddply(df,.(date,state,strike),summarize,avg=mean(prob),sd=sd(prob),count=length(prob))
      panelAverages <- rbind(panelAverages,panelAverage)
    }
    timestamp <- paste0(
      "\nExpiry: "
      ,df[1,1]
      ,"\nCalculated: "
      ,Sys.Date()
    )
    
    change_averages <- ddply(df,.(change,state),summarize,avg=mean(prob))
    title <- paste0(
      "Probability of up/neutral/down given percent change",
      timestamp
    )
    p1 <- ggplot(data=change_averages,aes(x=change,y=avg,group=state,colour=state)) +
      geom_line() +
      geom_point() +
      ggtitle(title)
    ggsave(paste0(dateFolder,"p1-",df[1,1],".jpg"),plot=p1,height=7,width=14)
    
    strike_averages <- ddply(df,.(strike,state),summarize,avg=mean(prob),sd=sd(prob),count=length(prob))
    title <- paste0(
      "Probability of up/neutral/down given strike",
      timestamp
    )
    p2 <- ggplot(data=strike_averages,aes(x=strike,y=avg,ymax=avg+sd,ymin=avg-sd,group=state,colour=state)) +
      geom_point(size=3) +
      geom_errorbar(width=0.2) +
      ggtitle(title)
    ggsave(paste0(dateFolder,"p2-",df[1,1],".jpg"),plot=p2,height=7,width=14)
    
    #Density
    jpeg(
      file=paste0(dateFolder,"p3-",df[1,1],".jpg")
      ,width=14
      ,height=7
      ,res=300
      ,units="in"
    )
    par(mfrow=c(3, 1))
    states <- unique(df$state)
    for (i in 1:length(states)) {
      d <- density(df[which(df$state==states[i]),]$prob)
      plot(d, type="n", main=paste("Kernal density",states[i],"states",timestamp))
      polygon(d,  border="#ba0c2f", col="#ba0c2f")
    }
    dev.off()
    par(mfrow=c(1, 1))
  }
}
underlying <- 32.47
percent <- 0.0286*2
sub <- subset(panelAverages,strike>=(underlying*(1-percent)) & strike<=(underlying*(1+percent)))
#sub <- panelAverages
limited_averages <- ddply(sub,.(date,state),summarize,avg=mean(avg),sd=mean(sd),count=sum(count))
p3 <- ggplot(data=limited_averages,aes(x=date,y=avg,ymax=avg+sd,ymin=avg-sd,group=state,colour=state)) +
#   scale_x_date(date_breaks = "2 months") +
  geom_line(size=1) +
  geom_point(size=3) +
  geom_errorbar(width=0.2)
ggsave(paste0(dateFolder,"p4.jpg"),plot=p3,height=7,width=14)
