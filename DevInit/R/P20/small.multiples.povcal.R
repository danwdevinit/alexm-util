library(plyr)
library(data.table)
library(ggplot2)

wd <- "D:/Documents/Data/PovCal/text"
setwd(wd)

mpi <- read.csv("D:/Documents/P20_small_wealth_multiples/mpi.csv",as.is=TRUE)

gdpList <- list()
gdpIndex <- 1

txts <- list.files(wd,pattern="*.txt")
for(i in 1:length(txts)){
  filename <- txts[i]
  basename <- substr(filename, 1, nchar(filename) - 4)
  split <- strsplit(basename,"_")[[1]]
  iso3 <- split[1]
  povline <- split[4]
  if(povline=="2.38"){
    text <- readLines(filename)
    grepResults <- grep("Data mean in PPP$:",text,fixed=TRUE)
    grepResultsAlt <- grep("Mean: ",text,fixed=TRUE)
    if(length(grepResults)>0){
      datum <- as.numeric(trimws(strsplit(text[grepResults],":")[[1]][2]))
      df <- data.frame(iso3=iso3,gdpmean=datum)
      gdpList[[gdpIndex]] <- df
      gdpIndex <- gdpIndex + 1
    }else if(length(grepResultsAlt)>0){
      datum <- as.numeric(trimws(strsplit(text[grepResultsAlt[length(grepResultsAlt)]],":")[[1]][2]))
      df <- data.frame(iso3=iso3,gdpmean=datum)
      gdpList[[gdpIndex]] <- df
      gdpIndex <- gdpIndex + 1
    }
  }
}

gdp <- rbindlist(gdpList)

wd <- "D:/Documents/Data"
setwd(wd)

data <- read.csv("total_dist.csv",as.is=TRUE)

names(data)[which(names(data)=="iso")] <- "iso3"

wide <- subset(data,type==3 & pov.line==2.38)
wide <- wide[c(1,7,10:209)]
wide <- wide[complete.cases(wide[c(1:2)]),]

long <- reshape(wide,varying=c(3:202),idvar=c(1:2),v.names=c("P","L"),direction="long")
long <- long[order(long$iso3),]
long <- long[complete.cases(long),]

long <- join(long,gdp,by="iso3")
long$gdpperdiem <- long$gdpmean/30.42
long$P <- long$P*long$gdpperdiem

# save(long,file="D:/Documents/P20_small_wealth_multiples/povcaldata.RData")
# 
# load("D:/Documents/P20_small_wealth_multiples/povcaldata.RData")

wd <- "D:/Documents/Data"
setwd(wd)

iso3s <- unique(long$iso3)


dataList <- list()
dataIndex <- 1

for(i in 1:length(iso3s)){
  data <- subset(long,iso3==iso3s[i])
  iso3 <- iso3s[i]
  hc <- unique(data$povheadcount)
  y.prime <- diff(data$P)/diff(data$L)  
  y.prime <- quantile(y.prime,probs=seq(0,1,length=1001),na.rm=TRUE)
  y.prime <- y.prime-2.38
  incomes <- data.frame(y.prime)
  names(incomes) <- c("income")
  
  pic.file <- paste0("D:/Documents/P20_small_wealth_multiples/individual_povcal/",iso3,".jpg")
  d <- ggplot(incomes,aes(x=income)) + geom_density(aes(fill=1,colour=1),alpha=0.3)
  
  this.mpi <- subset(mpi,iso==iso3)$hc
  
  if(length(this.mpi)>0){
    vline.cut <- incomes[(this.mpi*10)+1,]
    d <- d + geom_vline(xintercept=vline.cut)
  }else{
    vline.cut <- NA
  }
  d <- d + theme_bw() + theme(legend.position="none") + labs(title=iso3,x="Adj. income",y="Density")
  ggsave(filename=pic.file,plot=d,height=5,width=8,units="in")
  
  incomes$iso3 <- iso3
  incomes$hc <- hc
  incomes$vline.cut <- vline.cut
  dataList[[dataIndex]] <- incomes
  dataIndex <- dataIndex + 1
  #   spl <- smooth.spline(data$L, y=data$P)
  #   pred <- predict(spl)
  #   plot(data$L,data$P)
  #   lines(pred,col=2)
#   pred.prime <- predict(spl,deriv=1)
#   plot(pred.prime$y~pred.prime$x)
}

all.data <- rbindlist(dataList)
all.data <- subset(all.data,hc>0.01 & iso3!="TTO" & iso3!="DNK")
# blank <- data.frame(c(NA,NA,NA,NA,NA,NA,NA,NA),c("",""," "," ","  ","  ","   ","   "),c(NA,NA,NA,NA,NA,NA,NA,NA))
# names(blank) <- names(all.data)
all.plot.alpha <- ggplot(all.data,aes(x=income)) + geom_density(aes(fill=1,colour=1),alpha=0.3) + geom_vline(aes(xintercept=vline.cut)) + xlim(-3,3) + facet_wrap(~iso3,ncol=5) + theme_bw() + theme(legend.position="none") + labs(x="Adj. income",y="Density")
all.data$hc[which(all.data$hc>1)] <- all.data$hc[which(all.data$hc>1)]/100
all.data <- all.data[order(-all.data$hc),]
all.data$iso3 <- factor(all.data$iso3,levels=unique(all.data$iso3))
all.plot <- ggplot(all.data,aes(x=income)) + geom_density(aes(fill=1,colour=1),alpha=0.3) + geom_vline(aes(xintercept=vline.cut)) + xlim(-3,3) + facet_wrap(~iso3,ncol=5) + theme_bw() + theme(legend.position="none") + labs(x="Adj. income",y="Density")

ggsave(filename="D:/Documents/P20_small_wealth_multiples/aggregate/all.povcal.pdf",plot=all.plot,width=8,height=30,units="in",limitsize=FALSE)
ggsave(filename="D:/Documents/P20_small_wealth_multiples/aggregate/all.povcal.alpha.pdf",plot=all.plot.alpha,width=8,height=30,units="in",limitsize=FALSE)

