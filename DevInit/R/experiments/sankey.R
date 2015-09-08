setwd("C:/git/rCharts_d3_sankey")
dp <- "C:/git/digital-platform/country-year/"
require(rCharts)
require(rjson)
require(igraph)

#get source from original example
#this is a JSON, so will need to translate
#this is complicated and unnecessary but feel I need to replicate
#for completeness

#expect most data to come straight from R
#in form of source, target, value

links <- matrix(unlist(
  rjson::fromJSON(
    file = "http://bost.ocks.org/mike/sankey/energy.json"
  )$links
),ncol = 3, byrow = TRUE)

nodes <- unlist(
  rjson::fromJSON(
    file = "http://bost.ocks.org/mike/sankey/energy.json"
  )$nodes
)

#convert to data.frame so souce and target can be character and value numeric
links <- data.frame(links)
colnames(links) <- c("source", "target", "value")
links$source <- sapply(links$source, FUN = function(x) {return(as.character(nodes[x+1]))}) #x+1 since js starts at 0
links$target <- sapply(links$target, FUN = function(x) {return(nodes[x+1])}) #x+1 since js starts at 0


#now we finally have the data in the form we need
sankeyPlot <- rCharts$new()
sankeyPlot$setLib('.')
sankeyPlot$setTemplate(script = "layouts/chart.html")

sankeyPlot$set(
  data = links,
  nodeWidth = 15,
  nodePadding = 10,
  layout = 32,
  width = 960,
  height = 500,
  units = "TWh",
  title = "Sankey Diagram"
)

sankeyPlot

###ODA by bundle####
library(plyr)
oda <- read.csv(paste0(dp,"oda.csv"),na.strings="",stringsAsFactors=FALSE)
cutpoint <- 10
uk2013 <- subset(oda,id.from=="GB" & year==2013)
topRecips <- ddply(uk2013,.(id.to),summarize,value=sum(value,na.rm=TRUE))
topRecips <- topRecips[order(-topRecips$value),]
topCut <- topRecips[1:cutpoint,]$id.to
topTrans <- function(Xvector,cut){
  results <- character(length(Xvector))
  for(i in 1:length(Xvector)){
    x <- Xvector[i]
    if(x %in% cut){
      results[i] <- x
    }else{
      results[i] <- "Other"
    }
  }
  return(results)
}
uk2013 <- transform(uk2013,id.to=topTrans(id.to,topCut))
uk2013 <- ddply(uk2013,.(id.to,bundle),summarize,value=sum(value,na.rm=TRUE))
names(uk2013) <- c("source","target","value")

#now we finally have the data in the form we need
sankeyPlot <- rCharts$new()
sankeyPlot$setLib('.')
sankeyPlot$setTemplate(script = "layouts/chart.html")

sankeyPlot$set(
  data = uk2013,
  nodeWidth = 15,
  nodePadding = 10,
  layout = 32,
  width = 1800,
  height = 800,
  units = "USD",
  title = "Sankey Diagram"
)

sankeyPlot

###ODA by sector####
cutpoint <- 10
uk2013 <- subset(oda,id.from=="GB" & year==2013)
topRecips <- ddply(uk2013,.(id.to),summarize,value=sum(value,na.rm=TRUE))
topRecips <- topRecips[order(-topRecips$value),]
topCut <- topRecips[1:cutpoint,]$id.to
topTrans <- function(Xvector,cut){
  results <- character(length(Xvector))
  for(i in 1:length(Xvector)){
    x <- Xvector[i]
    if(x %in% cut){
      results[i] <- x
    }else{
      results[i] <- "Other"
    }
  }
  return(results)
}
uk2013 <- transform(uk2013,id.to=topTrans(id.to,topCut))
uk2013 <- ddply(uk2013,.(id.to,sector),summarize,value=sum(value,na.rm=TRUE))
names(uk2013) <- c("source","target","value")

#now we finally have the data in the form we need
sankeyPlot2 <- rCharts$new()
sankeyPlot2$setLib('.')
sankeyPlot2$setTemplate(script = "layouts/chart.html")

sankeyPlot2$set(
  data = uk2013,
  nodeWidth = 15,
  nodePadding = 10,
  layout = 32,
  width = 1800,
  height = 800,
  units = "USD",
  title = "Sankey Diagram"
)

sankeyPlot2

###ODA####
cutpoint <- 10
uk2013 <- subset(oda,id.from=="GB" & year==2013)
topRecips <- ddply(uk2013,.(id.to),summarize,value=sum(value,na.rm=TRUE))
topRecips <- topRecips[order(-topRecips$value),]
topCut <- topRecips[1:cutpoint,]$id.to
topTrans <- function(Xvector,cut){
  results <- character(length(Xvector))
  for(i in 1:length(Xvector)){
    x <- Xvector[i]
    if(x %in% cut){
      results[i] <- x
    }else{
      results[i] <- "Other"
    }
  }
  return(results)
}
#uk2013 <- transform(uk2013,id.to=topTrans(id.to,topCut))
uk2013 <- subset(uk2013,id.to %in% topCut)
uk2013recip <- ddply(uk2013,.(id.from,id.to),summarize,value=sum(value,na.rm=TRUE))
uk2013bundle <- ddply(uk2013,.(id.to,bundle),summarize,value=sum(value,na.rm=TRUE))
uk2013sector <- ddply(uk2013,.(bundle,sector),summarize,value=sum(value,na.rm=TRUE))
names(uk2013recip) <- c("source","target","value")
names(uk2013bundle) <- c("source","target","value")
names(uk2013sector) <- c("source","target","value")
uk2013full <- rbind(uk2013recip,uk2013bundle,uk2013sector)

#now we finally have the data in the form we need
sankeyPlot3 <- rCharts$new()
sankeyPlot3$setLib('.')
sankeyPlot3$setTemplate(script = "layouts/chart.html")

sankeyPlot3$set(
  data = uk2013full,
  nodeWidth = 15,
  nodePadding = 10,
  layout = 32,
  width = 1800,
  height = 800,
  units = "USD",
  title = "Sankey Diagram"
)

sankeyPlot3
