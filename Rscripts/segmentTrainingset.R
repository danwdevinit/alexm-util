setwd('~/Documents')
data <- read.csv('org_train.csv', row.names=NULL, header=TRUE, sep='\t',stringsAsFactors=FALSE)
dir.create("~/tmp")
setwd("~/tmp/")
dir.create("~/tmp/AidData-work")
setwd("~/tmp/AidData-work/")
dir.create("~/tmp/AidData-work/all")
setwd("~/tmp/AidData-work/all")
text <- paste(data$title,data$short,data$long,sep=' ')
df <- data.frame(text,data$act_code,stringsAsFactors=FALSE)
codes <- data.frame(unique(data$act_code))

for(i in 1:20){
  dir.create(paste("~/tmp/AidData-work/all/",codes[i,1],sep=""))
  setwd(paste("~/tmp/AidData-work/all/",codes[i,1],sep=""))
  matchingdf <- df[which(df$data.act_code==codes[i,1]),]
  for(j in 1:nrow(matchingdf)){
    cat(matchingdf$text[j],file=paste(j,".txt",sep=""))
  }
  print(codes[i,1])
  setwd("~/tmp/AidData-work/all")
}