library(Hmisc)
library(data.table)

setwd("D:/Documents/Data/DHSmeta")

cwi <- read.csv("global_cwi_replication.csv",na.strings="",as.is=TRUE)

cwi$weights <- cwi$sample.weights/1000000
cwi$weighted.cwi <- cwi$cwi*cwi$weights

quints <- quantile(cwi$weighted.cwi, prob = seq(0, 1, length = 6), type = 5,na.rm=TRUE)

for(i in 2:length(quints)){
  quint <- quints[i]
  quintName <- paste0("quint.",(i-1)*20)
  cwi[[quintName]] <- (cwi$weighted.cwi <= quint)
}

decs <- quantile(cwi$weighted.cwi, prob = seq(0, 1, length = 11), type = 5,na.rm=TRUE)
cwi$dec.50 <- (cwi$weighted.cwi <= decs[6])

cwi.table <- data.table(cwi)

cwi.collapse <- cwi.table[
  ,.(p20=mean(quint.20,na.rm=TRUE)
     ,years=paste(unique(year),collapse="; ")
     ,iso2=max(iso2))
  , by=.(filename)]

setnames(cwi.collapse,"iso2","iso2c")

cwi.collapse[which(cwi.collapse$iso2c=="LB")]$iso2c <- "LR"

write.csv(cwi.collapse,"cwi_headcounts.csv")