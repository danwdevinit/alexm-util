#install.packages('ggplot2')
#install.packages('scales')
library(ggplot2)
library(scales)


setwd("C:/git/alexm-util/DevInit/R/tmp")
filenames <- list.files("C:/git/digital-platform/latest-year", pattern="*.csv", full.names=TRUE)

len = length(filenames)
filename = character(len)
normW = numeric(len)
logW = numeric(len)
normality = numeric(len)
for (i in 1:len)
{
  graph <- read.csv(filenames[i], header = TRUE,sep=",",check.names=FALSE)
  graph <- graph[complete.cases(graph),]
  logGraph <- graph
  logGraph$value <- log(graph$value)
  data <- graph$value
  log <- log(data)
  #atitle <- basename(filenames[i])
  #a <- ggplot(graph, aes(value) ) +
  #  geom_histogram(aes( y = ..count..))+
  #  xlab("value") +
  #  ylab("Count") +
  #  ggtitle(atitle)
  #b <- ggplot(logGraph, aes(value) ) +
  #  geom_histogram(aes( y = ..count..))+
  #  xlab("value") +
  #  ylab("Count") +
  #  ggtitle(atitle)
  #ggsave(a, file = paste(basename(filenames[i]),".jpg",sep=""),dpi = 1000)
  #ggsave(b, file = paste(basename(filenames[i]),"-log.jpg",sep=""),dpi = 1000)
  s <- shapiro.test(data)
  filename[i] <- basename(filenames[i])
  normW[i] <- s$statistic
  l <- shapiro.test(log)
  logW[i] <- l$statistic
  normality[i] <- s$statistic - l$statistic
}
df <- data.frame(filename,normW,logW,normality,stringsAsFactors=FALSE)
write.csv(df,"distributions.csv",row.names=FALSE,na="")