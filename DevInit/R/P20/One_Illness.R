setwd("D:/Documents/P20/Krishna")
library(Hmisc)
library(ggplot2)
library(plyr)

kham <- read.csv("kham.csv",check.names=FALSE,na.strings=c("","."))
kham$loc <- "kham"
goda <- read.csv("goda.csv",check.names=FALSE,na.strings=c("","."))
goda$loc <- "goda"
nalg <- read.csv("nalg.csv",check.names=FALSE,na.strings=c("","."))
nalg$loc <- "nalg"

df <- rbind(kham,goda,nalg)
df$loc <- as.factor(df$loc)
summary(glm(CAT~Age+SEX+loc,data=df,family="binomial"))
summary(glm(CAT~Age+loc,data=df,family="binomial"))
summary(lm(SG_B~Age+SEX+loc,data=df))
summary(lm(SG_B~Age+loc,data=df))
#Age has a positive effect on your starting stage
summary(lm(SG_A~Age+SEX+loc,data=df))
summary(lm(SG_A~Age+loc,data=df))
#But age has no effect on your ending stage

df <- transform(df,SG_D = SG_A-SG_B)
summary(lm(SG_D~Age+SEX+loc,data=df))
summary(lm(SG_D~Age+loc,data=df))
#This is a good finding^^^
#Age coef negative and sig on difference in stages of progress
#So older people may have more stages of progress at the start of the survey
#But they are also more likely to move down the ladder
summary(lm(SG_D~Age,data=df))
plot(SG_D~SEX,data=df)

describe(df$SEX)
describe(df$Age)
hist(df$Age,main="Histogram of respondent's age from Krishna Surveys",xlab="Age",col="#BA0C2F",prob=TRUE)
gender <- ddply(df,.(SEX),summarize,count=length(S_NO))
barplot(gender$count,main="Respondent's gender from Krishna Surveys",ylab="Count",col="#BA0C2F",names.arg=c("Female","Male"))

# p1 <- ggplot(data=df,aes(x=Age,y=SG_D)) +
#   stat_binhex(bins=15)+
#   xlim(c(0,100)) +
#   ylim(c(-10,10))
# p1

# Todo: Codification of whether a cause is negative or positive...
# See if age or gender causes more or less pos/neg life experiences
