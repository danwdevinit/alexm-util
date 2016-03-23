# install.packages("equivalence")
library(equivalence)

fake_data <- read.csv("~/git/torch-rnn/output/district-rev-100450.csv",na.strings="",fill=TRUE,header=FALSE,skip=1)
real_data <- read.csv("/home/alex/District_rev_exp250615.csv")
names(fake_data) <- names(real_data)
#Parse numbers (removing commas)
fake_data <- transform(fake_data,Value=as.numeric(gsub(",","",Value)))
real_data <- transform(real_data,Value=as.numeric(gsub(",","",Value)))
fake_data <- transform(fake_data,logValue=log(Value))
real_data <- transform(real_data,logValue=log(Value))
# Remove missing and subset
fake_data <- fake_data[which(complete.cases(fake_data$Value)),]
# fake_data <- fake_data[which(is.finite(fake_data$logValue)),]
real_data <- real_data[sample(1:nrow(real_data),nrow(fake_data),replace=FALSE),]

# hist(real_data$logValue)
# hist(fake_data$logValue)

# TOST
a <- tost(fake_data$Value,real_data$Value,paired=TRUE)
# a
a$result
a$tost.p.value
