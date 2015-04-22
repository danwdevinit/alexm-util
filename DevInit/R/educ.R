#List all csvs
filenames <- list.files("C:/git/alexm-util/DevInit/Uganda/Educ/tmp/", pattern="*.csv", full.names=TRUE)

#Iterate through files, reading them in
data <- read.csv(filenames[1], header = TRUE,sep=",",na.strings="",check.names=FALSE)
for (i in 2:length(filenames))
{
  data <- rbind(data,read.csv(filenames[i], header = TRUE,sep=",",na.strings="",check.names=FALSE))
}
