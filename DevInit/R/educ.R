#List all csvs
filenames <- list.files("C:/git/alexm-util/DevInit/Uganda/Educ/tmp/", pattern="*.csv", full.names=TRUE)

#Iterate through files, reading them in
message(paste0("Reading ",filenames[1],"..."))
data <- read.csv(filenames[1], header = TRUE,sep=",",na.strings="",check.names=FALSE)
for (i in 2:length(filenames))
{
  message(paste0("Reading ",filenames[i],"..."))
  data <- rbind(data,read.csv(filenames[i], header = TRUE,sep=",",na.strings="",check.names=FALSE))
}
