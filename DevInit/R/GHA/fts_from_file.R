wd <- "D:/Documents/"
setwd(wd)
data <- read.csv("fts.csv",header=TRUE,skip=5,na.strings="",as.is=TRUE)

#Parse numbers (removing commas)
data <- transform(data,USD.committed.contributed=as.numeric(gsub(",","", USD.committed.contributed)))
data <- transform(data,Original.currency.amount=as.numeric(gsub(",","", Original.currency.amount)))
data <- transform(data,USD.pledged=as.numeric(gsub(",","", USD.pledged)))
data <- transform(data,Project.current.request=as.numeric(gsub(",","", Project.current.request)))
data <- transform(data,Item.ID=as.numeric(gsub(",","", Item.ID)))

#Sample transform to new column
data <- transform(data,millionsContributed=USD.committed.contributed/1000000)

#construct a fake deflator with donor, year, and a random value
donorList <- unique(data$Donor)
fakeDeflator <- runif(length(donorList))
fakeDeflatorYear <- rep(2016)
deflator <- data.frame(donorList,fakeDeflatorYear,fakeDeflator)
#Make deflator names match data
names(deflator) <- c("Donor","Appeal.year","deflator")
#Add deflator to dataset by matching unique combinations of donor and appeal year
data <- merge(
  data
  ,deflator
  ,by=c("Donor","Appeal.year")
  ,all.x=TRUE
  )
#Create deflated value by multiplying contributed*deflator
data <- transform(data,contributedDeflated=USD.committed.contributed*deflator)

#Example String transformations
data <- transform(data,USAisDONOR = (Donor=="United States of America") )

