library(plyr)

wd <- "C:/R/"
setwd(wd)
data <- read.csv("fts.csv",header=TRUE,skip=5,na.strings="",as.is=TRUE)

#Parse numbers (removing commas)
data <- transform(data,USD.committed.contributed=as.numeric(gsub(",","", USD.committed.contributed)))
data <- transform(data,Original.currency.amount=as.numeric(gsub(",","", Original.currency.amount)))
data <- transform(data,USD.pledged=as.numeric(gsub(",","", USD.pledged)))
data <- transform(data,Project.current.request=as.numeric(gsub(",","", Project.current.request)))
data <- transform(data,Item.ID=as.numeric(gsub(",","", Item.ID)))

#Remove total row
data <- subset(data,Donor!="Total:")

#First define the thesaurus as a list of org aliases
thesaurus = list(
  "Caritas" = c(
    "Caritas"
    ,"Caritas Dev Kalemie"
    ,"Caritas Haiti"
    ,"Caritas Kananga"
    ,"Caritas Germany (DCV)"
    ,"Caritas Chad"
    ,"Caritas Développement Niger"
    )
  ,"World Vision" = c(
    "World Vision"
    ,"World Vision Kenya"
    ,"World Vision South Sudan"
    ,"World Vision Sudan")
)
#Then define the lookup function
thesaurusLookup <- function(x){
  #Start with a missing result (so later we can check if we found one)
  result <- NA
  #Loop through the thesaurus
  for(i in 1:length(thesaurus)){
    #If X (our string) is within the list at this index
    if(x %in% thesaurus[[i]]){
      #Make result = the name of that index
      result <- names(thesaurus)[i]
    }
  }
  #If we didn't find a result, return the original string
  if(is.na(result)){
    return(x)
  }
  #Otherwise, return the result
  else{
    return(result)
  }
}
#A test (should return "Caritas")
thesaurusLookup("Caritas Dev Kalemie")

#The transformation
data$Appealing.org <- sapply(data$Appealing.agency.top.org.,thesaurusLookup)

#see the difference
setdiff(data$Appealing.org,data)
