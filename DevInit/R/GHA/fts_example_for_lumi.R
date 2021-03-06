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

thesaurus = list(
  "Caritas" = c("Caritas","Caritas Dev Kalemie","Caritas Haiti","Caritas Kananga"),
  "World Vision" = c("World Vision","World Vision Kenya","World Vision South Sudan","World Vision Sudan")
)
thesaurusLookup <- function(x){
  result <- NA
  for(i in 1:length(thesaurus)){
    if(x %in% thesaurus[[i]]){
      result <- names(thesaurus)[i]
    }
  }
  if(is.na(result)){
    return(x)
  }
  else{
    return(result)
  }
}

for(i in 1:length(categories)){
  categoryWords <- categories[[i]]
  category <- names(categories[i])
  data$temp <- FALSE
  for(j in 1:length(categoryWords)){
    word <- categoryWords[j]
    message(word)
    data <- transform(data,temp=(temp|grepl(word,Description)|grepl(word,Project.title)))
  }
  data[category] <- data$temp
  data$temp <- NULL
}