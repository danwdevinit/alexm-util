#install.packages('reshape')
library(reshape)

wd <- "C:/git/digital-platform/user-data/"
setwd(wd)

#Delete everything in user-data
unlink(dir(wd, full.names = TRUE),recursive=TRUE)

#List all files in country-year
filenames <- list.files("C:/git/digital-platform/country-year/", pattern="*.csv", full.names=TRUE)

#Define references and mapping
refPath = "C:/git/digital-platform/reference/"
conceptPath = "C:/git/digital-platform/concepts.csv"
concepts <- read.csv(conceptPath, header = TRUE,sep=",",na.strings="",check.names=FALSE)
refMap <- list("domestic"="budget-type,domestic-budget-level")
refMap <- c(refMap,"domestic-sectors"="budget-type,domestic-budget-level")
refMap <- c(refMap,"intl-flows-donors"="flow-type,flow-name")
refMap <- c(refMap,"intl-flows-recipients"="flow-type,flow-name")
refMap <- c(refMap,"largest-intl-flow"="largest-intl-flow")
refMap <- c(refMap,"fragile-states"="fragile-states")
refMap <- c(refMap,"long-term-debt"="debt-flow,destination-institution-type,creditor-type,creditor-institution,financing-type")
refMap <- c(refMap,"oda"="sector,bundle,channel")
refMap <- c(refMap,"oof"="sector,oof-bundle,channel")
refMap <- c(refMap,"fdi-out"="financing-type")
refMap <- c(refMap,"dfis-out"="financing-type")

#Iterate through files, reading them in
for (i in 1:length(filenames))
{
  data <- read.csv(filenames[i], header = TRUE,sep=",",na.strings="",check.names=FALSE)
  names <- colnames(data)
  basename = substr(basename(filenames[i]), 1, nchar(basename(filenames[i])) - 4)
  fwd = paste(wd,basename,sep="/")
  #Create a folder for each csv
  dir.create(fwd)
  setwd(fwd)
  concept = concepts[which(concepts$id==basename),]
  #If we have an ID and a year to widen it by, provide wide
  if("id" %in% names & "year" %in% names)  {
    wdata <- reshape(data,idvar="id",timevar="year",direction="wide")
    write.csv(wdata,paste(basename,"-wide",".csv",sep=""),row.names=FALSE,na="")
  }
  #Copy the original CSV
  file.copy(filenames[i],paste(basename,".csv",sep=""))
  #Reference folder
  rwd = paste(fwd,"reference",sep="/")
  dir.create(rwd)
  setwd(rwd)
  #Provide meta-data from concepts.csv
  #write.csv(concept,paste(basename,"-metadata-wide",".csv",sep=""),row.names=FALSE,na="")
  write.table(t(concept),paste(basename,"-metadata",".csv",sep=""),col.names=FALSE,na="",sep=",")
  if(basename %in% names(refMap)){
    refNames = strsplit(refMap[[basename]],",")
    for(j in 1:length(refNames)){
      refBaseName = refNames[[j]]
      refName = paste(refPath,refBaseName,".csv",sep="")
      #Copy the reference files
      file.copy(refName,paste(refBaseName,".csv",sep=""))
    }
  }
  setwd(wd)
}
