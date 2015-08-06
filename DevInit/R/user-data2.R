#install.packages('reshape')
#install.packages("utils")
#install.packages("openxlsx")
library(openxlsx)
library(reshape)
library(utils)

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
refMap <- list("domestic"="budget-type,domestic-budget-level,domestic-sources,currency,fiscal-year")
refMap <- c(refMap,"domestic-sectors"="budget-type,domestic-budget-level,domestic-sources,currency,fiscal-year")
refMap <- c(refMap,"domestic-netlending"="budget-type,domestic-budget-level,domestic-sources,currency,fiscal-year")
refMap <- c(refMap,"intl-flows-donors"="flow-type,flow-name")
refMap <- c(refMap,"intl-flows-recipients"="flow-type,flow-name")
refMap <- c(refMap,"largest-intl-flow"="largest-intl-flow")
refMap <- c(refMap,"fragile-states"="fragile-states")
refMap <- c(refMap,"long-term-debt"="debt-flow,destination-institution-type,creditor-type,creditor-institution,financing-type")
refMap <- c(refMap,"oda"="sector,bundle,channel")
refMap <- c(refMap,"oof"="sector,oof-bundle,channel")
refMap <- c(refMap,"fdi-out"="financing-type")
refMap <- c(refMap,"dfis-out-dev"="financing-type")
refMap <- c(refMap,"ssc-out"="financing-type")

#Iterate through files, reading them in
for (i in 1:length(filenames))
{
  #Read Data
  data <- read.csv(filenames[i], header = TRUE,sep=",",na.strings="",check.names=FALSE)
  names <- colnames(data)
  basename = substr(basename(filenames[i]), 1, nchar(basename(filenames[i])) - 4)
  fwd = paste(wd,basename,sep="/")
  
  #Create a folder for each indicator with sub-csv dir
  dir.create(fwd)
  setwd(fwd)
  cwd = paste(fwd,"csv",sep="/")
  dir.create(cwd)
  
  #Create workbook
  wb <- list()
  
  #Fill meta-data sheet/csv
  concept = concepts[which(concepts$id==basename),]
  wideConcept <- t(concept)
  wideConcept <- data.frame(rownames(wideConcept),wideConcept,row.names=NULL) 
  wb <- list(wb,"Metadata"=wideConcept)
  
  write.table(wideConcept,paste0(cwd,"/",basename,"-metaData",".csv"),col.names=FALSE,row.names=FALSE,na="",sep=",")
  
  #Fill notes sheet/csv
  interpolated <- concept$interpolated[1]
  notesList <- c(basename,"")
  baseLength <- length(notesList)
  if(!is.na(interpolated)){
    notesList<-c(
      notesList
      ,"Note: This data contains interpolated values. The interpolated values are typically contained in a column called 'value,' while the uninterpolated values are stored in 'original-value.'"
      ,""
    )
  }
  notesDf <- data.frame(notesList)
  if(length(notesList)>baseLength){
    wb <- list(wb,"Notes"=notesDf)
    write.table(notesDf,paste0(cwd,"/",basename,"-notes",".csv"),col.names=FALSE,row.names=FALSE,na="",sep=",")
  }
  
  
  #Copy the original CSV
  file.copy(filenames[i],paste0(cwd,"/",basename,".csv"))
  wb <- list(wb,"Data"=data)
  
  
  #If we have an ID and a year to widen it by, provide wide
#   if("id" %in% names & "year" %in% names)  {
#     wdata <- reshape(data,idvar="id",timevar="year",direction="wide")
#     write.csv(wdata,paste(basename,"-wide",".csv",sep=""),row.names=FALSE,na="")
#   }
#   
#   #Reference folder
#   rwd = paste(fwd,"reference",sep="/")
#   dir.create(rwd)
#   setwd(rwd)
#   #Copy entity.csv
#   file.copy(paste(refPath,"entity",".csv",sep=""),"entity.csv")
#   #Provide meta-data from concepts.csv
#   #write.csv(concept,paste(basename,"-metadata-wide",".csv",sep=""),row.names=FALSE,na="")
#   write.table(t(concept),paste(basename,"-metadata",".csv",sep=""),col.names=FALSE,na="",sep=",")
#   if(basename %in% names(refMap)){
#     refNames = strsplit(refMap[[basename]],",")
#     for(j in 1:length(refNames)){
#       refBaseName = refNames[[j]]
#       refName = paste(refPath,refBaseName,".csv",sep="")
#       #Copy the reference files
#       file.copy(refName,paste(refBaseName,".csv",sep=""))
#     }
#   }
#addDataFrame(x=wideConcept,sheet=meta,col.names=FALSE,row.names=FALSE,showNA=FALSE)
#addDataFrame(x=notesDf,sheet=notes,col.names=FALSE,row.names=FALSE,showNA=FALSE)
#addDataFrame(x=data,sheet=dataSheet,row.names=FALSE,showNA=FALSE)
  write.xlsx(wb
             ,paste0(basename,".xlsx")
             ,colNames=c(FALSE,FALSE,FALSE)
             ,rowNames=c(FALSE,FALSE,FALSE)
             )
  setwd(wd)
}
#Zip em up
# filenames <- list.files(wd, pattern="/*", full.names=FALSE)
# for(i in 1:length(filenames)){
#   files <- dir(filenames[i],full.names=TRUE)
#   zip(zipfile = filenames[i],files=files)
# }