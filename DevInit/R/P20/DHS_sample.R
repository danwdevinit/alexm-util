#install.packages('foreign')
#install.packages('Hmisc')
library(foreign)
library(Hmisc)

parseLabelDo <- function(df,file){
  text <- readLines(file)
  for(i in 1:length(text)){
    line <- text[i]
    if(substr(line,1,5)=="label"){
      quoteIndex <- regexpr("\"",line)
      label <- gsub("\"","",substr(line,quoteIndex,nchar(line)))
      varname <- substr(line,16,quoteIndex-1)
      varname <- gsub("_",".",varname,fixed=TRUE)
      varname <- gsub(" ","",varname,fixed=TRUE)
      # command <- paste0("label(df$",varname,") = \"",label,"\"")
      command <- paste0("names(df)[which(names(df)=='",varname,"')] = \"",label,"\"")
      if(varname %in% names(df)){
        eval(parse(text=command)) 
      }
    }
  }
  return(df)
}

setwd("C:/Users/alexm/Downloads/ZZHR62DT")
df <- read.dta("ZZHR62FL.dta",convert.underscore=TRUE)
df <- df[, colSums(!is.na(df)) != 0]
df <- parseLabelDo(df,"ZZHR62FL.do")
write.csv(df,"labeledData.csv",row.names=FALSE,na="")
