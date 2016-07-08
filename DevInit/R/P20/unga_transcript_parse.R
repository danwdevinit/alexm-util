library(data.table)

wd <- "D:/Documents/Data/UNGA 2015 Transcripts/"

setwd(wd)

# pdfs <- list.files(wd,pattern="*.pdf",full.names=TRUE)
# 
# #pdfs
# #Requires poppler for Windows, if on Windows http://blog.alivate.com.au/poppler-windows/
# for (i in 1:length(pdfs))
# {
#   filename <- pdfs[i]
#   basename <- substr(basename(filename), 1, nchar(basename(filename)) - 4)
#   filetype <- substr(basename(filename),nchar(basename(filename))-3,nchar(basename(filename)))
#   if(filetype==".pdf"){
#     system(paste0("pdftotext -layout \"",filename,"\" \"",paste0(wd,basename,".txt"),"\""))
#   }
# }

txts <- list.files(wd,pattern="*.txt",full.names=TRUE)

queries <- c(
  "global goals","SDGs","leave no one behind","agenda 2030","post 2015 agenda","ending poverty"
  )

dataList <- list()
dataIndex <- 1

for(i in 1:length(txts)){
  txt <- txts[i]
  basename <- substr(basename(txt), 1, nchar(basename(txt)) - 4)
  message(basename)
  text <- readLines(txt)
  grepResults <- sapply(queries,grep,x=text,ignore.case=TRUE)
  this.list <- list(
    "speaker"= basename
    )
  for(j in 1:length(grepResults)){
    query <- queries[j]
    grepResult <- grepResults[[j]]
    if(length(grepResult)>0){
      matches <- paste(text[grepResult],collapse="; ")
    }else{
      matches <- ""
    }
    this.list[[query]] <- matches
  }
  dataList[[dataIndex]] <- data.frame(this.list)
  dataIndex <- dataIndex + 1
}

data <- rbindlist(dataList)

write.csv(data,"search_term_matches.csv",row.names=FALSE,na="")
