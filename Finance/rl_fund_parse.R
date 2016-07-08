library(data.table)

wd <- "D:/Documents/Personal/RL_Pensions/"

setwd(wd)

# pdfs <- list.files(wd,pattern="*.pdf",full.names=TRUE)
# 
# #pdfs
# #Requires poppler for Windows, if on Windows http://blog.alivate.com.au/poppler-windows/
# for (i in 1:length(pdfs))
# {
#   filename <- pdfs[i]
#   basename <- substr(basename(filename), 1, nchar(basename(filename)) - 4)
#   fundname <- gsub("_"," ",substr(basename,1,nchar(basename)-33))
#   filetype <- substr(basename(filename),nchar(basename(filename))-3,nchar(basename(filename)))
#   if(filetype==".pdf"){
#     system(paste0("pdftotext -layout \"",filename,"\" \"",paste0(wd,fundname,".txt"),"\""))
#   }
# }

txts <- list.files(wd,pattern="*.txt",full.names=TRUE)

data <- read.csv("table.csv")

queries <- c(
  "Management style:"
  ,"Launch date:"
  ,"Fund size:"
  ,"ABI Sector:"
  ,"Benchmark:"
  ,"Manager:"
  ,"Fund Management Charge:"
  ,"Investment Expenses:"
  ,"Total Expense Ratio:"
  )

for(i in 1:length(queries)){
  data[[queries[i]]] <- NA
}

for(i in 1:length(txts)){
  txt <- txts[i]
  basename <- substr(basename(txt), 1, nchar(basename(txt)) - 4)
  fundname <- gsub("-","/",substr(basename,14,nchar(basename)))
  message(fundname)
  text <- readLines(txt)
  grepResults <- sapply(queries,grep,x=text)
  for(j in 1:length(grepResults)){
    query <- queries[j]
    grepResult <- grepResults[[j]]
    if(length(grepResult)>0){
      match <- trimws(strsplit(text[grepResult],":")[[1]][2])
    }else{
      match <- ""
    }
    data[which(data$Fund.Name==fundname),query] <- match
  }
}

write.csv(data,"final_data.csv",row.names=FALSE,na="")
