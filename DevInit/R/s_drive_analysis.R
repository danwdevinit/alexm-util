#install.packages('tm')
library(tm)
require(XML)

#Setup
wd <- "D:/Documents/Text Dump/"
input <- "S:/Projects"
lastFolder <- tail(strsplit(input,"/")[[1]],1)
wd <- paste0(wd,lastFolder,"/")
dir.create(wd)
setwd(wd)

#List all text files
txts <- list.files(input, pattern="*.txt", full.names=TRUE, recursive=TRUE)
pdfs <- list.files(input, pattern="*.pdf", full.names=TRUE, recursive=TRUE)
docs <- list.files(input, pattern="*.doc", full.names=TRUE, recursive=TRUE)
docxs <- list.files(input, pattern="*.docx", full.names=TRUE, recursive=TRUE)
docs <- setdiff(docs,docxs)

#Copy txts
for (i in 1:length(txts))
{
  filename <- txts[i]
  basename <- basename(filename)
  filetype <- substr(basename,nchar(basename)-3,nchar(basename))
  if(filetype==".txt" & file.size(filename)<50000000){
    file.copy(filename,paste0(wd,basename))
  }
}

#pdfs
#Requires poppler for Windows, if on Windows http://blog.alivate.com.au/poppler-windows/
for (i in 1:length(pdfs))
{
  filename <- pdfs[i]
  basename <- substr(basename(filename), 1, nchar(basename(filename)) - 4)
  system(paste0("pdftotext -layout \"",filename,"\" \"",paste0(wd,basename,".txt"),"\""))
}

#docs
for (i in 1:length(docs))
{
  filename <- docs[i]
  basename <- substr(basename(filename), 1, nchar(basename(filename)) - 4)
  doc <- readDOC()(list('uri'=filename),"en")
  write(doc$content,paste0(wd,basename,".txt"))
}

#docxs
for (i in 1:length(docxs))
{
  filename <- docxs[i]
  basename <- substr(basename(filename), 1, nchar(basename(filename)) - 5)
  unzip(filename, exdir = tempdir()) 
  doc <- xmlParse(file.path(tempdir(), "word", "document.xml") )
  docText <- paste(xpathSApply(doc,"//w:t",xmlValue),collapse=" ")
  write(docText,paste0(wd,basename,".txt"))
}
