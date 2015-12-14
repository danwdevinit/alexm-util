wd <- "D:/Documents/Text Dump"
setwd(wd)

#List all text files
pdfs <- list.files("S:/", pattern="*.pdf", full.names=TRUE, recursive=TRUE)
txts <- list.files("S:/", pattern="*.txt", full.names=TRUE, recursive=TRUE)
docs <- list.files("S:/", pattern="*.doc", full.names=TRUE, recursive=TRUE)
docxs <- list.files("S:/", pattern="*.docx", full.names=TRUE, recursive=TRUE)