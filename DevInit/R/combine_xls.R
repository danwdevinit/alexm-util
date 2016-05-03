library(gdata)
library(data.table)

wd <- "D:/Documents/Data/WB_GHA/"
setwd(wd)

vocab <- c("emergency")
setNames <- c("Date","Financier","Transaction type","Amount USD","Project ID")

for(i in 1:length(vocab)){
  data <- list()
  dataIndex <- 1
  word <- vocab[i]
  message(word)
  newwd <- paste0(wd,word)
  setwd(newwd)
  
  filenames <- list.files(newwd, pattern="*.xls", full.names=TRUE)
  
  for(j in 1:length(filenames)){
    message(paste(j,"/",length(filenames)))
    filename <- filenames[j]
    if(file.size(filename)>0){
      basename <- substr(basename(filename), 1, nchar(basename(filename)) - 4)
      df <- read.xls(filename)
      df$PROJECT_ID <- basename
      names(df) <- setNames
      data[[dataIndex]] <- df 
      dataIndex <- dataIndex + 1
      rm(df)
    }
  }
  
  metaSet <- rbindlist(data,fill=TRUE)
  write.csv(metaSet,paste0(word,"_transact.csv"),na="",row.names=FALSE)
  rm(metaSet)
  
  setwd(wd)
}


