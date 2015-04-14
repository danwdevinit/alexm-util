encoding <- function(path,sep){
  codepages <- setNames(iconvlist(), iconvlist())
  
  options(warn=-1)
  x <- lapply(codepages, function(enc) try(read.table(path,
                          fileEncoding=enc,
                          nrows=3, header=TRUE, sep=sep)))
  options(warn=0)
  rowsCols <- unique(do.call(rbind, sapply(x, dim)))
  
  for(i in 1:length(rownames(rowsCols))){
    enc <- rownames(rowsCols)[i]
    rows <- rowsCols[enc,][1]
    cols <- rowsCols[enc,][2]
    if(rows==3){
      row <- rows
      col <- cols
    }
  }
  
  #View encodings that might be right
  maybe_ok <- sapply(x, function(x) isTRUE(all.equal(dim(x), c(row,col))))
  return(codepages[maybe_ok])
}

path <- "C:/Users/alexm/Downloads/CRS 2013 data/CRS 2013 data.txt"
possible_encodings <- encoding(path,"|")
print(possible_encodings)

path <- "C:/git/digital-platform/concepts.csv"
possible_encodings <- encoding(path,",")
print(possible_encodings)
