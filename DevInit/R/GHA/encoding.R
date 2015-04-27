#Define function "encoding"
encoding <- function(path,sep){
  #List all possible encodings
  codepages <- setNames(iconvlist(), iconvlist())
  
  #Try to turn warnings off...This may not work right.
  options(warn=-1)
  #Apply each of the encodings to the filepath, recording the results in a list
  x <- lapply(codepages, function(enc) try(read.table(path,
                          fileEncoding=enc,
                          nrows=3, header=TRUE, sep=sep)))
  #Try to turn warnings back on
  options(warn=0)
  #Record the dimensions of our list sets
  rowsCols <- unique(do.call(rbind, sapply(x, dim)))
  
  #For every combination of rows and columns, find the encoding that
  #output 3 rows (the correct number)
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

#Example pipe separated value
path <- "C:/Users/alexm/Downloads/CRS 2013 data/CRS 2013 data.txt"
possible_encodings <- encoding(path,"|")
print(possible_encodings)

#Example CSV
path <- "C:/git/digital-platform/concepts.csv"
possible_encodings <- encoding(path,",")
print(possible_encodings)
