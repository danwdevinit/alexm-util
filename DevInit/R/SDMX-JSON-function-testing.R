#Necessary libraries
library(RCurl)
library(rjson)

OECDa <- function(content){
  BOM <- "\ufeff"
  if(attr(regexpr(BOM, content), "match.length") != - 1){
    content <- gsub(BOM, "", content)
  }
  rawJson <- fromJSON(content)
  rawData <- rawJson$dataSets[[1]]$observations
  rawStructure <- rawJson$structure
  dimensions <- rawStructure$dimensions[[1]]
  attributes <- rawStructure$attributes$observation
  names <- c(sapply(dimensions, "[[", 2),"obsValue",sapply(attributes, "[[", 1))
  ndim <- length(sapply(dimensions, "[[", 2))
  natt <- 1+length(sapply(attributes, "[[", 1))
  ncol <- ndim+natt
  data <- setNames(data.frame(matrix(ncol = ncol, nrow = 0)),names)
  for(i in 1:length(rawData)){
    row <- rawData[i]
    rawDimensions <- names(row)
    splitDimensions <- strsplit(rawDimensions,":")[[1]]
    for(j in 1:length(splitDimensions)){
      dimensionReference <- dimensions[[j]]$values
      dimensionIndex <- as.integer(splitDimensions[j])+1
      dimensionValue <- dimensionReference[[dimensionIndex]][[2]]
      data[i,j] <- dimensionValue
    }
    for(j in 1:length(row[[1]])){
      if(j>1){
        attributeReference <- attributes[[j-1]]$values
        rawAttIndex <- row[[1]][[j]]
        if(is.null(rawAttIndex)){
          attributeValue <- NA
        }else{
          attributeIndex <- as.integer(rawAttIndex)+1
          attributeValue <- attributeReference[[attributeIndex]][[2]]
        }
      }else{
        attributeValue <- as.double(row[[1]][[j]])
      }
      data[i,ndim+j] <- attributeValue
    }
  }
  names(data)[which(names(data)=="Year")] <- "obsTime"
  return(data.frame(data))
}

OECDb <- function(content){
  BOM <- "\ufeff"
  if(attr(regexpr(BOM, content), "match.length") != - 1){
    content <- gsub(BOM, "", content)
  }
  rawJson <- fromJSON(content)
  rawData <- rawJson$dataSets[[1]]$observations
  rawStructure <- rawJson$structure
  dimensions <- rawStructure$dimensions[[1]]
  attributes <- rawStructure$attributes$observation
  names <- c(sapply(dimensions, "[[", 2),"obsValue",sapply(attributes, "[[", 1))
  ndim <- length(sapply(dimensions, "[[", 2))
  natt <- 1+length(sapply(attributes, "[[", 1))
  ncol <- ndim+natt
  #data <- setNames(data.frame(matrix(ncol = ncol, nrow = 0)),names)
  data <- list()
  for(i in 1:length(names)){
    data[[names[i]]] = character(length(rawData))
  }
  for(i in 1:length(rawData)){
    row <- rawData[i]
    rawDimensions <- names(row)
    splitDimensions <- strsplit(rawDimensions,":")[[1]]
    for(j in 1:length(splitDimensions)){
      dimensionReference <- dimensions[[j]]$values
      dimensionIndex <- as.integer(splitDimensions[j])+1
      dimensionValue <- dimensionReference[[dimensionIndex]][[2]]
      #data[i,j] <- dimensionValue
      data[[j]][i] <- dimensionValue
    }
    for(j in 1:length(row[[1]])){
      if(j>1){
        attributeReference <- attributes[[j-1]]$values
        rawAttIndex <- row[[1]][[j]]
        if(is.null(rawAttIndex)){
          attributeValue <- NA
        }else{
          attributeIndex <- as.integer(rawAttIndex)+1
          attributeValue <- attributeReference[[attributeIndex]][[2]]
        }
      }else{
        attributeValue <- as.double(row[[1]][[j]])
      }
      #data[i,ndim+j] <- attributeValue
      data[[ndim+j]][i] <- attributeValue
    }
  }
  data <- data.frame(data,stringsAsFactors=FALSE)
  names(data)[which(names(data)=="Year")] <- "obsTime"
  return(data)
}

OECDc <- function(content){
  BOM <- "\ufeff"
  if(attr(regexpr(BOM, content), "match.length") != - 1){
    content <- gsub(BOM, "", content)
  }
  rawJson <- fromJSON(content)
  rawData <- rawJson$dataSets[[1]]$observations
  rawStructure <- rawJson$structure
  dimensions <- rawStructure$dimensions[[1]]
  attributes <- rawStructure$attributes$observation
  names <- c(sapply(dimensions, "[[", 2),"obsValue",sapply(attributes, "[[", 1))
  ndim <- length(sapply(dimensions, "[[", 2))
  natt <- 1+length(sapply(attributes, "[[", 1))
  ncol <- ndim+natt
  #data <- setNames(data.frame(matrix(ncol = ncol, nrow = 0)),names)
  data <- matrix(ncol=ncol,nrow=length(rawData))
  for(i in 1:length(rawData)){
    row <- rawData[i]
    rawDimensions <- names(row)
    splitDimensions <- strsplit(rawDimensions,":")[[1]]
    for(j in 1:length(splitDimensions)){
      dimensionReference <- dimensions[[j]]$values
      dimensionIndex <- as.integer(splitDimensions[j])+1
      dimensionValue <- dimensionReference[[dimensionIndex]][[2]]
      data[i,j] <- dimensionValue
    }
    for(j in 1:length(row[[1]])){
      if(j>1){
        attributeReference <- attributes[[j-1]]$values
        rawAttIndex <- row[[1]][[j]]
        if(is.null(rawAttIndex)){
          attributeValue <- NA
        }else{
          attributeIndex <- as.integer(rawAttIndex)+1
          attributeValue <- attributeReference[[attributeIndex]][[2]]
        }
      }else{
        attributeValue <- as.double(row[[1]][[j]])
      }
      data[i,ndim+j] <- attributeValue
    }
  }
  data <- setNames(data.frame(data,stringsAsFactors=FALSE),names)
  names(data)[which(names(data)=="Year")] <- "obsTime"
  return(data)
}

####Testing####
#' An alternative to \code{summaryRprof()}
#' 
#' \code{proftools} parses a profiling file and prints an easy-to-understand
#' table showing the most time-intensive function calls. 
#' 
#' Line numbers are included if \code{Rprof()} was run with 
#' \code{line.numbering=TRUE}. If it was run with \code{memory.profiling=TRUE},
#' this function will probably break.
#' 
#' Below the table are printed any files identified if line numbering is true,
#' the total time recorded by \code{Rprof()}, and the "parent call".  The
#' parent call consists of the parent call stack of all the call stacks in the\
#' table. Note that this is the parent call stack of only the printed lines,
#' not of all stacks recorded by \code{Rprof()}. This makes the table easier to read and fit into the console. 
#' 
#' @export
#' @param file A profiling file generated by \code{Rprof()}
#' @param lines The number of lines (call stacks) you want returned. Lines are
#' printed from most time-intensive to least.
proftable <- function(file, lines = 10) {
  profdata <- readLines(file)
  interval <- as.numeric(strsplit(profdata[1L], "=")[[1L]][2L]) / 1e+06
  filelines <- grep("#File", profdata)
  files <- profdata[filelines]
  profdata <- profdata[-c(1, filelines)]
  total.time <- interval * length(profdata)
  ncalls <- length(profdata)
  profdata <- gsub("\\\"| $", "", profdata)
  calls <- lapply(profdata, function(x) rev(unlist(strsplit(x, " "))))
  stacktable <- as.data.frame(table(sapply(calls, function(x) paste(x, collapse = " > "))) / ncalls * 100, stringsAsFactors = FALSE)
  stacktable <- stacktable[order(stacktable$Freq[], decreasing = TRUE), 2:1]
  colnames(stacktable) <- c("PctTime", "Call")
  stacktable <- head(stacktable, lines)
  shortcalls = strsplit(stacktable$Call, " > ")
  shortcalls.len <- range(sapply(shortcalls, length))
  parent.call <- unlist(lapply(seq(shortcalls.len[1]), function(i) Reduce(intersect, lapply(shortcalls,"[[", i))))
  shortcalls <- lapply(shortcalls, function(x) setdiff(x, parent.call))
  stacktable$Call = sapply(shortcalls, function(x) paste(x, collapse = " > "))
  if (length(parent.call) > 0) {
    parent.call <- paste(paste(parent.call, collapse = " > "), "> ...")
  } else {
    parent.call <- "None"
  }
  frac <- sum(stacktable$PctTime)
  attr(stacktable, "total.time") <- total.time
  attr(stacktable, "parent.call") <- parent.call
  attr(stacktable, "files") <- files
  attr(stacktable, "total.pct.time") <- frac
  print(stacktable, row.names=FALSE, right=FALSE, digits=3)
  if(length(files) > 0) {
    cat("\n")
    cat(paste(files, collapse="\n"))
    cat("\n")
  }
  cat(paste("\nParent Call:", parent.call))
  cat(paste("\n\nTotal Time:", total.time, "seconds\n"))
  cat(paste0("Percent of run time represented: ", format(frac, digits=3)), "%")
  
  invisible(stacktable)
}

url <- "http://stats.oecd.org/SDMX-JSON/data/TABLE1/20005+20001+801+1+2+301+68+3+18+4+5+40+20+21+6+701+742+22+7+820+8+76+9.1.5+1010+2102.1121+1122.A+D+N/all?startTime=2005&endTime=2014&dimensionAtObservation=allDimensions"
#url <- "http://stats.oecd.org/SDMX-JSON/data/TABLE1/20005+20001+801+1+2+301+68+3+18+4+5+40+20+21+6+701+742+22+7+820+8+76+9+69+61+50+10+11+12+302+20002+918+20006+72+62+30+82+75+546+613+552+83+70+84+45+77+87+566+732+764+55+576+20007+20003+301+4+5+6+701+12+302+20011+1+2+68+3+18+4+5+40+21+6+22+7+76+9+69+61+50+10+12+1+2+68+3+18+4+5+40+21+6+22+7+76+9+69+61+50+10+12+918.1.5+1010+1015+1100+1110+1120+1200+1210+1211+1212+1213+1214+1220+1230+1300+1310+1311+1320+1330+1301+1400+1410+1420+1500+1510+1520+1600+1610+1611+1612+1613+1614+1620+1621+1622+1623+1624+1630+1640+1700+1800+1810+1820+1900+1999+99999+1901+1902+1903+1904+1905+1906+60+70+2000+2100+2101+2102+2103+2104+547+2105+2106+2107+2108+2110+2901+2902+230+235+240+265+266+294+291+292+293+280+287+300+301+302+310+303+295+299+298+102+325+326+327+795+800+805+786+330+332+340+345+353+384+751+752+753+386+756+761+388+389+103+359+415+425+420+207+1+2+3+4.1121+1122+1120+1130+1140+1151+1152+1150.A+D/all?startTime=2005&endTime=2014&dimensionAtObservation=allDimensions&pid=baa5e675-3487-422e-bb2a-4da97dbbfd4d"
content <- getURL(url, httpheader = list('User-Agent' = 'rsdmx-json/0.0.1'), ssl.verifypeer = FALSE, .encoding = "UTF-8")

Rprof("out.out",line.profiling=TRUE)
data <- OECDc(content)
Rprof(NULL)
proftable("out.out")

library(microbenchmark)
compare <- microbenchmark(OECDa(content), OECDb(content), OECDc(content), times = 100)

library(ggplot2)
autoplot(compare)
####End testing####
