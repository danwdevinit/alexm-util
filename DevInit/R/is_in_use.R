wd <- "C:/git/alexm-util/DevInit/R"
setwd(wd)

trunc <- function(str){
  newStr <- substr(str,1,nchar(str)-4)
  return (newStr)
}

concepts <- read.csv("C:/git/digital-platform/concepts.csv",na.strings="",as.is=TRUE)
concepts <- transform(
  concepts
  ,hasMapTheme = !is.na(map.theme)
  )
keep <- c("id","hasMapTheme")
concepts <- concepts[keep]

countryYear <- list.files("C:/git/digital-platform/country-year", pattern="*.csv")
countryYear <- data.frame(sapply(countryYear,trunc),stringsAsFactors=FALSE)
names(countryYear) <- "id"
countryYear$reference <- FALSE

refs <- list.files("C:/git/digital-platform/reference", pattern="*.csv")
refs <- data.frame(sapply(refs,trunc),stringsAsFactors=FALSE)
names(refs) <- "id"
refs$reference <- TRUE

allFiles <- rbind(countryYear,refs)

code <- readLines("C:/git/di-website/wp/wp-content/themes/diConcept/dist/app.js")

used <- logical(nrow(allFiles))
for(i in 1:nrow(allFiles)){
  fileName <- allFiles[i,1]
  inCode <- max(grepl(fileName,code))
  used[i] <- inCode
}

allFiles$used <- used

final <- merge(
  allFiles
  ,concepts
  ,by="id"
  ,all=TRUE
  )

final[which(is.na(final$used)),]$used <- 0
final[which(is.na(final$hasMapTheme)),]$hasMapTheme <- FALSE
final[which(is.na(final$reference)),]$reference <- FALSE

final <- transform(
  final
  ,in_ddh=used | hasMapTheme
    )

keep <- c("id","reference","in_ddh")
final <- final[keep]

#We know these are used, even if they're not in the code
final[which(grepl("oda-donor",final$id)),]$in_ddh <- TRUE

write.csv(final,"in_code_check.csv",na="",row.names=FALSE)
