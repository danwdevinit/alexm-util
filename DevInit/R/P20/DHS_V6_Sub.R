library(plyr)
library(Hmisc)
library(foreign)
library(ggplot2)

wd <- "D:/Documents/Data/DHS/v_6"
setwd(wd)

# filenames <- list.files(wd, pattern="*.zip",ignore.case=TRUE)
# 
# 
# for(i in 1:length(filenames)){
#   zip <- filenames[i]
#   unzip(zip, files = NULL, list = FALSE, overwrite = TRUE,
#         junkpaths = FALSE, exdir = ".", unzip = "internal",
#         setTimes = FALSE)
# }

dtas <- list.files(wd, pattern="*.dta",ignore.case=TRUE)

if(exists("hh")){
  rm(hh)
}

for(i in 1:length(dtas)){
  dta <- dtas[i]
  message(paste(paste0(i,"/",length(dtas)),dta))
  dat <- read.dta(dta)
  if(!exists("hh")){
    hh <- dat
  }else{
    common <- intersect(names(hh),names(dat))
    hh <- hh[common]
    dat <- dat[common]
    hh <- rbind(hh,dat)
  }
}

hh$hv270 <- tolower(hh$hv270)
codeAgeCat <- function(x){
  startAge <- 0
  ageDiff <- 4
  endAge <- 4
  if(is.na(x)){
    return("missing")
  }
  while(endAge<95){
    endAge <- startAge+ageDiff
    if(x>=startAge & x<=endAge){
      return(
        paste0(startAge,"-",endAge)  
      )
    }
    startAge <- endAge + 1
  }
  return("missing")
}

hh$ageCategory <- vapply(hh$hv105,codeAgeCat,character(1))
hh$ageCategory <- factor(hh$ageCategory,
                         levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34"
                                    ,"35-39","40-44","45-49","50-54","55-59","60-64"
                                    ,"65-69","70-74","75-79","80-84","85-89","90-94"
                                    ,"95+","missing")                          
)
hh$hv270 <- factor(hh$hv270,
                   levels=c("poorest","poorer","middle","richer","richest"))

plot(table(hh$ageCategory,hh$hv270))

df <- ddply(hh,.(hv000,hv001,hv002,hv007,hv270),
function(x){
  bool = TRUE
  yBool = TRUE
  oBool = TRUE
  hv000 <- x$hv000[1]
  hv001 <- x$hv001[1]
  hv002 <- x$hv002[1]
  hv007 <- x$hv007[1]
  hv270 <- x$hv270[1]
  for(i in 1:length(x)){
    age <- x$hv105[i]
    if(!is.na(age)){
      bool <- bool & (age < 15 | age > 49)
      yBool <- yBool & age < 15
      oBool <- oBool & age > 49
    }
  }
  return(data.frame(hv000,hv001,hv002,hv007,hv270,none.in.target=bool,just.young=yBool,just.old=oBool))
}
)

plot(table(df$none.in.target,df$hv270))

write.csv(df,"households2.csv",na="",row.names=FALSE)

df <- hh[0,]

text <- readLines("label.do")
for(i in 1:length(text)){
  line <- text[i]
  if(substr(line,1,5)=="label"){
    quoteIndex <- regexpr("\"",line)
    label <- gsub("\"","",substr(line,quoteIndex,nchar(line)))
    varname <- substr(line,16,quoteIndex-1)
    varname <- gsub("_",".",varname,fixed=TRUE)
    varname <- gsub(" ","",varname,fixed=TRUE)
    command <- paste0("names(df)[which(names(df)=='",varname,"')] = \"",label,"\"")
    if(varname %in% names(df)){
      eval(parse(text=command)) 
    }
  }
}
write.csv(df,"dhs_phase6_subsaharan.csv",na="",row.names=FALSE)
