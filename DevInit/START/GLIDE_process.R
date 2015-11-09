#Setwd
wd <- "C:/git/alexm-util/DevInit/START"
setwd(wd)

#Define increment
`%+=%` = function(e1,e2) eval.parent(substitute(e1 <- e1 + e2))

#Read glide
glide <- read.csv("glide_results.csv",as.is=TRUE,na.strings="")

#Drop indeterminate countries
glide <- subset(
  glide
  ,Country!="(Non-Localized)"
  )

#Fix zeroes in date, set to jan 1
glide$Month[which(glide$Month<1)] <- 1
glide$Day[which(glide$Day<1)] <- 1

#Flip one American style date
glide[398,]$Month <- 11
glide[398,]$Day <- 30

#Parse date
glide <- transform(
  glide
  ,Date=as.Date(paste(Year,Month,Day,sep="-"))
  )

#Drop events prior to 2000 (prior to GLIDE)
glide <- subset(
  glide
  ,Date>as.Date("2000-01-01")
  )

#Drop irrelevant vars
keep <- c("Country","Country_Code","Date","Event_Code")
glide <- glide[keep]

#For every date without a crisis, crisis==0
countries <- unique(glide$Country_Code)
days <- seq(as.Date("2000-01-01"), as.Date("2015-11-05"), by="1 day")
permutation_length <- length(days)*length(countries)

glide_countries <- character(permutation_length)
glide_days <- double(permutation_length)
# class(glide_days) <- "Date"
glide_crisis <- double(permutation_length)
glide_crisis_code <- character(permutation_length)
permut_id <- 0
for(i in 1:length(countries)){
  country <- countries[i]
  for(j in 1:length(days)){
    day <- days[j]
    permut_id %+=% 1
    if((permut_id %% 50000)==0){message(permut_id)}
    glide_match <- subset(glide,(Country_Code==country & Date==day))
    if(nrow(glide_match)>0){
      glide_countries[permut_id] <- country
      glide_days[permut_id] <- day
      glide_crisis[permut_id] <- 1
      glide_crisis_code[permut_id] <- glide_match[1,4]
    }else{
      glide_countries[permut_id] <- country
      glide_days[permut_id] <- day
      glide_crisis[permut_id] <- 0
      glide_crisis_code[permut_id] <- NA
    }
  }
}

dat <- data.frame(
  glide_countries
  ,glide_days
  ,glide_crisis
  ,glide_crisis_code
  ,stringsAsFactors=FALSE)
names(dat) <- c("iso3","date","crisis","crisis_code")

dat <- transform(dat,date=as.Date(date,origin="1970-01-01"))

write.csv(dat,"crisis_dummy.csv",row.names=FALSE,na="")

