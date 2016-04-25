library(plyr)
library(Hmisc)
library(foreign)
library(ggplot2)

wd <- "D:/Documents/Data/DHS/v_6_hh"
setwd(wd)

filenames <- list.files(wd, pattern="*.zip",ignore.case=TRUE)


for(i in 1:length(filenames)){
  zip <- filenames[i]
  unzip(zip, files = NULL, list = FALSE, overwrite = TRUE,
        junkpaths = FALSE, exdir = ".", unzip = "internal",
        setTimes = FALSE)
}

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

df <- hh

df$hv011[which(is.na(df$hv011))] <- 0
df$hv219[which(df$hv219=="Female")] <- "female"
df$hv219[which(df$hv219=="Male")] <- "male"
df$hv219 <- factor(df$hv219,levels=c("female","male"))

df <- transform(df,none.in.target=(hv010==0 & hv011==0),some.in.target=(hv010>0 | hv011>0))

ot <- subset(df,none.in.target==TRUE & hv220>=15 & hv220<=49)
plot(table(ot$hv220,ot$hv219))

ot <- subset(ot,hv219=="female")
write.csv(ot,"female_headed_not_surveyed.csv",na="",row.names=FALSE)
