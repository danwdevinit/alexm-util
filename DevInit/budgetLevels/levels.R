wd <- "C:/git/alexm-util/DevInit/budgetLevels"
setwd(wd)
df <- read.csv("./results.csv", header = TRUE,sep=",",na.strings="",check.names=FALSE,stringsAsFactors=FALSE)
names(df)[names(df) == "iso"] <- "id"
mult <- read.csv("C:/git/digital-platform/reference/current-ncu-to-constant-2012-usd.csv", header = TRUE,sep=",",na.strings="",check.names=FALSE,stringsAsFactors=FALSE)
names(mult)[names(mult)=="value"] <- "value.mult"
if("value-ncu" %in% colnames(df)){
  names(df)[names(df)=="value-ncu"] <- "value.ncu"
  df$value <- df$value.ncu
}else{
  df$value.ncu <- df$value
}
df <- merge(
  df
  ,mult
  ,by=c("id","year")
)
df <- transform(df,value=value.mult*value.ncu)
df <- transform(df,l1=gsub(" ","-",tolower(gsub("[^[:alnum:] ]", "", l1))))
df <- transform(df,l2=gsub(" ","-",tolower(gsub("[^[:alnum:] ]", "", l2))))
df <- transform(df,l3=gsub(" ","-",tolower(gsub("[^[:alnum:] ]", "", l3))))
df <- transform(df,l4=gsub(" ","-",tolower(gsub("[^[:alnum:] ]", "", l4))))
df <- transform(df,l5=gsub(" ","-",tolower(gsub("[^[:alnum:] ]", "", l5))))
df <- transform(df,l6=gsub(" ","-",tolower(gsub("[^[:alnum:] ]", "", l6))))
keep <- c(1,2,5,6,7,8,9,10,11,12,13)
df <- df[keep]
names(df)[names(df) == "value.ncu"] <- "value-ncu"
names(df)[names(df) == "type"] <- "budget-type"
write.csv(df,"./results_ncu.csv",row.names=FALSE,na="")
names(df)
df <- read.csv("./results.csv", header = TRUE,sep=",",na.strings="",check.names=FALSE,stringsAsFactors=FALSE)
levels <- df[c(6:11)]
levels <- reshape(
  levels
  ,varying=1:6
  ,sep=""
  ,direction="long")
levels <- levels[c(1,2)]
names(levels) <- c("level","name")
levels <- unique(levels)[complete.cases(levels),]
levels <- transform(levels,id=gsub(" ","-",tolower(gsub("[^[:alnum:] ]", "", name))))
levels$sectoral <- "FALSE"
levels <- levels[c(3,4,1,2)]
levels <- df[c(6:11)]
levels <- reshape(
  levels
  ,varying=1:6
  ,sep=""
  ,direction="long")
levels <- levels[c(1,2)]
names(levels) <- c("level","name")
levels <- unique(levels)[complete.cases(unique(levels)),]
levels <- transform(levels,id=gsub(" ","-",tolower(gsub("[^[:alnum:] ]", "", name))))
levels$sectoral <- "FALSE"
levels <- levels[c(3,4,1,2)]
old.levels <- read.csv("C:/git/digital-platform/reference/domestic-budget-level.csv", header = TRUE,sep=",",na.strings="",check.names=FALSE,stringsAsFactors=FALSE)
sectoral <- subset(old.levels,sectoral==TRUE)
old.levels <- old.levels[c(1,2,3,5)]
levels <- merge(
  levels
  ,old.levels
  ,by=intersect(names(levels),names(old.levels))
  ,all.x=TRUE
)
levels <- levels[order(levels$id)[!duplicated(sort(levels$id))],]
new.levels <- rbind(levels,sectoral)
new.levels <- unique(new.levels)
write.csv(new.levels,"./results_levels.csv",row.names=FALSE,na="")
