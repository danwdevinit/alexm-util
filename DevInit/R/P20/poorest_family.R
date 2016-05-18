library(Hmisc)
library(foreign)
library(data.table)
library(plyr)

setwd("D:/Documents/Data/DHSauto/johr6cdt")
data <- read.dta("JOHR6CFL.dta")
cwi.df <- read.csv("wealth_replication.csv",na.strings="",as.is=TRUE)
setnames(data,"hv001","cluster")
setnames(data,"hv002","household")
data <- join(
  data
  ,cwi.df
  ,by=c("cluster","household")
)

poorest.family <- subset(data,household==16 & cluster==119)
poorest.family <- poorest.family[colSums(!is.na(poorest.family)) > 0]

setwd("D:/Documents/Data/DHSauto/jopr6cdt")
members <- read.dta("JOPR6CFL.dta")
setnames(members,"hv001","cluster")
setnames(members,"hv002","household")
members <- join(
  members
  ,cwi.df
  ,by=c("cluster","household")
)

poorest.family.members <- subset(members,household==16 & cluster==119)
poorest.family.members <- poorest.family.members[colSums(!is.na(poorest.family.members)) > 0]

setnames(poorest.family.members,"hv105","age")
setnames(poorest.family.members,"hv104","gender")
setnames(poorest.family.members,"hv109","educ")
setnames(poorest.family.members,"hv115","marital")
setnames(poorest.family.members,"hv101","head.relationship")
setnames(poorest.family.members,"sh07","nationality")
setnames(poorest.family.members,"sh16a","literacy")
setnames(poorest.family.members,"hv024","region")
setnames(poorest.family.members,"hv025","residence.type")
setnames(poorest.family.members,"hv201","drinking.water")
setnames(poorest.family.members,"hv205","toilet")
setnames(poorest.family.members,"hv206","electricity")
setnames(poorest.family.members,"hv207","radio")
setnames(poorest.family.members,"hv213","floor")
setnames(poorest.family.members,"hv214","wall")
setnames(poorest.family.members,"hv215","roof")
setnames(poorest.family.members,"hv216","sleeping.rooms")
setnames(poorest.family.members,"hv225","share.toilet")
setnames(poorest.family.members,"hv226","cooking.fuel")
setnames(poorest.family.members,"hv242","separate.kitchen")
setnames(poorest.family.members,"hv243a","mobile.phone")
setnames(poorest.family.members,"hv252","smoking")
setnames(poorest.family.members,"shgov","governorate")
setnames(poorest.family.members,"shdistrict","district")
setnames(poorest.family.members,"shsub_dist","sub.district")
setnames(poorest.family.members,"shlocality","locality")
setnames(poorest.family.members,"sharea","area")
setnames(poorest.family.members,"shsub_area","sub.area")
setnames(poorest.family.members,"shstratum","stratum")
setnames(poorest.family.members,"shblock","block")
setnames(poorest.family.members,"shbuilding","building")
setnames(poorest.family.members,"shcamps","camp")
setnames(poorest.family.members,"sh100","housing.type")
setnames(poorest.family.members,"sh109b","owns.bed.or.sofa")
setnames(poorest.family.members,"sh110_c","satellite")
setnames(poorest.family.members,"sh110_f","freezer")
setnames(poorest.family.members,"sh110_g","washing.machine")
setnames(poorest.family.members,"sh110_h","dish.washer")
setnames(poorest.family.members,"sh110_i","solar.heater")
setnames(poorest.family.members,"sh110_j","air.con")
setnames(poorest.family.members,"sh110_k","fan")
setnames(poorest.family.members,"sh110_l","water.cooler")
setnames(poorest.family.members,"sh110_m","microwave")
setnames(poorest.family.members,"sh110_n","camera")
setnames(poorest.family.members,"sh110b","mobiles")
setnames(poorest.family.members,"sh113a","separate.bathroom")
setnames(poorest.family.members,"sh116a","rooms")
setnames(poorest.family.members,"sh118","private.car")
setnames(poorest.family.members,"sh123a","credit.card")
setnames(poorest.family.members,"sh110a","computers")
# setnames(poorest.family.members,"sh110d","internet")
# setnames(poorest.family.members,"hv227","water.treated")


keep <- c(
  "cluster"
  ,"household"
  ,"age"
  ,"gender"
  ,"educ"
  ,"marital"
  ,"head.relationship"
  ,"nationality"
  ,"literacy"
  ,"region"
  ,"residence.type"
  ,"drinking.water"
  ,"toilet"
  ,"electricity"
  ,"radio"
  ,"tv"
  ,"phone"
  ,"car"
  ,"fridge"
  ,"floor"
  ,"wall"
  ,"roof"
  ,"sleeping.rooms"
  ,"share.toilet"
  ,"cooking.fuel"
  ,"separate.kitchen"
  ,"mobile.phone"
  ,"smoking"
  ,"governorate"
  ,"district"
  ,"sub.district"
  ,"locality"
  ,"area"
  ,"sub.area"
  ,"stratum"
  ,"block"
  ,"building"
  ,"camp"
  ,"housing.type"
  ,"owns.bed.or.sofa"
  ,"satellite"
  ,"freezer"
  ,"washing.machine"
  ,"dish.washer"
  ,"solar.heater"
  ,"air.con"
  ,"fan"
  ,"water.cooler"
  ,"microwave"
  ,"camera"
  ,"mobiles"
  ,"separate.bathroom"
  ,"rooms"
  ,"private.car"
  ,"credit.card"
  ,"computers"
#   ,"water.treated"
#   ,"internet"
  )

for(i in 1:length(keep)){
  varname <- keep[i];
  if(!(varname %in% names(poorest.family.members))){
    message(varname)
  }
}

poorest.family.members <- poorest.family.members[keep]

poorest <- t(poorest.family.members)

write.csv(data.frame(poorest),"D:/Documents/Data/DHSmeta/poorest.family.csv")

