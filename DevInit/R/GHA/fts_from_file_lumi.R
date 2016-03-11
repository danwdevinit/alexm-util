library(plyr)

wd <- "C:/R/"
setwd(wd)
data <- read.csv("fts.csv",header=TRUE,skip=5,na.strings="",as.is=TRUE)

#Parse numbers (removing commas)
data <- transform(data,USD.committed.contributed=as.numeric(gsub(",","", USD.committed.contributed)))
data <- transform(data,Original.currency.amount=as.numeric(gsub(",","", Original.currency.amount)))
data <- transform(data,USD.pledged=as.numeric(gsub(",","", USD.pledged)))
data <- transform(data,Project.current.request=as.numeric(gsub(",","", Project.current.request)))
data <- transform(data,Item.ID=as.numeric(gsub(",","", Item.ID)))

#Remove total row
data <- subset(data,Donor!="Total:")

#Merge to create new column "Code name" based on donor type
codenames <- read.csv("codename.csv",na.strings="",as.is=TRUE)
codenames <- codenames[!duplicated(codenames$Donor),]
data <- join(data, codenames, by='Donor', type='left', match='all')

#Lets find donors without a codename
withoutCodename <- subset(data,is.na(codename))
unique(withoutCodename$Donor)

#Merge used to create new column "Private money" based on donor type
private <- c("private","no")
private.money <- data.frame(Donor,private.money)
data <- merge(
  data
  ,code.name
  ,by=c("Donor")
  ,all.x=TRUE
)


data <- read.csv("otherprivateorgs.csv",header=TRUE,na.strings="",as.is=TRUE)
otherprivateorgs <- c("Org name") 

data <- merge(
  data
  ,otherprivateorgs
  ,by=c("Donor", "Org name")
  ,all.x=TRUE
)

if codename = "private sector corporations" OR "foundations" OR "private individual organisations"
private="private"
else 
  if 
otherprivateorgs = "private"
private = "private"
else 
  private.money="no"


#Merge to create new column "Donor DAC region" based on country list of DAC regions
OECDregionname <- c("Europe","FAr East Asia","Middle East","North & Central America","North of Sahara","Oceania","South & Central Asia","South America","South of Sahara")
oecdregion.donor <- data.frame(Donor, OECDCountry)
data <- merge(
  data
  ,OECDcountry
  ,by=c("Donor")
  ,all.x=TRUE
)
data <- transform(data,OECDcountry=oecdregion.donor[Donor])

#Merge to create new column "Donor UN region" based on country list of UN regions; replicate as above


#Merge to create new column "Appealing agency code name" based on appealing agency 
Appealingagency <- unique(data$Appealingagency)
appelaingagency <- c("academia/think/research","CERF","CHF","DAC governments","EC","ERF","foundations","NDD","NGOs","other","Other multi","Other pooled","private individual organisations","private sector corporations","red cross","UN Multi")
appealing.agency <- data.frame(Appealingagency,appealing.agency)
data <- merge(
  data
  ,code.name
  ,by=c("Appealingagency")
  ,all.x=TRUE
)
data <- transform(data,appealingagency=appealing.agency[Appealingagency])


#Merge to create new column "NGO type" based on appealing agency type
NGOtype <- unique(data$NGO)
NGOtype <- c("Affiliated national NGO","International NGO","National NGO","Local NGO","Undefined","SINGO")
ngo.code <- data.frame(Appealingagency,NGO)
data <- merge(
  data
  ,code.name
  ,by=c("Appealingagency")
  ,all.x=TRUE
)
data <- transform(data,NGOtype=ngo.code[Appealingagency])


#Merge to create new column "Appealing agency DAC region" based on country list of DAC regions
OECDregionagency <- c("Europe","FAr East Asia","Middle East","North & Central America","North of Sahara","Oceania","South & Central Asia","South America","South of Sahara")
oecdregion.agency <- data.frame(Appealingagency, OECDCountry)
data <- merge(
  data
  ,OECDregionagency
  ,by=c("Appealingagency")
  ,all.x=TRUE
)
data <- transform(data,OECDregionagency=oecdregion.agency[appelaingagency])

#Merge to create new column "Appealing agency UN region" based on country list of UN regions; replicate as above

#Create new column "Channels" based on appealing agency code name
Channels <- c("Multilateral","Public sector","Red Cross","NGOs and CSOs","Other")
if appealingagency = "CHF" OR "ERF" OR "Other pooled" OR "CERF" OR "UN Multi" OR "Other Multi"
Channels = "Multilateral" else
  if appealingagency = "DAC government" OR "NDD" OR "EC" Channels = "Public sector" else
    if appealingagency = "red cross" Channels = "Red Cross" else
      if appealingagency = "NGOs" OR "foundations" OR "academia/think/research" Channels = "NGOs and CSOs" else
        if appealingagency = "private sector corporations" OR "private individual organisations" OR "other" Channels = "Other" else
          Channels = "Code not found"

#Create new column "Domestic response" based on donor is same as destination country
destinationcountry <- c(DestinationCountry)
domesticresponse <- c("Domestic","No")
if Donor = Destinationcountry 
domesticresponse = "Domestic" else domesticresponse = "No"


#Merge to create new column "Fragile states" based on country list 
fragilestaes <- c("Fragile","No")
fragile.country <- data.frame(destinationcountry, fragilestate)
data <- merge(
  data
  ,fragilestaes
  ,by=c("Destination Country")
  ,all.x=TRUE
)

data <- transform(data,fragilestaes=fragile.country[destinationcountry])


#Replicate above for environmentally vulnerable; income groups; developing countries lists

#how do I make sure about country names being same?!

