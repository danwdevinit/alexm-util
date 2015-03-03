install.packages('foreign')
require(foreign)

path<- "C:/Users/alexm/AppData/Roaming/Skype/My Skype Received Files/allginis_2014.dta"

df <- read.dta(path)

write.csv(df,"allginis_2014.csv",row.names=FALSE,na="")
