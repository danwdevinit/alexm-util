#Import
df <- read.csv("C:/git/alexm-util/Rscripts/Snap Poll Data.csv")
View(df)

#Change working directory
getwd()
WD <- "C:/git/alexm-util/Rscripts/"
setwd (WD)
df <- read.csv("Snap Poll Data.csv")

#Read from online
fpe <- read.table("http://data.princeton.edu/wws509/datasets/effort.dat")

#Package for pulling from drop box: repmis
#Package for pulling directly from server: RMySQL

#Remove one column named "surveyTitle"
df <- df[,!(names(df) %in% "surveyTitle")]

View(df[1:10,])

#Subsets and merges
df1 <- df[1:1008,]
df2 <- df[1009:2060,]

df1 <- df1[,1:20]
df2 <- df2[,c(1:5,21:35)]

df3 <- merge(df1,df2,by = c("standardId","surveyCountry","surveyYear",
                            "respondentGender","respondentBirthYear"), all=TRUE)

#Create new varibles/Rename
names(df)[names(df) == "respondentGender"] <- "Gender"
names(df)[names(df) == "respondentBirthYear"] <- "Age"

summary(df$Age)

df$Old[df$Age == c("45-54","55-64","65 and up")] <- "Older"
df$Old[df$Age == c("20-34","35-44")] <- "Younger"

View(df)

df[is.na(df)] <- ""
View(df)

df$Gender2[df$Gender =="m"] <- "Male"
df$Gender2[df$Gender =="f"] <- "Female"

#MTCARS
SE <- (sd(mtcars$mpg)/sqrt(nrow(mtcars)))
mtcars$SEP <- mtcars$mpg + SE
mtcars$SEN <- mtcars$mpg - SE

df$Test1 <-(log(10)-log(2))
df$Test2 <- (sqrt(144))
df$Test3 <- df$Test1 - df$Test2

df <- read.csv("Snap Poll Data.csv")

#Drop column surveyTitle
df <- df[,!(names(df) %in% "surveyTitle")]

#Drop if not m
df1 <- df[!(df$respondentGender %in% "m"),]

#Keep if m
df1 <- df[(df$respondentGender %in% "m"),]
df1 <- df[which(df$respondentGender == "m"),]

#Subset with columns
sub <- c("respondentGender","respondentBirthYear","qg_70")
df1 <- subset(df,select = sub)

#Subset with rows
df1 <- subset(df, respondentGender == "m" | respondentGender =="f")

#Select random rows
df1 <- df[sample(1:nrow(df),500, replace = FALSE),]

#Proportions by year
df <- read.csv("TRIP_journal.csv")
names(df)[names(df) == "C..Year"] <- "Year"
library(plyr)
df1 <- count(df, vars = c("Year","Policy_Prescription","Issue_Area"))
View(df1)

#Split array and turn it into dataframe is daply, ddply is data to data
#Group by year, data mutation, find proportion by this function
df1 <- ddply(.data = df1, .variables = .(Year), mutate,
             Proportion = freq/sum(freq))
View(df1)
#Only yesses
df1<- df1[!(df1$Policy_Prescription %in% "No"),]

#Dplyr?
install.packages("Lahman")
library(Lahman)
install.packages("dplyr")
library(dplyr)

players <- group_by(Batting,playerID)
games <- summarise(players,total = sum(G))
head(arrange(games,desc(total)),5)

Batting %.%
  group_by(playerID) %.%
  summarise(total=sum(G)) %.%
  arrange(desc(total)) %.%
head(5)