#Works as "help()"
?View

#Use command+return to run a line

#Information on data.frame
?mtcars

#View pre-installed data.frame
View(mtcars)

#print data in console
print(mtcars)

#Look at attributes/sum stats of data.frame
summary(mtcars)

#Dimensions of data
dim(mtcars)
nrow(mtcars)
ncol(mtcars)

###############Collapsible Section/Highlight to run all########
dim(mtcars)
nrow(mtcars)
ncol(mtcars)
###############################################################

#Column and row names
colnames(mtcars)
rownames(mtcars)

#Mean of column named...
mean(mtcars$mpg)
mean(mtcars$wt)

#Standard Dev/Median
sd(mtcars$wt)
median(mtcars$wt)

#Assign a value to an object
four <- 4

#Create vectors; c() is concatenate
wb <- c("Stop","Drop","Roll")
xb <- c(1,2,3)

#data.frame joins vectors into matrix
df <- data.frame(wb,xb)

#Test frame
fivebytwo <- data.frame(c(1,2,3,4,5),c(1,2,3,4,5))
twobyfive <- data.frame(c(1,2),c(1,2),c(1,2),c(1,2),c(1,2))

#OLS
library(MASS)
xframe <- data.frame(c(1,1,1,1,1),c(12,32,44,52,11),c(2,3,1,5,2))
yframe <- data.frame(c(12,512,3,2,3))
x <- as.matrix(xframe)
y <- as.matrix(yframe)
b <- ginv(t(x)%*%x)%*%(t(x)%*%y)

#Install packages
install.packages("plyr")
library(plyr)
install.packages("devtools")
require(devtools)
install_github('ramnathv/rCharts@dev')
