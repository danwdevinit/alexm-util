getwd()
wd <- "/Users/Alex/R_work"
setwd(wd)

#Creating functions
words.function <- function(x)
{ print(x)
  
}

words.function("Hello World")

easy.function <- function(x, y)
{
  x+y
}

easy.function(5,8)

easy2.function <- function(x,y)
{
  (x-y)/y
}
easy2.function(4,5)

easy3.function <- function(x,y,z)
{
  ((x-y)/y)/z
}
easy3.function(1,2,3)

find.mean <- function(x,y)
{
  a <- colSums(x[,y,drop=FALSE])
  b <- nrow(x)
  mean <- a/b
}
mtcars$meanmpg <- find.mean(mtcars,1)
mtcars$meanhorse <- find.mean(mtcars,4)

diffmeans <- function(x,y,z)
{
  mean1 <-colSums(x[,y,drop=FALSE])/nrow(x)
  mean2 <-colSums(x[,z,drop=FALSE])/nrow(x)
  diffmeans <- mean1/mean2
}
mtcars$diffmean <- diffmeans(mtcars,1,4)

library(plyr)
df <- read.csv("Journal Article Database 1.0.csv")

table1 <- list(table(df$Paradigm))
dt <- as.data.frame(table1)
#ddply needs something to group
dt$Count <- 1
dt <- ddply(.data = dt, .variables = .(Count), mutate,
            Proportion = Freq/sum(Freq))
View(dt)
dt <- subset(dt,select = c(Var1, Freq, Proportion))

top.line <- function(x)
{
  table1 <- list(table(x))
  df <- as.data.frame(table1)
  df$Count <- 1
  df <- ddply(.data = df, .variables = .(Count), mutate, Proportion = Freq/sum(Freq))
  df <- subset(df,select = c(x, Freq, Proportion))
}

toplineParadigm <- top.line(df$Paradigm)

#For loop
for(j in 1:ncol(mtcars))
{
  print(c(max(mtcars[,j])))
}

#Simple histogram
p <- ncol(mtcars)
X11()
par(mfrow = c(5,3))
for(j in 1:p)
{
  hist(mtcars[,j])
}

X11()
hist(mtcars$mpg, col="#123874",
     main = "Histogram of Miles Per Gallon",
     xlab = "Miles Per Gallon")

#Density plot
d <- density(mtcars$mpg)
X11()
plot(d, main = "Kernal Density of Miles Per Gallon")
polygon(d, col = "#123874", border = "orange")

#Bar plot
counts <- table(mtcars$gear)
View(counts)
X11()
barplot(counts, main="Car Distribution",
        xlab="Number of Gears",
        horiz = TRUE)

#Box Plot
X11()
boxplot(mpg~cyl, data=mtcars, main = "Car Mileage Data",
        xlab = "Number of Cylinders", ylab ="Miles per Gallon")

X11()
boxplot(wt~gear, data=mtcars, main= "Car Data", xlab = "Gears", ylab = "Weight")

#Violin Plot
install.packages("vioplot")
library(vioplot)

x1 <- mtcars$mpg[mtcars$cyl==4]
x2 <- mtcars$mpg[mtcars$cyl==6]
x3 <- mtcars$mpg[mtcars$cyl==8]

X11()
vioplot(x1,x2,x3, names = c("4 cyl", "6 cyl", "8 cyl"),
        col = "gold")