#install.packages('plyr')
library(plyr)

####Data####

wd <- "C:/Users/alexm/Documents/Rwork"
setwd(wd)

#Import grades
data <- read.csv("Uganda Primary Leaving Exam Results 2014.csv"
                 , header = TRUE
                 ,sep=","
                 ,na.strings=""
                 ,check.names=TRUE
                 ,as.is=T
)
#Passing scores
pass <- c("1","2","3","4")

#Create a new variable, TRUE for pass, FALSE for fail
schoolScores <- transform(data,indivPass=(DIV %in% pass))
#Pivot by school and calculate the number of passes/fails
schoolScores <- ddply(schoolScores,.(SCHOOL,DISTRICT)
                      ,summarize
                      ,pass=sum(indivPass==TRUE)
                      ,fail=sum(indivPass==FALSE)
                      ,male=sum(M.F=="M",na.rm=TRUE)
                      ,female=sum(M.F=="F",na.rm=TRUE)
                      ,malePass = sum(indivPass==TRUE & M.F=="M",na.rm=TRUE)
                      ,femalePass = sum(indivPass==TRUE & M.F=="F",na.rm=TRUE)
                      )
#Calculate the total number of students
schoolScores  <- transform(schoolScores,students=pass+fail)

#From total number of students, calculate percentages
schoolData <- transform(schoolScores
                        ,passRate=pass/students
                        ,percentFemale = female/students
                        ,malePassRate=malePass/male
                        ,femalePassRate=femalePass/female
                        ,logStu = log(students))
#Normal distribution censored at 100%
hist(schoolData$passRate)
#Normal distribution
hist(schoolData$percentFemale)
#Definitely not a random relationship
plot(passRate~percentFemale,schoolData)
#Is it an artifact of schools with few students? Nope
plot(passRate~percentFemale,subset(schoolData,students>=25))
#Even just looking at the female pass rate is weird...
plot(femalePassRate~percentFemale,schoolData)

####Test of randomly distributed normal/lognormal variables####

#Set 600,000 observations
obs <- 600000

#Grab mean and standard deviation of test scores
test <- transform(data,numAgg=as.numeric(AGG))
test$numAgg[which(is.na(test$numAgg))] <- 37
meanAgg <- mean(test$numAgg)
sdAgg <- sd(test$numAgg)
maxAgg <- max(test$numAgg)
minAgg <- min(test$numAgg)

#Generate a censored normal dist for scores
score <- rnorm(obs,meanAgg,sdAgg)
score[which(score>maxAgg)]<-maxAgg
score[which(score<minAgg)]<-minAgg

#Random gender
gender <- logical(obs)
for(i in 1:obs){
  gender[i]<-sample(c(TRUE,FALSE),1)
}

#Randomly group them into schools
meanStudents <- mean(schoolData$logStu)
sdStudents <- sd(schoolData$logStu)
maxStudents <- max(schoolData$logStu)
minStudents <- min(schoolData$logStu)
lastIndex <- 1
schoolNumber <- 1
school <- numeric(obs)

while(school[obs]==0){
  logNormStudents <- rnorm(1,meanStudents,sdStudents)
  logNormStudents[which(logNormStudents>maxStudents)] <- maxStudents
  logNormStudents[which(logNormStudents<minStudents)] <- minStudents
  logNormalSchool <- round(exp(logNormStudents))
  for(i in lastIndex:(lastIndex+logNormalSchool)){
    school[i]<-schoolNumber
  }
  lastIndex <- lastIndex+logNormalSchool+1
  schoolNumber <- schoolNumber+1
}
school <- school[1:obs]

testData <- data.frame(score,gender,school)

testSchoolData <- ddply(testData
                        ,.(school)
                        ,summarize
                        ,pass=sum(score<=34)
                        ,fail=sum(score>34)
                        ,female=sum(gender))

testSchoolData  <- transform(testSchoolData,students=pass+fail)
testSchoolData <- transform(testSchoolData
                            ,passRate=pass/students
                            ,percentFemale = female/students)

plot(passRate~percentFemale,testSchoolData)
