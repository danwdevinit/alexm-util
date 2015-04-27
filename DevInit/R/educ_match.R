#install.packages('stringdist')
#install.packages('plyr')
library(plyr)
library(stringdist)

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

#Import Staff
staff <- read.csv("Staff.csv"
                 , header = TRUE
                 ,sep=","
                 ,na.strings=""
                 ,check.names=TRUE
                 ,as.is=T
)

#Recode District names
staff <- staff[which(staff$Workplan=="6: Education"),]
staff$Annual.Gross.Salary <- as.numeric(gsub(",","",staff$Annual.Gross.Salary))
staff <- ddply(staff,.(District),function(x){
  annualSalary <- sum(x$Annual.Gross.Salary,na.rm=TRUE)
  teacherCount <- nrow(x)
  return(data.frame(annualSalary,teacherCount))
})
res <- character(nrow(staff))
uniqueStaff <- unique(staff$District)
for(i in 1:nrow(staff)){
  district <- staff[i,1]
  distSplit <- strsplit(district," ")[[1]]
  distLow <- distSplit[1]
  dist <- toupper(distSplit[1])
  distWordLen <- length(distSplit)
  if(distWordLen==3){
    res[i] <- paste(dist,"MUN.")
  }
  else if(distWordLen==2 & (paste(distLow,"Municipal Council") %in% uniqueStaff) ){
    res[i] <- paste(dist,"MAIN")
  }
  else{
    res[i] <- dist
  }
}
staff[,1]<- res

#Pivot Data
pass <- c("1","2","3")
districtScores <- transform(data,indivPass=(DIV %in% pass))
districtScores <- ddply(districtScores,.(DISTRICT),summarize,
                        pass=sum(indivPass==TRUE),fail=sum(indivPass==FALSE),male=sum(M.F=="M",na.rm=TRUE),female=sum(M.F=="F",na.rm=TRUE))
districtScores <- transform(districtScores,students=pass+fail)

finalData <- merge(districtScores
                   ,staff
                   ,by.x=c("DISTRICT")
                   ,by.y=c("District"))

finalData <- transform(finalData
                       ,stuTeachRatio=students/teacherCount
                       ,percentFemale = female/students
                       ,passRate=pass/students
                       ,failRate=fail/students
                       ,logStu = log(students)
                       ,logTea = log(teacherCount)
                       ,stuTeachRatiosq = (students/teacherCount)^2)

fit <- lm(passRate~percentFemale+stuTeachRatio+logStu+annualSalary+stuTeachRatiosq,finalData)
summary(fit)
#Only significant variable is # of students. M to F ratio, annual salary, stu to teach ratio all inconsequential in passRate


schoolScores <- transform(data,indivPass=(DIV %in% pass))
schoolScores <- ddply(schoolScores,.(SCHOOL,DISTRICT),summarize,
                      pass=sum(indivPass==TRUE),fail=sum(indivPass==FALSE),male=sum(M.F=="M",na.rm=TRUE),female=sum(M.F=="F",na.rm=TRUE))
schoolScores  <- transform(schoolScores,students=pass+fail)

schoolData <- transform(schoolScores
                       ,passRate=pass/students
                       ,percentFemale = female/students
                       ,logStu = log(students))
schoolData$District.f <- as.factor(schoolData$DISTRICT)
schoolData <- schoolData[complete.cases(schoolData),]

fit <- lm(passRate~District.f+logStu+percentFemale,schoolData)
summary(fit)
#When controlling for district specific effects, logStu and percentFemale are significant
fit <- lm(passRate~logStu+percentFemale,schoolData)
summary(fit)
#Even more when ignoring district effects
plot(passRate~percentFemale,schoolData)
#Interesting evidence of policies regarding gender

aggScores <- transform(data,newAGG=as.numeric(AGG),indivPass=(DIV %in% pass))
aggScores <- ddply(aggScores,.(SCHOOL,DISTRICT),summarize,
                   pass=sum(indivPass==TRUE),fail=sum(indivPass==FALSE),aggAGG=sum(newAGG,na.rm=TRUE),male=sum(M.F=="M",na.rm=TRUE),female=sum(M.F=="F",na.rm=TRUE))

aggData <- transform(aggScores,students=pass+fail)
aggData <- transform(aggData
                     ,percentFemale=female/students
                     ,avgAGG=aggAGG/students
                     ,passRate=pass/students)
plot(avgAGG~passRate,aggData)
