#install.packages("jsonlite")
#install.packages("curl")
#install.packages("plyr")
library(jsonlite)
wd <- "C:/Users/alexm/Documents/"
setwd(wd)

#years = c(2000:2015)
years = c(2015)

####Meta-data####
root <- "http://fts.unocha.org/api/v1/"
countries <- fromJSON(paste(root,"country.json",sep=""))
sectors <- fromJSON(paste(root,"sector.json",sep=""))
organizations <- fromJSON(paste(root,"organization.json",sep=""))
emergencies <- fromJSON(paste(root,"Emergency/year/",years[1],".json",sep=""))
if(length(years)>1){
  for(i in 2:length(years)){
    year <- years[i]
    emergencies <- rbind(emergencies, fromJSON(paste(root,"Emergency/year/",year,".json",sep="")))
    print(paste("Pulling emergencies for year ==",year))
  }
}
appeals <- fromJSON(paste(root,"Appeal/year/",years[1],".json",sep=""))
if(length(years)>1){
  for(i in 2:length(years)){
    year <- years[i]
    appeals <- rbind(appeals, fromJSON(paste(root,"Appeal/year/",year,".json",sep="")))
    print(paste("Pulling appeals for year ==",year))
  }
}

####Projects####
projects <- fromJSON(paste(root,"Project/appeal/",appeals$id[1],".json",sep=""))
for(i in 2:nrow(appeals)){
  projects <- rbind(projects, fromJSON(paste(root,"Project/appeal/",appeals$id[i],".json",sep="")))
  print(paste("Pulling projects for appeal ==",appeals$id[i]))
}

####Contributions####
#contrib_appeal <- fromJSON(paste(root,"Contribution/appeal/",appeals$id[1],".json",sep=""))
#for(i in 2:nrow(appeals)){
#  contrib_appeal <- rbind(contrib_appeal, fromJSON(paste(root,"Contribution/appeal/",appeals$id[i],".json",sep="")))
#  print(paste("Pulling contributions for appeal ==",appeals$id[i]))
#}
contrib_emerg <- fromJSON(paste(root,"Contribution/emergency/",emergencies$id[1],".json",sep=""))
for(i in 2:nrow(emergencies)){
  contrib_emerg <- rbind(contrib_emerg, fromJSON(paste(root,"Contribution/emergency/",emergencies$id[i],".json",sep="")))
  print(paste("Pulling contributions for emergency ==",emergencies$id[i]))
}

####Save####
#setwd(paste(wd,"FTS-Complete",sep=""))
#write.csv(countries,"./countries.csv",row.names=FALSE,na="")
#write.csv(sectors,"./sectors.csv",row.names=FALSE,na="")
#write.csv(organizations,"./organizations.csv",row.names=FALSE,na="")
#write.csv(emergencies,"./emergencies.csv",row.names=FALSE,na="")
#write.csv(appeals,"./appeals.csv",row.names=FALSE,na="")
#write.csv(projects,"./projects.csv",row.names=FALSE,na="")
#write.csv(contrib_appeal,"./contrib_appeal.csv",row.names=FALSE,na="")
#write.csv(contrib_emerg,"./contrib_emerg.csv",row.names=FALSE,na="")

####Merge Contributions to Projects####
colnames(appeals) <- paste("appeal",colnames(appeals),sep=".")
colnames(emergencies) <- paste("emergency",colnames(emergencies),sep=".")
colnames(projects) <- paste("project",colnames(projects),sep=".")
colnames(contrib_emerg) <- paste("contrib",colnames(contrib_emerg),sep=".")

projectContrib <- merge(contrib_emerg
                 ,projects
                 ,by.x=c("contrib.project_code")
                 ,by.y=c("project.code")
                 ,all.y=TRUE)
projectContrib <- ddply(projectContrib,.(contrib.project_code),function(x){
  pledges = 0
  commited = 0
  contributed = 0
  for(i in 1:nrow(x)){
    if(!is.na(x$contrib.status[i])){
      if(x$contrib.status[i]=="Pledge"){
        if(class(x$contrib.amount[i])=="numeric"){
          pledges <- pledges+x$contrib.amount[i]
        }
      }else if(x$contrib.status[i]=="Commitment"){
        if(class(x$contrib.amount[i])=="numeric"){
          commited <- commited+x$contrib.amount[i]
        }
      }else if(x$contrib.status[i]=="Paid contribution"){
        if(class(x$contrib.amount[i])=="numeric"){
          contributed <- contributed+x$contrib.amount[i]
        }
      }
    }
  }
  keep <- colnames(contrib)[which(substr(colnames(contrib),1,2)=="pr")]
  y <- x[keep][1,]
  y$pledges <- pledges
  y$commited <- commited
  y$contributed <- contributed
  return(y)
})
appealEmerg <- merge(appeals
                    ,emergencies
                    ,by.x=c("appeal.emergency_id")
                    ,by.y=c("emergency.id")
                    ,all.x=TRUE)
projectContrib <- merge(projectContrib
                    ,appealEmerg
                    ,by.x=c("project.appeal_id")
                    ,by.y=c("appeal.id")
                    ,all.x=TRUE)
names(projectContrib)[names(projectContrib) == "contrib.project_code"] <- "project.code"

####Merge data to contributions####
contrib <- merge(contrib_emerg
                  ,projects
                  ,by.x=c("contrib.project_code")
                  ,by.y=c("project.code")
                  ,all.x=TRUE)
contrib <- merge(contrib
                  ,appeals
                  ,by.x=c("contrib.appeal_id")
                  ,by.y=c("appeal.id")
                  ,all.x=TRUE)
contrib <- merge(contrib
                 ,emergencies
                 ,by.x=c("contrib.emergency_id")
                 ,by.y=c("emergency.id")
                 ,all.x=TRUE)