require(XML)

wd <- "D:/Documents/Data/Nepal/"
setwd(wd)

repOrg <- "XI-IATI-WHS-NEPAL"

statusList <- list(
  "Pipeline/identification" = "1"
  ,"Implementation" = "2"
  ,"Completion" = "3"
  ,"Post-completion" = "4"
  ,"Cancelled" = "5"
  ,"Suspended" = "6"
)

transTypeList <- list(
  "Incoming Funds" = "1"
  ,"Commitment" = "2"
  ,"Disbursement" = "3"
  ,"Expenditure" = "4"
  ,"Interest Repayment" = "5"
  ,"Loan Repayment" = "6"
  ,"Reimbursement" = "7"
  ,"Purchase of Equity" = "8"
  ,"Sale of Equity" = "9"
  ,"Credit Guarantee" = "10"
)

listToXML <- function(node, sublist){
  for(i in 1:length(sublist)){
    if(length(names(sublist)[i])>0){
      if(names(sublist)[i]=="attrs"){
        xmlAttrs(node) <- sublist[[i]]
      }else if(names(sublist)[i]=="text"){
        if(is.na(sublist[[i]])){
          xmlValue(node) <- ""
        }else{
          xmlValue(node) <- sublist[[i]] 
        }
      }else {
        child <- newXMLNode(names(sublist)[i], parent=node);
        if (typeof(sublist[[i]]) == "list"){
          listToXML(child, sublist[[i]]) 
        }
        else{
          if(is.na(sublist[[i]])){
            xmlValue(child) <- ""
          }else{
            xmlValue(child) <- sublist[[i]] 
          }
        }
      }
    }
  } 
}

now <- format(Sys.time(),"%Y-%m-%dT%TZ",tz="UTC")
activityAttrs = c("last-updated-datetime"=now, "xml:lang"="en", "default-currency"="USD", "hierarchy"="1", "linked-data-uri"="")

df <- read.csv("activities.csv",na.strings="",as.is=TRUE)
dt <- read.csv("transactions.csv",na.strings="",as.is=TRUE)

li <- list()

for(i in 1:nrow(df)){
  activity <- df[i,]
  activityList <- list(attrs=activityAttrs)
  activityList[['iati-identifier']] <- paste(repOrg,activity$Project.ID,sep="-")
  activityList[['reporting-org']] <- list(
    attrs=c(ref=repOrg,type="80")
    ,narrative=list(
      attrs=c("xml:lang"="en")
      ,text=activity$Reporting.organisation
    )
  )
  activityList[['title']] <- list(
    narrative=list(
      attrs=c("xml:lang"="en")
      ,text=activity$Project.title
    )
  )
  activityList[['description']] <- list(
    narrative=list(
      attrs=c("xml:lang"="en")
      ,text=activity$Project.description
    )
  )
  activityList[['participating-org']] <- list(
    attrs=c(ref="",role="3")
    ,narrative=list(
      attrs=c("xml:lang"="en")
      ,text=activity$Reporting.organisation
    )
  )
  activityList[['activity-status']] <- list(attrs=c(code=statusList[[activity$Project.status]]))
  activityList <- c(activityList
    ,list(
      "activity-date" = list(
        attrs=c(type="1","iso-date"=activity$Planned.start.date)
        ,narrative=list(attrs=c("xml:lang"="en"))
      )
      ,"activity-date" = list(
        attrs=c(type="2","iso-date"=activity$Start.date)
        ,narrative=list(attrs=c("xml:lang"="en"))
      )
      ,"activity-date" = list(
        attrs=c(type="3","iso-date"=activity$Planned.end.date)
        ,narrative=list(attrs=c("xml:lang"="en"))
      )
      ,"activity-date" = list(
        attrs=c(type="4","iso-date"=activity$End.date)
        ,narrative=list(attrs=c("xml:lang"="en"))
      )
    )
  )
  activityList[['contact-info']] <- list(
    attrs=c(type="")
    ,organisation = list(
      narrative = list(
        attrs=c("xml:lang"="en")
        ,text=activity$Contact.organisation
      )  
    )
    ,telephone = list()
    ,email = activity$Contact.email
    ,website = list()
    ,"mailing-address" = list(
      narrative = list(
        attrs=c("xml:lang"="en")
        ,text=""
      )
    )
  )
  activityList[['recipient-country']] <- list(
    attrs=c(code="NP",percentage="100")
  )
  activityList[['location']] <- list(
    attrs=c(ref="")
    ,description = list(
      narrative = list(
        attrs=c("xml:lang"="en")
        ,text=activity$Project.location
      )  
    )
  )
  activityList[['sector']] <- list(
    attrs=c(vocabulary="",code="")  
  )
  activityList[['budget']] <- list(
    attrs=c(type="")
    ,"period-start"=list(attrs=c("iso-date"=activity$Budget.period.start))
    ,"period-end"=list(attrs=c("iso-date"=activity$Budget.period.end))
    ,"value"=list(
      attrs=c(
        "currency"=activity$Budget.currency
         ,"value-date"=activity$Budget.period.start
      )
      ,text=activity$Budget.amount
    )
  )
  
  transactions <- dt[which(dt$Project.ID==activity$Project.ID),]
  
  for(j in 1:nrow(transactions)){
    transaction <- transactions[j,]
    transactionList <- list(attrs=c(ref=transaction$Transaction.type))
    transactionList[['transaction-type']] <- list(
      attrs = c(code = transTypeList[[transaction$Transaction.type]])
    )
    transactionList[['transaction-date']] <- list(
      attrs = c("iso-date" = transaction$Transaction.date)
    )
    transactionList[['value']] <- list(
      attrs = c(
        "currency" = transaction$Currency
        ,"value-date" = transaction$Transaction.date
      )
      ,text = transaction$Value
    )
    transactionList[['description']] <- list(
      narrative=list(
        attrs=c("xml:lang"="en")
        ,text=transaction$Description
      )
    )
    transactionList[['provider-org']] <- list(
      attrs = c(ref="","provider-activity-id"="")
      ,narrative = list(
        attrs=c("xml:lang"="en")
        ,text=transaction$Provider
      )
    )
    transactionList[['receiver-org']] <- list(
      attrs = c(ref="","receiver-activity-id"="")
      ,narrative = list(
        attrs=c("xml:lang"="en")
        ,text=transaction$Receiver
      )
    )
    transactionList[['sector']] <- list(
      attrs=c(vocabulary=transaction$Sector.vocabulary,code="")
      ,narrative = list(
        attrs=c("xml:lang"="en")
        ,text=transaction$Sector
      )
    )
    activityList <- c(activityList
                      ,list(
                        "transaction" = transactionList
                      )
    )
  }
  activityList[['related-activity']] <- list(
    attrs=c(
      ref=activity$Parent.project.ID
      ,type="1"
    )
  )
# Only UNICEF seems to have this and it's causing problems   
#   activityList[['result']] <- list(
#     attrs=c(type="","aggregation-status"="false")
#     ,description = list(
#       narrative = list(
#         attrs=c("xml:lang"="en")
#         ,text=activity$Project.result
#       )  
#     )
#   )
  
  li <- c(li, list("iati-activity" = activityList))
}

root <- newXMLNode("iati-activities",attrs=c(version = "2.01","generated-datetime" = now))
#Suppress namespace warning due to multiple activities/transactions
options("suppressXMLNamespaceWarning" = TRUE)
listToXML(root, li)
saveXML(root
        , file="gha_iati.xml"
        , compression=0
        , indent=TRUE
#         , prefix = '<?xml version="1.0"?>\n'
        , encoding = getEncoding(root)
        )


