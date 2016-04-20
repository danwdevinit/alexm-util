require(XML)

wd <- "D:/Documents/Data"
setwd(wd)

statusList <- list(
  "1" = "Pipeline/identification"
  ,"2" = "Implementation"
  ,"3" = "Completion"
  ,"4" = "Post-completion"
  ,"5" = "Cancelled"
  ,"6" = "Suspended"
)

transTypeList <- list(
  "1" = "Incoming Funds"
  ,"2" = "Commitment"
  ,"3" = "Disbursement"
  ,"4" = "Expenditure"
  ,"5" = "Interest Repayment"
  ,"6" = "Loan Repayment"
  ,"7" = "Reimbursement"
  ,"8" = "Purchase of Equity"
  ,"9" = "Sale of Equity"
  ,"10" = "Credit Guarantee"
)

# df <- xmlToDataFrame("iatiActivity_Nepal.xml")
data <- xmlParse("iatiActivity_Nepal.xml")
xml_data <- xmlToList(data)

dataList <- list()
dataIndex <- 1
transactionList <- list()
transactionIndex <- 1


xml_names <- names(xml_data)
for(i in 1:length(xml_names)){
  name <- xml_names[i]
  if(name=="iati-activity"){
    dataObj <- list()
    activity <- xml_data[[i]]
    activity_names <- names(activity)
    dataObj$id <- activity['iati-identifier'][[1]]
    dataObj$reporting <- activity['reporting-org'][[1]]$narrative$text
    dataObj$title <- activity$title$narrative$text
    dataObj$description <- activity$description$narrative$text
    dataObj$status <- statusList[activity["activity-status"][[1]]][[1]]
    dataObj$budget_start <- activity["budget"][[1]]["period-start"][[1]]
    dataObj$budget_end <- activity["budget"][[1]]["period-end"][[1]]
    dataObj$budget_amt <- activity["budget"][[1]]$value$text
    dataObj$budget_currency <- activity["budget"][[1]]$value$.attrs[1]
    dataObj$location <- activity$location$description$narrative$text
    dataObj$result <- activity$result$description$narrative$text
    dataObj$contact_org <- activity$`contact-info`$organisation$narrative$text
    dataObj$contact_email <- activity$`contact-info`$email
    parent_id <- ""
    planned_start <- ""
    start <- ""
    planned_end <- ""
    end <- ""
    for(j in 1:length(activity_names)){
      activity_name <- activity_names[j]
      attribute <- activity[[j]]
      if(activity_name=="related-activity"){
        type <- attribute[2]
        ref <- attribute[1]
        if(type==1){
          parent_id <- ref
        }
      }
      if(activity_name=="activity-date"){
        type <- attribute$.attrs[1]
        date <- attribute$.attrs[2]
        if(type=="1"){
          planned_start <- date
        }
        if(type=="2"){
          start <- date
        }
        if(type=="3"){
          planned_end <- date
        }
        if(type=="4"){
          end <- date
        }
      }
      if(activity_name=="transaction"){
        transactionObj <- list()
        transactionObj['project_id'] <- activity['iati-identifier'][[1]]
        transactionObj['trans_provider'] <- attribute['provider-org'][[1]]$narrative$text
        transactionObj['trans_receiver'] <- attribute['receiver-org'][[1]]$narrative$text
        transactionObj['trans_type'] <- transTypeList[attribute['transaction-type'][[1]]][[1]]
        transactionObj['trans_sector'] <- attribute$sector$.attrs[2]
        transactionObj['trans_sector_code'] <- attribute$sector$.attrs[1]
        transactionObj['trans_value'] <- attribute$value$text
        transactionObj['trans_currency'] <- attribute$value$.attrs[1]
        transactionObj['trans_date'] <- attribute['transaction-date'][[1]]
        transactionObj['trans_desc'] <- trimws(attribute$description$narrative$text)
        transactionList[[transactionIndex]] <-  transactionObj
        transactionIndex <- transactionIndex + 1
      }
    }
    dataObj$parent_id <- parent_id
    dataObj$planned_start <- planned_start
    dataObj$start <- start
    dataObj$planned_end <- planned_end
    dataObj$end <- end
    dataList[[dataIndex]] <- dataObj
    dataIndex <- dataIndex + 1
  }
}

df  <-  as.data.frame(t(matrix(unlist(dataList), nrow=length(unlist(dataList[1])))))
dt  <-  as.data.frame(t(matrix(unlist(transactionList), nrow=length(unlist(transactionList[1])))))
dataNames <- c("Project ID","Reporting organisation","Project title","Project description"
               ,"Project status","Budget period start","Budget period end","Budget amount"
               ,"Budget currency","Project location","Project result","Contact organisation"
               ,"Contact email","Parent project ID","Planned start date","Start date"
               ,"Planned end date","End date"
)
transactionNames <- c("Project ID","Provider","Receiver","Transaction type","Sector"
                      ,"Sector vocabulary code","Value","Currency"
                      ,"Transaction date","Description")
names(df) <- dataNames
names(dt) <- transactionNames
write.csv(df,"activities.csv",row.names=FALSE,na="")
write.csv(dt,"transactions.csv",row.names=FALSE,na="")
