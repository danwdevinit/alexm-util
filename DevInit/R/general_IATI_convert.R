require(XML)

wd <- "D:/Documents/Data/Nepal/R/Christian Aid Ireland"
setwd(wd)

repOrg <- "XI-IATI-WHS-NEPAL"
repOrgName <- "Nepal data for WHS"

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

locatList <- list(
    "Gorkha" = list(
      attrs = c(
        ref="geoname_id"
        )
      ,"location-id" = list(
        attrs = c(
            code="1283379"
            ,vocabulary="GEO"
          )
        )
      ,name = "Gorkha"
      ,exactness = list(
        attrs = c(
          code="1"
          )
        )
      ,"location-class" = list(
        attrs = c(
          code="1"
          )
        )
      ,coordinates = list(
        attrs = c(
          latitude="28.33333"
          ,longitude="84.83333"
          )
        )
      )
    ,"Sindhupalchok" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="7647006"
          ,vocabulary="GEO"
        )
      )
      ,name = "Sindhupalchok"
      ,exactness = list(
        attrs = c(
          code="1"
        )
      )
      ,"location-class" = list(
        attrs = c(
          code="1"
        )
      )
      ,coordinates = list(
        attrs = c(
          latitude="27.82936"
          ,longitude="85.54504"
        )
      )
    )
    ,"Dolakha" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="1283431"
          ,vocabulary="GEO"
        )
      )
      ,name = "Dolakha"
      ,exactness = list(
        attrs = c(
          code="1"
        )
      )
      ,"location-class" = list(
        attrs = c(
          code="1"
        )
      )
      ,coordinates = list(
        attrs = c(
          latitude="27.83333"
          ,longitude="86.25"
        )
      )
    )
    ,"Sindhuli" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="1282776"
          ,vocabulary="GEO"
        )
      )
      ,name = "Sindhuli"
      ,exactness = list(
        attrs = c(
          code="1"
        )
      )
      ,"location-class" = list(
        attrs = c(
          code="1"
        )
      )
      ,coordinates = list(
        attrs = c(
          latitude="27.25"
          ,longitude="86.0"
        )
      )
    )
    ,"Bhaktapur" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="1283618"
          ,vocabulary="GEO"
        )
      )
      ,name = "Bhaktapur"
      ,exactness = list(
        attrs = c(
          code="1"
        )
      )
      ,"location-class" = list(
        attrs = c(
          code="1"
        )
      )
      ,coordinates = list(
        attrs = c(
          latitude="27.66734"
          ,longitude="85.41673"
        )
      )
    )
    ,"Kalimati" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="10276890"
          ,vocabulary="GEO"
        )
      )
      ,name = "Kalimati"
      ,exactness = list(
        attrs = c(
          code="1"
        )
      )
      ,"location-class" = list(
        attrs = c(
          code="1"
        )
      )
      ,coordinates = list(
        attrs = c(
          latitude="27.69882"
          ,longitude="85.29704"
        )
      )
    )
    ,"Lamjung" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="1283151"
          ,vocabulary="GEO"
        )
      )
      ,name = "Lamjung"
      ,exactness = list(
        attrs = c(
          code="1"
        )
      )
      ,"location-class" = list(
        attrs = c(
          code="1"
        )
      )
      ,coordinates = list(
        attrs = c(
          latitude="28.25"
          ,longitude="84.41667"
        )
      )
    )
    ,"Tandrang" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="7799402"
          ,vocabulary="GEO"
        )
      )
      ,name = "Tandrang"
      ,exactness = list(
        attrs = c(
          code="1"
        )
      )
      ,"location-class" = list(
        attrs = c(
          code="1"
        )
      )
      ,coordinates = list(
        attrs = c(
          latitude="28.0337"
          ,longitude="84.7752"
        )
      )
    )
    ,"Thumi" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="7799474"
          ,vocabulary="GEO"
        )
      )
      ,name = "Thumi"
      ,exactness = list(
        attrs = c(
          code="1"
        )
      )
      ,"location-class" = list(
        attrs = c(
          code="1"
        )
      )
      ,coordinates = list(
        attrs = c(
          latitude="28.1314"
          ,longitude="84.8216"
        )
      )
    )
    ,"Manbu" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="7799245"
          ,vocabulary="GEO"
        )
      )
      ,name = "Manbu"
      ,exactness = list(
        attrs = c(
          code="1"
        )
      )
      ,"location-class" = list(
        attrs = c(
          code="1"
        )
      )
      ,coordinates = list(
        attrs = c(
          latitude="28.1475"
          ,longitude="84.9167"
        )
      )
    )
    ,"Aaruarbang" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="7798888"
          ,vocabulary="GEO"
        )
      )
      ,name = "Aaruarbang"
      ,exactness = list(
        attrs = c(
          code="1"
        )
      )
      ,"location-class" = list(
        attrs = c(
          code="1"
        )
      )
      ,coordinates = list(
        attrs = c(
          latitude="28.0922"
          ,longitude="84.7985"
        )
      )
    )
    ,"Aarupokhari" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="7798900"
          ,vocabulary="GEO"
        )
      )
      ,name = "Aarupokhari"
      ,exactness = list(
        attrs = c(
          code="1"
        )
      )
      ,"location-class" = list(
        attrs = c(
          code="1"
        )
      )
      ,coordinates = list(
        attrs = c(
          latitude="28.0618"
          ,longitude="84.776"
        )
      )
    )
    ,"Dhawa" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="7799009"
          ,vocabulary="GEO"
        )
      )
      ,name = "Dhawa"
      ,exactness = list(
        attrs = c(
          code="1"
        )
      )
      ,"location-class" = list(
        attrs = c(
          code="1"
        )
      )
      ,coordinates = list(
        attrs = c(
          latitude="28.0134"
          ,longitude="84.7823"
        )
      )
    )
    ,"Aaruchanuate" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="7798892"
          ,vocabulary="GEO"
        )
      )
      ,name = "Aruchunaute"
      ,exactness = list(
        attrs = c(
          code="1"
        )
      )
      ,"location-class" = list(
        attrs = c(
          code="1"
        )
      )
      ,coordinates = list(
        attrs = c(
          latitude="28.0689"
          ,longitude="84.8136"
        )
      )
    )
    ,"Bocha" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="7801797"
          ,vocabulary="GEO"
        )
      )
      ,name = "Bocha"
      ,exactness = list(
        attrs = c(
          code="1"
        )
      )
      ,"location-class" = list(
        attrs = c(
          code="1"
        )
      )
      ,coordinates = list(
        attrs = c(
          latitude="27.69288"
          ,longitude="85.98859"
        )
      )
    )
    ,"Dandakharka" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="7801801"
          ,vocabulary="GEO"
        )
      )
      ,name = "Dandakharka"
      ,exactness = list(
        attrs = c(
          code="1"
        )
      )
      ,"location-class" = list(
        attrs = c(
          code="1"
        )
      )
      ,coordinates = list(
        attrs = c(
          latitude="27.65941"
          ,longitude="84.91963"
        )
      )
    )
    ,"Dudhpokhari" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="7801395"
          ,vocabulary="GEO"
        )
      )
      ,name = "Dudhpokhari"
      ,exactness = list(
        attrs = c(
          code="1"
        )
      )
      ,"location-class" = list(
        attrs = c(
          code="1"
        )
      )
      ,coordinates = list(
        attrs = c(
          latitude="27.61009"
          ,longitude="85.92159"
        )
      )
    )
    ,"Ghangsukathokar" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="7800623"
          ,vocabulary="GEO"
        )
      )
      ,name = "Ghangsukathokar"
      ,exactness = list(
        attrs = c(
          code="1"
        )
      )
      ,"location-class" = list(
        attrs = c(
          code="1"
        )
      )
      ,coordinates = list(
        attrs = c(
          latitude="27.55942"
          ,longitude="86.04689"
        )
      )
    )
    ,"Katakuti" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="7801804"
          ,vocabulary="GEO"
        )
      )
      ,name = "Katakuti"
      ,exactness = list(
        attrs = c(
          code="1"
        )
      )
      ,"location-class" = list(
        attrs = c(
          code="1"
        )
      )
      ,coordinates = list(
        attrs = c(
          latitude="27.62756"
          ,longitude="85.9923"
        )
      )
    )
    ,"Lakuridada" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="7801807"
          ,vocabulary="GEO"
        )
      )
      ,name = "Lakuridada"
      ,exactness = list(
        attrs = c(
          code="1"
        )
      )
      ,"location-class" = list(
        attrs = c(
          code="1"
        )
      )
      ,coordinates = list(
        attrs = c(
          latitude="27.68601"
          ,longitude="85.9619"
        )
      )
    )
    ,"Lapilang" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="7800633"
          ,vocabulary="GEO"
        )
      )
      ,name = "Lapilang"
      ,exactness = list(
        attrs = c(
          code="1"
        )
      )
      ,"location-class" = list(
        attrs = c(
          code="1"
        )
      )
      ,coordinates = list(
        attrs = c(
          latitude="27.74062"
          ,longitude="86.09881"
        )
      )
    )
    ,"Magapauwa" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="7800634"
          ,vocabulary="GEO"
        )
      )
      ,name = "Magapauwa"
      ,exactness = list(
        attrs = c(
          code="1"
        )
      )
      ,"location-class" = list(
        attrs = c(
          code="1"
        )
      )
      ,coordinates = list(
        attrs = c(
          latitude="27.65511"
          ,longitude="85.99366"
        )
      )
    )
    ,"Phasku" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="10323632"
          ,vocabulary="GEO"
        )
      )
      ,name = "Phasku"
      ,exactness = list(
        attrs = c(
          code="1"
        )
      )
      ,"location-class" = list(
        attrs = c(
          code="1"
        )
      )
      ,coordinates = list(
        attrs = c(
          latitude="27.61424"
          ,longitude="86.02946"
        )
      )
    )
    ,"Pawati" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="7800639"
          ,vocabulary="GEO"
        )
      )
      ,name = "Pawati"
      ,exactness = list(
        attrs = c(
          code="1"
        )
      )
      ,"location-class" = list(
        attrs = c(
          code="1"
        )
      )
      ,coordinates = list(
        attrs = c(
          latitude="27.59018"
          ,longitude="86.06369"
        )
      )
    )
    ,"Sailungeswor" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="7800640"
          ,vocabulary="GEO"
        )
      )
      ,name = "Sailungeswor"
      ,exactness = list(
        attrs = c(
          code="1"
        )
      )
      ,"location-class" = list(
        attrs = c(
          code="1"
        )
      )
      ,coordinates = list(
        attrs = c(
          latitude="27.58287"
          ,longitude="86.0206"
        )
      )
    )
    ,"Susmachhemawati" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="7800641"
          ,vocabulary="GEO"
        )
      )
      ,name = "Susmachhemawati"
      ,exactness = list(
        attrs = c(
          code="1"
        )
      )
      ,"location-class" = list(
        attrs = c(
          code="1"
        )
      )
      ,coordinates = list(
        attrs = c(
          latitude="27.70497"
          ,longitude="86.04809"
        )
      )
    )
    ,"Sundrawati" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="7801812"
          ,vocabulary="GEO"
        )
      )
      ,name = "Sundrawati"
      ,exactness = list(
        attrs = c(
          code="1"
        )
      )
      ,"location-class" = list(
        attrs = c(
          code="1"
        )
      )
      ,coordinates = list(
        attrs = c(
          latitude="27.71434"
          ,longitude="86.07367"
        )
      )
    )
    ,"Sunkhani" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="7800249"
          ,vocabulary="GEO"
        )
      )
      ,name = "Sunkhani"
      ,exactness = list(
        attrs = c(
          code="1"
        )
      )
      ,"location-class" = list(
        attrs = c(
          code="1"
        )
      )
      ,coordinates = list(
        attrs = c(
          latitude="27.70059"
          ,longitude="86.10729"
        )
      )
    )
    ,"Kavre" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="1283282"
          ,vocabulary="GEO"
        )
      )
      ,name = "Kavre"
      ,exactness = list(
        attrs = c(
          code="1"
        )
      )
      ,"location-class" = list(
        attrs = c(
          code="1"
        )
      )
      ,coordinates = list(
        attrs = c(
          latitude="27.58333"
          ,longitude="85.66667"
        )
      )
    )
    ,"Rasuwa" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="1282859"
          ,vocabulary="GEO"
        )
      )
      ,name = "Rasuwa"
      ,exactness = list(
        attrs = c(
          code="1"
        )
      )
      ,"location-class" = list(
        attrs = c(
          code="1"
        )
      )
      ,coordinates = list(
        attrs = c(
          latitude="28.16667"
          ,longitude="85.33333"
        )
      )
    )
    ,"Nuwakot" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="1282973"
          ,vocabulary="GEO"
        )
      )
      ,name = "Nuwakot"
      ,exactness = list(
        attrs = c(
          code="1"
        )
      )
      ,"location-class" = list(
        attrs = c(
          code="1"
        )
      )
      ,coordinates = list(
        attrs = c(
          latitude="27.91667"
          ,longitude="85.25"
        )
      )
    )
    ,"Ramechhap" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="1282879"
          ,vocabulary="GEO"
        )
      )
      ,name = "Ramechhap"
      ,exactness = list(
        attrs = c(
          code="1"
        )
      )
      ,"location-class" = list(
        attrs = c(
          code="1"
        )
      )
      ,coordinates = list(
        attrs = c(
          latitude="27.41667"
          ,longitude="86.08333"
        )
      )
    )
    ,"Dhading" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="1283472"
          ,vocabulary="GEO"
        )
      )
      ,name = "Dhading"
      ,exactness = list(
        attrs = c(
          code="1"
        )
      )
      ,"location-class" = list(
        attrs = c(
          code="1"
        )
      )
      ,coordinates = list(
        attrs = c(
          latitude="28.0"
          ,longitude="84.91667"
        )
      )
    )
    ,"Makawanpur" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="10330199"
          ,vocabulary="GEO"
        )
      )
      ,name = "Makawanpur"
      ,exactness = list(
        attrs = c(
          code="1"
        )
      )
      ,"location-class" = list(
        attrs = c(
          code="1"
        )
      )
      ,coordinates = list(
        attrs = c(
          latitude="27.41429"
          ,longitude="85.17962"
        )
      )
    )
    ,"Okhaldhunga" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="7801037"
          ,vocabulary="GEO"
        )
      )
      ,name = "Okhaldhunga"
      ,exactness = list(
        attrs = c(
          code="1"
        )
      )
      ,"location-class" = list(
        attrs = c(
          code="1"
        )
      )
      ,coordinates = list(
        attrs = c(
          latitude="27.31643"
          ,longitude="86.50508"
        )
      )
    )
    ,"Lalitpur" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="1283154"
          ,vocabulary="GEO"
        )
      )
      ,name = "Lalitpur"
      ,exactness = list(
        attrs = c(
          code="1"
        )
      )
      ,"location-class" = list(
        attrs = c(
          code="1"
        )
      )
      ,coordinates = list(
        attrs = c(
          latitude="27.58333"
          ,longitude="85.33333"
        )
      )
    )
    ,"Kathmandu" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="1283241"
          ,vocabulary="GEO"
        )
      )
      ,name = "Kathmandu"
      ,exactness = list(
        attrs = c(
          code="1"
        )
      )
      ,"location-class" = list(
        attrs = c(
          code="1"
        )
      )
      ,coordinates = list(
        attrs = c(
          latitude="27.70704"
          ,longitude="85.33218"
        )
      )
    )
    ,"Dhading" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="1283472"
          ,vocabulary="GEO"
        )
      )
      ,name = "Dhading"
      ,exactness = list(
        attrs = c(
          code="1"
        )
      )
      ,"location-class" = list(
        attrs = c(
          code="1"
        )
      )
      ,coordinates = list(
        attrs = c(
          latitude="28.0"
          ,longitude="84.91667"
        )
      )
    )
)

listToXML <- function(node, sublist){
  for(i in 1:length(sublist)){
    if(length(names(sublist)[i])>0){
      if(names(sublist)[i]=="attrs"){
        xmlAttrs(node) <- sublist[[i]]
      }else if(names(sublist)[i]=="text"){
        if(is.null(sublist[[i]])){
          xmlValue(node) <- ""
        } else if(is.na(sublist[[i]])){
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
          if(is.null(sublist[[i]])){
            xmlValue(child) <- ""
          }else if(is.na(sublist[[i]])){
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
    attrs=c(ref=repOrg,type="80","secondary-reporter"="1")
    ,narrative=list(
      attrs=c("xml:lang"="en")
      ,text=repOrgName
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
    attrs=c(ref=activity$Participating.organisation,role="3")
    ,narrative=list(
      attrs=c("xml:lang"="en")
      ,text=activity$Participating.organisation
    )
  )
  activityList[['activity-status']] <- list(attrs=c(code=statusList[[activity$Project.status]]))
  activityList <- c(activityList
    ,list(
      "activity-date" = list(
        attrs=c(type="2","iso-date"=activity$Start.date)
        ,narrative=list(attrs=c("xml:lang"="en"))
      )
      ,"activity-date" = list(
        attrs=c(type="4","iso-date"=activity$End.date)
        ,narrative=list(attrs=c("xml:lang"="en"))
      )
    )
  )
  activityList[['contact-info']] <- list(
    attrs=c(type=activity$Contact.type)
    ,organisation = list(
      narrative = list(
        attrs=c("xml:lang"="en")
        ,text=activity$Contact.organisation
      )  
    )
    ,department = list(
      narrative = list(
        attrs=c("xml:lang"="en")
        ,text=activity$Contact.department
      )  
    )
    ,"person-name" = list(
      narrative = list(
        attrs=c("xml:lang"="en")
        ,text=activity$Contact.name
      )  
    )
    ,"job-title" = list(
      narrative = list(
        attrs=c("xml:lang"="en")
        ,text=activity$Contact.title
      )  
    )
    ,telephone = activity$Contact.telephone
    ,email = activity$Contact.email
    ,website = activity$Contact.website
    ,"mailing-address" = list(
      narrative = list(
        attrs=c("xml:lang"="en")
        ,text=activity$Contact.address
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
    attrs=c(vocabulary="99",code="")
    ,narrative = list(
      attrs=c("xml:lang"="en")
      ,text=transaction$Cluster
    )
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
      attrs=c(vocabulary="99",code="")
      ,narrative = list(
        attrs=c("xml:lang"="en")
        ,text=transaction$Sector
      )
    )
    transactionList[['finance-type']] <- list(
      attrs=c(code=transaction$Finance.type)
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
  activityList[['result']] <- list(
    attrs=c(type="","aggregation-status"="false")
    ,title = list(
      narrative = list(
        attrs=c("xml:lang"="en")
        ,text=""
      )  
    )
    ,description = list(
      narrative = list(
        attrs=c("xml:lang"="en")
        ,text=activity$Project.result
      )  
    )
    ,indicator = list(
      attrs = c(
        measure=""
        ,ascending="1"
      )
      ,title= list(
        narrative = list(
          attrs=c("xml:lang"="en")
          ,text=""
        )
      )
    )
  )
  
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
#         , encoding = "utf8"
        )


