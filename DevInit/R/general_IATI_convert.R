require(XML)

wd <- "D:/Documents/Data/Nepal/R/"
#wd <- "~/Nepal_IATI_data_work/R/"
setwd(wd)

repOrg <- "XI-IATI-WHS-NEPAL"
repOrgName <- "Nepal Traceability Study 2016"

####Define dictionaries and functions####
validIDs <- c(
  "XM-"
  ,"AU-"
  ,"IE-"
  ,"AID"
  ,"411"
)

statusList <- list(
  "Pipeline/identification" = "1"
  ,"Implementation" = "2"
  ,"Completion" = "3"
  ,"Post-completion" = "4"
  ,"Cancelled" = "5"
  ,"Suspended" = "6"
)

transTypeList <- list(
  "incoming funds" = "1"
  ,"commitment" = "2"
  ,"disbursement" = "3"
  ,"expenditure" = "4"
  ,"interest repayment" = "5"
  ,"loan repayment" = "6"
  ,"reimbursement" = "7"
  ,"purchase of equity" = "8"
  ,"sale of equity" = "9"
  ,"credit guarantee" = "10"
)

locatList <- list(
    "Gorkha" = list(
      attrs = c(
        ref="geoname_id"
        )
      ,"location-id" = list(
        attrs = c(
            code="1283379"
            ,vocabulary="G1"
          )
        )
      ,name = list(
        narrative = list(
          attrs=c("xml:lang"="en")
          ,text="Gorkha"
          )
        )
      ,point = list(
        attrs = c(
          srsName="http://geonames.org/1283379"
          )
        ,pos = "28.33333 84.83333"
        )
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
      )
    ,"Sindhupalchok" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="7647006"
          ,vocabulary="G1"
        )
      )
      ,name = list(
        narrative = list(
          attrs=c("xml:lang"="en")
          ,text="Sindhupalchok"
        )
      )
      ,point = list(
        attrs = c(
          srsName="http://geonames.org/7647006"
        )
        ,pos = "27.82936 85.54504"
      )
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
    )
    ,"Dolakha" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="1283431"
          ,vocabulary="G1"
        )
      )
      ,name = list(
        narrative = list(
          attrs=c("xml:lang"="en")
          ,text="Dolakha"
        )
      )
      ,point = list(
        attrs = c(
          srsName="http://geonames.org/1283431"
        )
        ,pos = "27.83333 86.25"
      )
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
    )
    ,"Sindhuli" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="1282776"
          ,vocabulary="G1"
        )
      )
      ,name = list(
        narrative = list(
          attrs=c("xml:lang"="en")
          ,text="Sindhuli"
        )
      )
      ,point = list(
        attrs = c(
          srsName="http://geonames.org/1282776"
        )
        ,pos = "27.25 86.0"
      )
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
    )
    ,"Bhaktapur" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="1283618"
          ,vocabulary="G1"
        )
      )
      ,name = list(
        narrative = list(
          attrs=c("xml:lang"="en")
          ,text="Bhaktapur"
        )
      )
      ,point = list(
        attrs = c(
          srsName="http://geonames.org/1283618"
        )
        ,pos = "27.66734 85.41673"
      )
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
    )
    ,"Kalimati" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="10276890"
          ,vocabulary="G1"
        )
      )
      ,name = list(
        narrative = list(
          attrs=c("xml:lang"="en")
          ,text="Kalimati"
        )
      )
      ,point = list(
        attrs = c(
          srsName="http://geonames.org/10276890"
        )
        ,pos = "27.69882 85.29704"
      )
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
    )
    ,"Lamjung" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="1283151"
          ,vocabulary="G1"
        )
      )
      ,name = list(
        narrative = list(
          attrs=c("xml:lang"="en")
          ,text="Lamjung"
        )
      )
      ,point = list(
        attrs = c(
          srsName="http://geonames.org/1283151"
        )
        ,pos = "28.25 84.41667"
      )
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
    )
    ,"Tandrang" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="7799402"
          ,vocabulary="G1"
        )
      )
      ,name = list(
        narrative = list(
          attrs=c("xml:lang"="en")
          ,text="Tandrang"
        )
      )
      ,point = list(
        attrs = c(
          srsName="http://geonames.org/7799402"
        )
        ,pos = "28.0337 84.7752"
      )
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
    )
    ,"Thumi" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="7799474"
          ,vocabulary="G1"
        )
      )
      ,name = list(
        narrative = list(
          attrs=c("xml:lang"="en")
          ,text="Thumi"
        )
      )
      ,point = list(
        attrs = c(
          srsName="http://geonames.org/7799474"
        )
        ,pos = "28.1314 84.8216"
      )
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
    )
    ,"Manbu" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="7799245"
          ,vocabulary="G1"
        )
      )
      ,name = list(
        narrative = list(
          attrs=c("xml:lang"="en")
          ,text="Manbu"
        )
      )
      ,point = list(
        attrs = c(
          srsName="http://geonames.org/7799245"
        )
        ,pos = "28.1475 84.9167"
      )
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
    )
    ,"Aaruarbang" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="7798888"
          ,vocabulary="G1"
        )
      )
      ,name = list(
        narrative = list(
          attrs=c("xml:lang"="en")
          ,text="Aaruarbang"
        )
      )
      ,point = list(
        attrs = c(
          srsName="http://geonames.org/7798888"
        )
        ,pos = "28.0922 84.7985"
      )
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
    )
    ,"Aarupokhari" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="7798900"
          ,vocabulary="G1"
        )
      )
      ,name = list(
        narrative = list(
          attrs=c("xml:lang"="en")
          ,text="Aarupokhari"
        )
      )
      ,point = list(
        attrs = c(
          srsName="http://geonames.org/7798900"
        )
        ,pos = "28.0618 84.776"
      )
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
    )
    ,"Dhawa" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="7799009"
          ,vocabulary="G1"
        )
      )
      ,name = list(
        narrative = list(
          attrs=c("xml:lang"="en")
          ,text="Dhawa"
        )
      )
      ,point = list(
        attrs = c(
          srsName="http://geonames.org/7799009"
        )
        ,pos = "28.0134 84.7823"
      )
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
    )
    ,"Aaruchanuate" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="7798892"
          ,vocabulary="G1"
        )
      )
      ,name = list(
        narrative = list(
          attrs=c("xml:lang"="en")
          ,text="Aaruchanuate"
        )
      )
      ,point = list(
        attrs = c(
          srsName="http://geonames.org/7798892"
        )
        ,pos = "28.0689 84.8136"
      )
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
    )
    ,"Bocha" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="7801797"
          ,vocabulary="G1"
        )
      )
      ,name = list(
        narrative = list(
          attrs=c("xml:lang"="en")
          ,text="Bocha"
        )
      )
      ,point = list(
        attrs = c(
          srsName="http://geonames.org/7801797"
        )
        ,pos = "27.69288 85.98859"
      )
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
    )
    ,"Dandakharka" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="7801801"
          ,vocabulary="G1"
        )
      )
      ,name = list(
        narrative = list(
          attrs=c("xml:lang"="en")
          ,text="Dandakharka"
        )
      )
      ,point = list(
        attrs = c(
          srsName="http://geonames.org/7801801"
        )
        ,pos = "27.65941 84.91963"
      )
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
    )
    ,"Dudhpokhari" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="7801395"
          ,vocabulary="G1"
        )
      )
      ,name = list(
        narrative = list(
          attrs=c("xml:lang"="en")
          ,text="Dudhpokhari"
        )
      )
      ,point = list(
        attrs = c(
          srsName="http://geonames.org/7801395"
        )
        ,pos = "27.61009 85.92159"
      )
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
    )
    ,"Ghangsukathokar" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="7800623"
          ,vocabulary="G1"
        )
      )
      ,name = list(
        narrative = list(
          attrs=c("xml:lang"="en")
          ,text="Ghangsukathokar"
        )
      )
      ,point = list(
        attrs = c(
          srsName="http://geonames.org/7800623"
        )
        ,pos = "27.55942 86.04689"
      )
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
    )
    ,"Katakuti" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="7801804"
          ,vocabulary="G1"
        )
      )
      ,name = list(
        narrative = list(
          attrs=c("xml:lang"="en")
          ,text="Katakuti"
        )
      )
      ,point = list(
        attrs = c(
          srsName="http://geonames.org/7801804"
        )
        ,pos = "27.62756 85.9923"
      )
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
    )
    ,"Lakuridada" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="7801807"
          ,vocabulary="G1"
        )
      )
      ,name = list(
        narrative = list(
          attrs=c("xml:lang"="en")
          ,text="Lakuridada"
        )
      )
      ,point = list(
        attrs = c(
          srsName="http://geonames.org/7801807"
        )
        ,pos = "27.68601 85.9619"
      )
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
    )
    ,"Lapilang" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="7800633"
          ,vocabulary="G1"
        )
      )
      ,name = list(
        narrative = list(
          attrs=c("xml:lang"="en")
          ,text="Lapilang"
        )
      )
      ,point = list(
        attrs = c(
          srsName="http://geonames.org/7800633"
        )
        ,pos = "27.74062 86.09881"
      )
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
    )
    ,"Magapauwa" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="7800634"
          ,vocabulary="G1"
        )
      )
      ,name = list(
        narrative = list(
          attrs=c("xml:lang"="en")
          ,text="Magapauwa"
        )
      )
      ,point = list(
        attrs = c(
          srsName="http://geonames.org/7800634"
        )
        ,pos = "27.65511 85.99366"
      )
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
    )
    ,"Phasku" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="10323632"
          ,vocabulary="G1"
        )
      )
      ,name = list(
        narrative = list(
          attrs=c("xml:lang"="en")
          ,text="Phasku"
        )
      )
      ,point = list(
        attrs = c(
          srsName="http://geonames.org/10323632"
        )
        ,pos = "27.61424 86.02946"
      )
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
    )
    ,"Pawati" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="7800639"
          ,vocabulary="G1"
        )
      )
      ,name = list(
        narrative = list(
          attrs=c("xml:lang"="en")
          ,text="Pawati"
        )
      )
      ,point = list(
        attrs = c(
          srsName="http://geonames.org/7800639"
        )
        ,pos = "27.59018 86.06369"
      )
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
    )
    ,"Sailungeswor" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="7800640"
          ,vocabulary="G1"
        )
      )
      ,name = list(
        narrative = list(
          attrs=c("xml:lang"="en")
          ,text="Sailungeswor"
        )
      )
      ,point = list(
        attrs = c(
          srsName="http://geonames.org/7800640"
        )
        ,pos = "27.58287 86.0206"
      )
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
    )
    ,"Susmachhemawati" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="7800641"
          ,vocabulary="G1"
        )
      )
      ,name = list(
        narrative = list(
          attrs=c("xml:lang"="en")
          ,text="Susmachhemawati"
        )
      )
      ,point = list(
        attrs = c(
          srsName="http://geonames.org/7800641"
        )
        ,pos = "27.70497 86.04809"
      )
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
    )
    ,"Sundrawati" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="7801812"
          ,vocabulary="G1"
        )
      )
      ,name = list(
        narrative = list(
          attrs=c("xml:lang"="en")
          ,text="Sundrawati"
        )
      )
      ,point = list(
        attrs = c(
          srsName="http://geonames.org/7801812"
        )
        ,pos = "27.71434 86.07367"
      )
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
    )
    ,"Sunkhani" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="7800249"
          ,vocabulary="G1"
        )
      )
      ,name = list(
        narrative = list(
          attrs=c("xml:lang"="en")
          ,text="Sunkhani"
        )
      )
      ,point = list(
        attrs = c(
          srsName="http://geonames.org/7800249"
        )
        ,pos = "27.70059 86.10729"
      )
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
    )
    ,"Kavre" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="1283282"
          ,vocabulary="G1"
        )
      )
      ,name = list(
        narrative = list(
          attrs=c("xml:lang"="en")
          ,text="Kavre"
        )
      )
      ,point = list(
        attrs = c(
          srsName="http://geonames.org/1283282"
        )
        ,pos = "27.58333 85.66667"
      )
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
    )
    ,"Rasuwa" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="1282859"
          ,vocabulary="G1"
        )
      )
      ,name = list(
        narrative = list(
          attrs=c("xml:lang"="en")
          ,text="Rasuwa"
        )
      )
      ,point = list(
        attrs = c(
          srsName="http://geonames.org/1282859"
        )
        ,pos = "28.16667 85.33333"
      )
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
    )
    ,"Nuwakot" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="1282973"
          ,vocabulary="G1"
        )
      )
      ,name = list(
        narrative = list(
          attrs=c("xml:lang"="en")
          ,text="Nuwakot"
        )
      )
      ,point = list(
        attrs = c(
          srsName="http://geonames.org/1282973"
        )
        ,pos = "27.91667 85.25"
      )
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
    )
    ,"Ramechhap" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="1282879"
          ,vocabulary="G1"
        )
      )
      ,name = list(
        narrative = list(
          attrs=c("xml:lang"="en")
          ,text="Ramechhap"
        )
      )
      ,point = list(
        attrs = c(
          srsName="http://geonames.org/1282879"
        )
        ,pos = "27.41667 86.08333"
      )
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
    )
    ,"Dhading" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="1283472"
          ,vocabulary="G1"
        )
      )
      ,name = list(
        narrative = list(
          attrs=c("xml:lang"="en")
          ,text="Dhading"
        )
      )
      ,point = list(
        attrs = c(
          srsName="http://geonames.org/1283472"
        )
        ,pos = "28.0 84.91667"
      )
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
    )
    ,"Makawanpur" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="10330199"
          ,vocabulary="G1"
        )
      )
      ,name = list(
        narrative = list(
          attrs=c("xml:lang"="en")
          ,text="Makawanpur"
        )
      )
      ,point = list(
        attrs = c(
          srsName="http://geonames.org/10330199"
        )
        ,pos = "27.41429 85.17962"
      )
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
    )
    ,"Okhaldhunga" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="7801037"
          ,vocabulary="G1"
        )
      )
      ,name = list(
        narrative = list(
          attrs=c("xml:lang"="en")
          ,text="Okhaldhunga"
        )
      )
      ,point = list(
        attrs = c(
          srsName="http://geonames.org/7801037"
        )
        ,pos = "27.31643 86.50508"
      )
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
    )
    ,"Lalitpur" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="1283154"
          ,vocabulary="G1"
        )
      )
      ,name = list(
        narrative = list(
          attrs=c("xml:lang"="en")
          ,text="Lalitpur"
        )
      )
      ,point = list(
        attrs = c(
          srsName="http://geonames.org/1283154"
        )
        ,pos = "27.58333 85.33333"
      )
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
    )
    ,"Kathmandu" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="1283241"
          ,vocabulary="G1"
        )
      )
      ,name = list(
        narrative = list(
          attrs=c("xml:lang"="en")
          ,text="Kathmandu"
        )
      )
      ,point = list(
        attrs = c(
          srsName="http://geonames.org/1283241"
        )
        ,pos = "27.70704 85.33218"
      )
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
    )
    ,"Laprak" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="10328411"
          ,vocabulary="G1"
        )
      )
      ,name = list(
        narrative = list(
          attrs=c("xml:lang"="en")
          ,text="Laprak"
        )
      )
      ,point = list(
        attrs = c(
          srsName="http://geonames.org/10328411"
        )
        ,pos = "28.25424 84.78469"
      )
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
    )
    ,"Makaising" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="7801826"
          ,vocabulary="G1"
        )
      )
      ,name = list(
        narrative = list(
          attrs=c("xml:lang"="en")
          ,text="Makaising"
        )
      )
      ,point = list(
        attrs = c(
          srsName="http://geonames.org/7801826"
        )
        ,pos = "27.88021 84.6585"
      )
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
    )
    ,"Finam" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="7799037"
          ,vocabulary="G1"
        )
      )
      ,name = list(
        narrative = list(
          attrs=c("xml:lang"="en")
          ,text="Finam"
        )
      )
      ,point = list(
        attrs = c(
          srsName="http://geonames.org/7799037"
        )
        ,pos = "28.004 84.6567"
      )
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
    )
    ,"Takumajhalakuribot" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="7799396"
          ,vocabulary="G1"
        )
      )
      ,name = list(
        narrative = list(
          attrs=c("xml:lang"="en")
          ,text="Takumajhalakuribot"
        )
      )
      ,point = list(
        attrs = c(
          srsName="http://geonames.org/7799396"
        )
        ,pos = "28.1006 84.681"
      )
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
    )
    ,"Bhedapu" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="7801794"
          ,vocabulary="G1"
        )
      )
      ,name = list(
        narrative = list(
          attrs=c("xml:lang"="en")
          ,text="Bhedapu"
        )
      )
      ,point = list(
        attrs = c(
          srsName="http://geonames.org/7801794"
        )
        ,pos = "27.53521 86.05984"
      )
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
    )
    ,"Bhusaphedi" = list(
      attrs = c(
        ref="geoname_id"
      )
      ,"location-id" = list(
        attrs = c(
          code="7800618"
          ,vocabulary="G1"
        )
      )
      ,name = list(
        narrative = list(
          attrs=c("xml:lang"="en")
          ,text="Bhusaphedi"
        )
      )
      ,point = list(
        attrs = c(
          srsName="http://geonames.org/7800618"
        )
        ,pos = "27.64418 85.93448"
      )
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

splitLocat <- function(str){
  if(!is.null(str)){
    if(!is.na(str)){
      if(grepl(",",str,fixed=TRUE)){
        split <- strsplit(str,",",fixed=TRUE)[[1]]
        split <- sapply(split,trimws)
        return(split)
      }else{
        return(c(str))
      }
    }else{return("")}
  }else(return(""))
}



####Generate IATI####
now <- format(Sys.time(),"%Y-%m-%dT%TZ",tz="UTC")
activityAttrs = c("last-updated-datetime"=now, "xml:lang"="en", "default-currency"="USD", "hierarchy"="1", "linked-data-uri"="")

dirs <- list.dirs(wd,full.names=TRUE)

for(d in 2:length(dirs)){
  setwd(dirs[d])

  df <- read.csv("activities.csv",na.strings="",as.is=TRUE)
  dt <- read.csv("transactions.csv",na.strings="",as.is=TRUE)
  
  df[is.na(df)] <- ""
  dt[is.na(dt)] <- ""
  
  li <- list()
  
  for(i in 1:nrow(df)){
    activity <- df[i,]
    activityList <- list(attrs=activityAttrs)
    if(substr(activity$Project.ID,1,3) %in% validIDs){
      activityList[['iati-identifier']] <- activity$Project.ID
    }else{
      activityList[['iati-identifier']] <- paste(repOrg,activity$Project.ID,sep="-")
    }
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
    splits <- unique(splitLocat(activity$Project.location))
    for(k in 1:length(splits)){
      split <- splits[k]
      if(split!=""){
        locat <- locatList[[split]]
        activityList <- c(activityList
          ,list(
            "location" = locat
          )
        ) 
      }
    }
    activityList[['sector']] <- list(
      attrs=c(vocabulary="99",code="")
      ,narrative = list(
        attrs=c("xml:lang"="en")
        ,text=activity$Cluster
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
        attrs = c(code = transTypeList[[tolower(trimws(transaction$Transaction.type))]])
      )
      if(is.null(transTypeList[[tolower(trimws(transaction$Transaction.type))]])){
        message(transaction$Transaction.type)
      }
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
          ,text=paste(transaction$Description,transaction$Transaction.date.narrative,sep="; ")
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
    if(substr(activity$Parent.project.ID,1,3) %in% validIDs){
      activityList[['related-activity']] <- list(
        attrs=c(
          ref=activity$Parent.project.ID
          ,type="1"
        )
      ) 
    }else{
      activityList[['related-activity']] <- list(
        attrs=c(
          ref=paste(repOrg,activity$Parent.project.ID,sep="-")
          ,type="1"
        )
      ) 
    }
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
#            , encoding = "utf8"
          )
  setwd(wd)
}


