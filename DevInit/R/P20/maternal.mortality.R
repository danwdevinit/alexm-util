maternal.deaths <- function(df){
  maternal.deathsV <- c()
  for(i in 1:nrow(df)){
    maternal.deaths <- 0
    for(j in 1:20){
      num <- sprintf("%02d", j)
      pregVar <- paste0("mm9_",num)
      if(typeof(df[[pregVar]])!="NULL"){
        preg <- df[[pregVar]][i]
        if(!is.na(preg)){
          if(preg==2 |
               preg==3 |
               preg==5 |
               preg==6 |
               tolower(preg)=="died while pregnant" |
               tolower(preg)=="died during delivery" |
               tolower(preg)=="6 weeks after delivery" |
               tolower(preg)=="2 months after delivery"
             ){
            maternal.deaths <- maternal.deaths + 1
          }
        }
      }else{
        next;
      }
    }
    maternal.deathsV <- c(maternal.deathsV,maternal.deaths)
  }
  return(maternal.deathsV)
}