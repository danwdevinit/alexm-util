setwd("~/git/digital-platform/reference/")
colors <- read.csv("color.csv")
col2cmyk <- function(colVector){
  cV <- double(length(colVector))
  mV <- double(length(colVector))
  yV <- double(length(colVector))
  kV <- double(length(colVector))
  for(i in 1:length(colVector)){
    col <- col2rgb(colVector[i])
    r <- col[1,]/255
    g <- col[2,]/255
    b <- col[3,]/255
    k <- 1-max(r,g,b)
    kV[i] <- k
    if(k!=1){
      cV[i] <- (1-r-k)/(1-k)
      mV[i] <- (1-g-k)/(1-k)
      yV[i]<- (1-b-k)/(1-k) 
    }else{
      cV[i] <- 0
      mV[i] <- 0
      yV[i]<- 0
    }
  }
  df <- data.frame(cV,mV,yV,kV)
  names(df) <- c("c","m","y","k")
  return(df)
}
colors <- transform(colors,c=col2cmyk(value)["c"],m=col2cmyk(value)["m"],y=col2cmyk(value)["y"],k=col2cmyk(value)["k"])
keep <- c("id","value","r","g","b","c","m","y","k")
colors <- colors[keep]
write.csv(colors,"color.csv",row.names=FALSE)
