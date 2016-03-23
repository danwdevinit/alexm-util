library(rvest)

setwd("~/git/digital-platform/reference/")
colors <- read.csv("color.csv")
col2pantone <- function(colVector){
  pantone <- character(length(colVector))
  for(i in 1:length(colVector)){
    col <- col2rgb(colVector[i])
    r <- col[1,]
    g <- col[2,]
    b <- col[3,]
    url <- paste0("http://www.netfront.fr/Services/rgb2pantone/pantone.htm?r=",r,"&g=",g,"&b=",b)
    page <- html(url)
    pantoneTable <- page %>%
      html_node("table") %>%
      html_table(fill=TRUE)
    pantone[i] <- pantoneTable[3,2]
  }
  return(pantone)
}
colors <- transform(colors,pantone=col2pantone(value))
keep <- c("id","value","r","g","b","c","m","y","k","pantone")
colors <- colors[keep]
write.csv(colors,"color.csv",row.names=FALSE)
