#install.packages("openxlsx")
library(openxlsx)

#Write####
setwd("C:/Users/alexm/Documents/Rwork")

# for(i in 8:9){
#   size <- 1*(10^i)
#   df <- data.frame(rnorm(size),rnorm(size),rnorm(size),rnorm(size),rnorm(size),rnorm(size),rnorm(size),rnorm(size),rnorm(size),rnorm(size))
#   write.csv(df,paste0("df",i,".csv"))
#   #write.xlsx(df,paste0("df",i,".xlsx"))
# };

#Read####
readTime <- function(i){read.csv(paste0("df",i,".csv"));FALSE;}
sizes <- c()
times <- c()
for(i in 1:7){
  size <- 1*(10^i)
  sizes <- c(sizes,size)
  time <- system.time(readTime(i))[3]
  times <- c(times,time)
};

trial <- data.frame(sizes,times)