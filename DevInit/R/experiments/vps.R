setwd("C:/git/alexm-util/DevInit/R/experiments")
vps <- read.csv("vps.csv",as.is=TRUE)

# dummies = model.matrix(~vps$provider)
# 
# for(i in 2:length(colnames(dummies))){
#   vps[,paste0("provider",i)] <- dummies[,colnames(dummies)[i]]
# }

providers <- unique(vps$provider)
for(i in 1:length(providers)){
  data <- subset(vps,provider==providers[[i]])
  fit <- lm(price ~ mem+core+storage, data=data)
  print(providers[i])
  print(summary(fit))
}
