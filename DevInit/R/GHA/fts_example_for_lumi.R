data <- data.frame(character(1),character(1))
names(data)<-c("Description","Project.Title")
categories = list(
  "cashForFood" = c("cash for food","cash food","alternative matches"),
  "cashTransfer" = c("cash transfer","transfer cash"),
  "unconditionalCash" = c("unconditional","another word")
)
for(i in 1:length(categories)){
  categoryWords <- categories[[i]]
  category <- names(categoryWords)
  data$temp <- FALSE
  for(j in 1:length(categoryWords)){
    word <- categoryWords[j]
    message(word)
    data <- transform(data,temp=(temp|grepl(word,Description)|grepl(word,Project.Title)))
  }
  data[category] <- data$temp
  data$temp <- NULL
}