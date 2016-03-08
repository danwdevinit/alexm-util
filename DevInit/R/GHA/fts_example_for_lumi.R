data <- data.frame(logical(1))
categories = list(
  "cashForFood" = c("cash for food","cash food","alternative matches"),
  "cashTransfer" = c("cash transfer","transfer cash"),
  "unconditionalCash" = c("unconditional")
)
for(i in 1:length(categories)){
  categoryWords <- categories[i]
  category <- names(categoryWords)
  data$temp <- FALSE
  for(j in 1:lenth(categoryWords)){
    word <- categoryWords[j]
    data <- transform(data,temp=(temp|grepl(word,Description)|grepl(word,Project.Title)))
  }
  data[category] <- data$temp
  data$temp <- NULL
}