#install.packages("textir")
library(textir)
library(rpud)

data(we8there)

rpuDist(tfidf(we8thereCounts,normalize=TRUE))