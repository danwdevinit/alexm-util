####Script examples courtesy of:
####http://www.r-tutor.com/gpu-computing

####Distance Matrix####
test.data <- function(dim, num, seed=17) { 
     set.seed(seed) 
     matrix(rnorm(dim * num), nrow=num) 
 } 
m <- test.data(120, 4500) 
system.time(dist(m))

library(rpud)
system.time(rpuDist(m))

####Hierarchical Cluster Analysis####
d<- dist(as.matrix(mtcars))
hc <- hclust(d)
plot(hc)

test.data <- function(dim, num, seed=17) { 
     set.seed(seed) 
     matrix(rnorm(dim * num), nrow=num) 
 } 
m <- test.data(120, 1500) 
 
d <- rpuDist(m)                # Euclidean distance 
 
system.time(hclust(d))         # complete linkage
system.time(rpuHclust(d))      # rpuHclust
