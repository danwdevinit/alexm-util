#install.packages("treemap")
library(treemap)
wd <- "C:/git/alexm-util/DevInit/R/treemaps"
setwd(wd)

data <- read.csv("rob.csv",as.is=TRUE)
data$color <- rep("#ba0c2f")

treemap(subset(data,recip=="Bangladesh")
        ,index="flow"
        ,vSize="value"
        ,title="Bangladesh"
        ,vColor="color"
        ,type="color"
)

treemap(subset(data,recip=="Ethiopia")
        ,index="flow"
        ,vSize="value"
        ,title="Ethiopia"
        ,vColor="color"
        ,type="color"
)

treemap(subset(data,recip=="Uganda")
        ,index="flow"
        ,vSize="value"
        ,title="Uganda"
        ,vColor="color"
        ,type="color"
)