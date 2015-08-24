#install.packages("ggplot2")
#install.packages("extrafont")
#install.packages("plyr")
library(ggplot2)
library(plyr)
library(extrafont)
#font_import()
loadfonts()

ghahex <- c(
  "#fcd800" #Yellow
  ,"#67cdca" #Blue
  ,"#Ffa03c" #Orange
  ,"#61c994" #Green
  ,"#a169de" #Purple
  ,"#A6f2eb" #Light blue
  ,"#8eb8b4" #Grey blue
  ,"#ff8f19" #Orange
  ,"#b5a384" #Brown
  ,"#99e354" #Greem
)

wd <- "C:/git/alexm-util/DevInit/R/GHA/"
setwd(wd)

#First chart####
data <- read.csv("scaled_pies.csv")

# Calculate the percentages
data = ddply(data, .(country), transform, percent = usd/total)
# Format the labels and calculate their positions
data <- ddply(data, .(country), transform, pos = (cumsum(percent) - 0.5 * percent))
data$label <- paste0(sprintf("%.0f", data$percent*100), "%")
#data$label <- paste0("$",round(data$usd),"m")

p <- ggplot(data,aes(fill=sector,x=total/2,y=usd,width=total)) +
  geom_bar(position="fill", stat="identity") + 
  geom_text(aes(y = pos, x = total+60, label = label), size = 4,family="Calibri") +
  facet_grid(. ~ country) +
  coord_polar(theta="y") + 
  theme_bw() +
  theme(panel.grid=element_blank()) +
  theme(axis.text=element_blank()) +
  theme(axis.ticks=element_blank()) +
  theme(axis.title=element_blank()) +
  theme(legend.position="bottom") +
  theme(strip.background = element_blank()) +
  theme(panel.border = element_blank()) +
  theme(legend.title = element_blank()) +
  theme(strip.text = element_text(size=15)) +
  theme(text = element_text(family="Calibri")) +
  scale_fill_manual(values=ghahex)
  #+ geom_text(x=1,y=1,aes(label=paste0("$",total,"m")),family="Calibri")

ggsave("scaled_pies.pdf",p,device=cairo_pdf)


#Second chart####
data2 <- read.csv("scaled_pies2.csv")

# Calculate the percentages
data2 = ddply(data2, .(country), transform, percent = usd/total)
# Format the labels and calculate their positions
data2 <- ddply(data2, .(country), transform, pos = (cumsum(percent) - 0.5 * percent))
#data$label <- paste0(sprintf("%.0f", data$percent*100), "%")
data2$label <- paste0("$",round(data2$usd),"m")

data2 <- subset(data2,percent>0)
data2$country <- factor(data2$country,levels=c("Kenya","Uganda","South Sudan","Somalia"))

p2 <- ggplot(data2,aes(fill=resource,x=total/2,y=usd,width=total)) +
  geom_bar(position="fill", stat="identity") + 
  geom_text(aes(y = pos, x = total/2, label = label), size = 4,family="Calibri") +
  facet_grid(. ~ country) +
  #coord_polar(theta="y") + 
  theme_bw() +
  theme(panel.grid=element_blank()) +
  theme(axis.text=element_blank()) +
  theme(axis.ticks=element_blank()) +
  theme(axis.title=element_blank()) +
  theme(legend.position="bottom") +
  theme(strip.background = element_blank()) +
  theme(panel.border = element_blank()) +
  theme(legend.title = element_blank()) +
  theme(strip.text = element_text(size=15)) +
  theme(text = element_text(family="Calibri")) +
  scale_fill_manual(values=ghahex)
#+ geom_text(x=1,y=1,aes(label=paste0("$",total,"m")),family="Calibri")

ggsave("scaled_pies2.pdf",p2,device=cairo_pdf)
