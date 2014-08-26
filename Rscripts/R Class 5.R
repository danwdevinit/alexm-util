install.packages("survey")
install.packages("gridExtra")

#experiments with ggplot2
library(plyr)
library(ggplot2)
library(scales)
library(foreign)
library(survey)
library(grid)
library(gridExtra)

#reading in data
WD <- "~/R_work"
setwd (WD)
data <- read.csv("MT1.csv")

str(data)

#basic histogram ok
qplot(data=data, x=Q3, main = "% of Intro IR course devoted to policy analysis") +
ylab("Percentage of Responses") +
  xlab("Sextiles") 
  

#bar chart
ggplot(data, aes(Q3, fill=Country) ) +
geom_bar(aes(y = ..count../sum(..count..), group= Country, position="dodge")) +
  ylab("Percentage of Total Responses") + 
  xlab("Sextiles") +
  ggtitle("% of Intro IR course devoted to policy analysis")


#better version - need to add titles - scale is out of entire sample
ggplot(data, aes(Q3) ) +
  geom_histogram(aes( y = ..count../sum(..count..)))+
  scale_y_continuous(labels = percent_format()) +
  facet_grid(Country ~ ., scales="free_y") +
  xlab("X Label") +
  ylab("Y Label")

#all done - sans color
ggplot(data, aes(Q3) ) +
  geom_histogram(aes( y = ..count../sum(..count..), group = Country)) +
  scale_y_continuous(labels = percent_format()) +
  facet_grid(Country ~ ., scales="free_y") +
  ylab("Percentage of Responses") +
  xlab("Sextiles") +
  ggtitle("% of Intro IR course devoted to policy analysis")

#adding color
data <- read.csv("MT1b.csv")


ggplot(data) +
  geom_bar(aes(Q_20)) +
  ylab("Percentage of Total Responses") + 
  xlab("Sextiles") +
  ggtitle("Count of Intro IR course devoted to policy analysis")


ggplot(data, aes(Q_20) ) +
  geom_bar(aes( y = ..count.., fill=Q_20)) +
  ylab("Number of Responses") +
  xlab("Rational Choice Framework") +
  ggtitle("Which of the following statements best characterizes your work?")

ggplot(data, aes(Q_20) ) +
  geom_bar(aes( y = ..count.., fill=Q_20)) +
  ylab("Number of Responses") +
  xlab("Rational Choice Framework") +
  ggtitle("Which of the following statements best characterizes your work?") +
  scale_fill_manual(values=c("#666666", "#66cc66" , "#003366"))
  

data <- read.csv("MT1b.csv")


data$q20 <-revalue(data$Q_20, c("Response1" = "1 & 2",
                                "Response2" = "1 & 2",
                                "Response3" = "3"))

data <-data[which(data[2]=="United States" | data[2]== "United Kingdom" | data[2]== "Israel" | data[2]== "Mexico"),]

#dropping legend
ggplot(data, aes(x=q20) ) +
  geom_bar(aes( y = ..count../sum(..count..), fill=factor(..x..), group = country)) +
  scale_y_continuous(labels = percent_format()) +
  ylab("Density of Responses") +
  xlab("Rational Choice Framework") +
  ggtitle("Which of the following statements best characterizes your work?") +
  scale_fill_manual(values=c("#006699" , "#003366")) +
  theme(legend.position="None") +
  facet_grid(~ country)

ggplot(data, aes(x=q20) ) +
  geom_bar(aes( y = ..count../sum(..count..), fill=factor(..x..), group = country)) +
  scale_y_continuous(labels = percent_format()) +
  ylab("Density of Responses") +
  xlab("Rational Choice Framework") +
  ggtitle("Which of the following statements best characterizes your work?") +
  scale_fill_manual(values=c("#006699" , "#003366")) +
  theme(legend.position="None") +
  facet_grid(~ country)


#Building multiple plots and attaching them
data <- read.dta("MT1e12.dta")


data$Country <-revalue(data$country, c("United States" = "United States",
                                       "Hong Kong, China" = "All Others", 
                                       "Argentina" = "All Others", 
                                       "Australia"  = "All Others",
                                       "Brazil" = "All Others", 
                                       "Canada" = "All Others", 
                                       "Colombia" = "All Others",
                                       "Denmark" = "All Others", 
                                       "Finland" = "All Others", 
                                       "France" = "All Others",
                                       "French Canada" = "All Others", 
                                       "Ireland" = "All Others", 
                                       "Israel" = "All Others",
                                       "Mexico" = "All Others", 
                                       "New Zealand" = "All Others", 
                                       "Norway" = "All Others",
                                       "Singapore"  = "All Others", 
                                       "South Africa" = "All Others", 
                                       "Sweden" = "All Others",
                                       "Turkey" = "All Others", 
                                       "United Kingdom" = "All Others"))

d <- ggplot(data, aes(CIC)) +
  geom_density(aes(y = ..density.., group=Country, fill = Country, guide=FALSE)) +
  ylab("Percentage of Total Responses") + xlab("China's Current Influence") +
  theme(legend.direction="horizontal")+
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values=alpha(c("#CCCCCC", "#99CCFF"),0.65)) 

e <- ggplot(data, aes(CIC)) +
  geom_density(aes(y = ..density.., group=Country, fill = Country, guide=FALSE)) +
  xlab("China's Current Influence") +
  theme(axis.title.y=element_blank())+
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values=alpha(c("#CCCCCC", "#99CCFF"),0.65))

f <- ggplot(data, aes(CIF)) +
  geom_density(aes(y = ..density.., group=Country, fill = Country)) +
  xlab("China's Future Influence") +
  theme(axis.title.y=element_blank())+
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values=alpha(c("#CCCCCC", "#99CCFF"),0.65))

g <- ggplot(data, aes(USIC)) +
  geom_density(aes(y = ..density.., group=Country, fill = Country)) +
  xlab("US Current Influence") +
  theme(axis.title.y=element_blank())+
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values=alpha(c("#CCCCCC", "#99CCFF"),0.65))

h <- ggplot(data, aes(USIF)) +
  geom_density(aes(y = ..density.., group=Country, fill = Country)) +
  xlab("US Future Influence") +
  theme(axis.title.y=element_blank())+
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values=alpha(c("#CCCCCC", "#99CCFF"),0.65))


g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

legend <- g_legend(d)
grid.arrange(legend)

vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y) 
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2)))
print(e, vp = vplayout(1, 1))
print(f, vp = vplayout(1, 2))
print(g, vp = vplayout(2, 1))
print(h, vp = vplayout(2, 2))

g2 <-grid.arrange(arrangeGrob(e + theme(legend.position="none"),
                              f + theme(legend.position="none"),
                              g + theme(legend.position="none"),
                              h + theme(legend.position="none"), nrow=2),
                  main = textGrob("Perceived Influence of China vs US", vjust = 1, gp = gpar(fontface = "bold", cex = 1.5)),
                  left = textGrob("Percentage of Responses", rot = 90, vjust = 1), 
                  legend, nrow=2, heights=c(7,1))

png("example.png", width = 9000, height = 8000, res = 2000) 
grid.arrange(arrangeGrob(e + theme(legend.position="none"),
                         f + theme(legend.position="none"),
                         g + theme(legend.position="none"),
                         h + theme(legend.position="none"), nrow=2),
             main = textGrob("Perceived Influence of China vs US", vjust = 1, gp = gpar(fontface = "bold", cex = 1.5)),
             left = textGrob("Percentage of Responses", rot = 90, vjust = 1), 
             legend, nrow=2, heights=c(7,1))
dev.off()

theme(plot.title = element_text(lineheight=1, face="bold", size = b)) +
  theme(axis.text.x = element_text(angle = d, hjust = e, color = "black", size = f)) +
  theme(axis.text.y = element_text(color = "black", size = g)) +
  theme(axis.title.x = element_text(color = "black", size = h, face = "bold")) +
  theme(axis.title.y = element_text(color = "black", size = i, face = "bold")) +
  scale_y_continuous(labels = percent_format()) +
  theme(legend.position="None") 


###Smoothed plots
data <- read.dta("Epistemology.dta")

data1 <- data[data$issuearea %in% c("International Security", "International Political Economy"),]
#To make it just EJIR articles
#data1 <- data[data$name %in% "EJIR",]
#data1 <- data[data$name %in% "IO",]


#creates year by year counts of the strings w/in epistemology 
df1 <- count(data1, vars = c("year", "epistemology"))
names(df1)[names(df1)=="year"] <- "Year"
names(df1)[names(df1)=="epistemology"] <- "Epistemology"
names(df1)[names(df1)=="freq"] <- "Frequency"

ggplot(df1, aes(x=Year, y=Frequency, color=Epistemology)) +
  geom_point(shape=1) +
  geom_smooth() +
  ggtitle("Epistemology of IPE and International Security from 1980 to present") +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  theme(legend.position="bottom")



#df1 <- count(data1, vars = c("year", "epistemology")) 
#df2 <- sum(df1, vars = "Frequency")

#df1$Sum <- aggregate(df1$Frequency, by=list(df1$Year), FUN=sum, na.rm=FALSE)

#Finding proportion of time series variables 
df1$z <- as.numeric(as.character(df1$Frequency))

df2 <- ddply(.data = df1, .variables = .(Year), mutate,
             Proportion = Frequency/sum(Frequency))


ggplot(df2, aes(x=Year, y=Proportion, color=Epistemology)) +
  geom_point(shape=1) +
  geom_smooth() +
  ggtitle("Epistemology of IPE and International Security from 1980 to present") +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  theme(legend.position="bottom")


##Adding two smoothed plots to the same graph
df1 <- read.csv("Polity vs Freedom.csv")


a <- ggplot()+
  geom_smooth(data=df1, aes(x=Year, y=meanPolity2, color = "meanPolity2"), se = FALSE, size =1 ) +
  geom_smooth(data=df1, aes(x=Year, y=meanFH, color = "meanFH"), se = FALSE, size =1 ) +
  ggtitle("Mean values of Freedom House vs Polity IV") +
  theme(plot.title = element_text(lineheight=1, face="bold", size = 20)) +
  theme(axis.text.x = element_text(color = "black", size = 15)) +
  theme(axis.text.y = element_text(color = "black", size = 15)) +
  theme(axis.title.x = element_text(color = "black", size = 15, face = "bold")) +
  theme(axis.title.y = element_text(color = "black", size = 15, face = "bold")) +
  ylab("Average Democracy Score") +
  scale_color_manual("", values = c("meanPolity2"="black", "meanFH" = "red")) +
  theme(legend.position="bottom")

ggsave(a, file = "FH vs Polity.jpg",dpi = 1000)



