#install.packages('plyr')
#require(devtools)
#install_github('ramnathv/rCharts')
library(plyr)
library(rCharts)

####Data####

wd <- "C:/Users/alexm/Documents/Rwork"
setwd(wd)

data <- read.csv("Uganda Primary Leaving Exam Results 2014.csv", header = TRUE,sep=",",na.strings="",check.names=TRUE)

count(data$M.F)

male <- data[which(data$M.F=="M"),]
female <- data[which(data$M.F=="F"),]

#Overall
maleGrade <- count(male$DIV)
maleGrade$Gender <- "male"
maleGrade$Percent <- (maleGrade$freq/nrow(male))*100
femaleGrade <- count(female$DIV)
femaleGrade$Gender <- "female"
femaleGrade$Percent <- (femaleGrade$freq/nrow(female))*100

DIVByGender <- rbind(maleGrade,femaleGrade)
names(DIVByGender)[which(names(DIVByGender)=="x")] <- "Score"

#English
maleGrade <- count(male$ENG)
maleGrade$Gender <- "male"
maleGrade$Percent <- (maleGrade$freq/nrow(male))*100
femaleGrade <- count(female$ENG)
femaleGrade$Gender <- "female"
femaleGrade$Percent <- (femaleGrade$freq/nrow(female))*100

ENGByGender <- rbind(maleGrade,femaleGrade)
names(ENGByGender)[which(names(ENGByGender)=="x")] <- "Score"

#Science
maleGrade <- count(male$SCI)
maleGrade$Gender <- "male"
maleGrade$Percent <- (maleGrade$freq/nrow(male))*100
femaleGrade <- count(female$SCI)
femaleGrade$Gender <- "female"
femaleGrade$Percent <- (femaleGrade$freq/nrow(female))*100

SCIByGender <- rbind(maleGrade,femaleGrade)
names(SCIByGender)[which(names(SCIByGender)=="x")] <- "Score"

#Social Studies?
maleGrade <- count(male$SST)
maleGrade$Gender <- "male"
maleGrade$Percent <- (maleGrade$freq/nrow(male))*100
femaleGrade <- count(female$SST)
femaleGrade$Gender <- "female"
femaleGrade$Percent <- (femaleGrade$freq/nrow(female))*100

SSTByGender <- rbind(maleGrade,femaleGrade)
names(SSTByGender)[which(names(SSTByGender)=="x")] <- "Score"

#Maths
maleGrade <- count(male$MAT)
maleGrade$Gender <- "male"
maleGrade$Percent <- (maleGrade$freq/nrow(male))*100
femaleGrade <- count(female$MAT)
femaleGrade$Gender <- "female"
femaleGrade$Percent <- (femaleGrade$freq/nrow(female))*100

MATByGender <- rbind(maleGrade,femaleGrade)
names(MATByGender)[which(names(MATByGender)=="x")] <- "Score"

####Define DI colors####
diColors <- c("#ba0c2f" #Red
              ,"#1b365d" #blue
              ,"#ea7600" #Orange
              ,"#93328e" #purple
              ,"#0095c8" #lightblue
              ,"#b7bf10" #Yellow
)
diColorSwap <- c("#1b365d" #blue
              ,"#ba0c2f" #Red
              ,"#ea7600" #Orange
              ,"#93328e" #purple
              ,"#0095c8" #lightblue
              ,"#b7bf10" #Yellow
)
diColorsLong <- c('#00688c'
                  ,'#be84bb'
                  ,'#662363'
                  ,'#b7bf10'
                  ,'#0c192d'
                  ,'#1b365d'
                  ,'#cceaf4'
                  ,'#f2ad66'
                  ,'#ea7600'
                  ,'#0095c8'
                  ,'#595d07'
                  ,'#66bfde'
                  ,'#471845'
                  ,'#e9d6e8'
                  ,'#820820'
                  ,'#93328e'
                  ,'#004862'
                  ,'#f7f2cf'
                  ,'#80850b'
                  ,'#f7ced5'
                  ,'#d4d970'
                  ,'#d1d7df'
                  ,'#122541'
                  ,'#76869e'
                  ,'#5b0516'
                  ,'#fbe4cc'
                  ,'#ba0c2f'
                  ,'#723900'
                  ,'#d66d82'
                  ,'#a35200'
)

rPlotColors <- paste0("{color: {scale: {type: gradient, lower: "
                      ,"#f7ced5"
                      ,", upper:" 
                      ,"#820820"
                      ,"}}}"
)

####Graph####
d1 <- dPlot(
  y = "Percent",
  x = c("Score","Gender"),
  groups = "Gender",
  data = DIVByGender,
  type = "bar"
)
d1$xAxis( type = "addCategoryAxis", orderRule = "Score")
d1$yAxis( type = "addMeasureAxis")
d1$defaultColors(diColors)
d1$legend(
  x = 350,
  y = 25,
  width = 400,
  height = 100,
  horizontalAlign = "right"
)
d1$setTemplate(afterScript = "
  <script>
    myChart.axes[0].addGroupOrderRule('Gender',true);
    myChart.draw()
    myChart.axes[0].titleShape.text('Score')
    myChart.svg.append('text')
        .attr('x', 170)
        .attr('y', 20)
        .text('Ugandan primary school leaving exam overall score by gender (2014)')
        .style('text-anchor','beginning')
        .style('font-size', '100%')
        .style('font-family','sans-serif')
  </script>               
")

d2 <- dPlot(
  y = "Percent",
  x = c("Score","Gender"),
  groups = "Gender",
  data = ENGByGender,
  type = "bar"
)
d2$xAxis( type = "addCategoryAxis", orderRule = "Score")
d2$yAxis( type = "addMeasureAxis")
d2$defaultColors(diColors)
d2$legend(
  x = 350,
  y = 25,
  width = 400,
  height = 100,
  horizontalAlign = "right"
)
d2$setTemplate(afterScript = "
  <script>
    myChart.axes[0].addGroupOrderRule('Gender',true);
    myChart.draw()
    myChart.axes[0].titleShape.text('Score')
    myChart.svg.append('text')
        .attr('x', 170)
        .attr('y', 20)
        .text('Ugandan primary school leaving exam English score by gender (2014)')
        .style('text-anchor','beginning')
        .style('font-size', '100%')
        .style('font-family','sans-serif')
  </script>               
")

d3 <- dPlot(
  y = "Percent",
  x = c("Score","Gender"),
  groups = "Gender",
  data = SCIByGender,
  type = "bar"
)
d3$xAxis( type = "addCategoryAxis", orderRule = "Score")
d3$yAxis( type = "addMeasureAxis")
d3$defaultColors(diColors)
d3$legend(
  x = 350,
  y = 25,
  width = 400,
  height = 100,
  horizontalAlign = "right"
)
d3$setTemplate(afterScript = "
  <script>
    myChart.axes[0].addGroupOrderRule('Gender',true);
    myChart.draw()
    myChart.axes[0].titleShape.text('Score')
    myChart.svg.append('text')
        .attr('x', 170)
        .attr('y', 20)
        .text('Ugandan primary school leaving exam science score by gender (2014)')
        .style('text-anchor','beginning')
        .style('font-size', '100%')
        .style('font-family','sans-serif')
  </script>               
")

d4 <- dPlot(
  y = "Percent",
  x = c("Score","Gender"),
  groups = "Gender",
  data = SSTByGender,
  type = "bar"
)
d4$xAxis( type = "addCategoryAxis", orderRule = "Score")
d4$yAxis( type = "addMeasureAxis")
d4$defaultColors(diColorSwap)
d4$legend(
  x = 350,
  y = 25,
  width = 400,
  height = 100,
  horizontalAlign = "right"
)
d4$setTemplate(afterScript = "
  <script>
    myChart.axes[0].addGroupOrderRule('Gender',false);
    myChart.draw()
    myChart.axes[0].titleShape.text('Score')
    myChart.svg.append('text')
        .attr('x', 160)
        .attr('y', 20)
        .text('Ugandan primary school leaving exam social-studies score by gender (2014)')
        .style('text-anchor','beginning')
        .style('font-size', '100%')
        .style('font-family','sans-serif')
  </script>               
")

d5 <- dPlot(
  y = "Percent",
  x = c("Score","Gender"),
  groups = "Gender",
  data = MATByGender,
  type = "bar"
)
d5$xAxis( type = "addCategoryAxis", orderRule = "Score")
d5$yAxis( type = "addMeasureAxis")
d5$defaultColors(diColors)
d5$legend(
  x = 350,
  y = 25,
  width = 400,
  height = 100,
  horizontalAlign = "right"
)
d5$setTemplate(afterScript = "
  <script>
    myChart.axes[0].addGroupOrderRule('Gender',true);
    myChart.draw()
    myChart.axes[0].titleShape.text('Score')
    myChart.svg.append('text')
        .attr('x', 170)
        .attr('y', 20)
        .text('Ugandan primary school leaving exam maths score by gender (2014)')
        .style('text-anchor','beginning')
        .style('font-size', '100%')
        .style('font-family','sans-serif')
  </script>               
")

####Export####

charts <- c(d1
            ,d2
            ,d3
            ,d4
            ,d5
            )

for(i in 1:length(charts)){
  chart <- charts[[i]]
  chart$save(paste0('//dipr-dc01/home$/AlexM/My Documents/uganda_educ/chart',i,'.html'), cdn = TRUE)
}
