library(tidyverse)
options(scipen=999)
library(scatterplot3d)
getdata = function()
{
  #open the file and parse into df
  path.to.secret = "C:/Users/blue/Desktop/STAT419/project/";
  measure_raw = utils::read.csv( paste0(path.to.secret, "cm.final.measure.txt"), header=TRUE, quote="", sep="|");
  return(measure_raw)
}
prepareMeasureData = function(x)
{
  measure = x[-c(1,8:12,14,15,16,17,18,28)]
  measure = measure[c(2,7:8,18)]
  #convert gender to factor
  measure$my.gender = as.factor(measure$my.gender)
  return(measure)
}
getmale = function(x)
{
  return(x %>% filter(my.gender=='m'))
}
getfemale = function(x)
{
  return(x %>% filter(my.gender=='f'))
}
getnumeric = function(x)
{
  return(x %>% select_if(is.numeric))
}
cleanoutlier = function(x,y)
{
  new = rbind(x,y) %>% filter(hand.length<50)
  return (new)
}
build3correlationtables = function(x,y,z)
{
  #all genders
  buildLatexCorrelationTable(as.matrix(getnumeric(x)),myFile = "C:/_git_/WSU_STATS419_FALL2020/project-measure/tables/table-both-correlation.tex",myCaption = "Descriptive Statistics and Correlation Analysis for Both Genders",rotateTable=FALSE)
  #males
  buildLatexCorrelationTable(as.matrix(getnumeric(y)),myFile ="C:/_git_/WSU_STATS419_FALL2020/project-measure/tables/table-male-correlation.tex",myCaption = "Descriptive Statistics and Correlation Analysis for Males",rotateTable=FALSE)
  #females
  buildLatexCorrelationTable(as.matrix(getnumeric(z)),myFile ="C:/_git_/WSU_STATS419_FALL2020/project-measure/tables/table-female-correlation.tex",myCaption = "Descriptive Statistics and Correlation Analysis for Females",rotateTable=FALSE)
}
linearRegression = function(x)
{
  return(lm(height ~ ., getnumeric(x)))
}
dataRegressionPlot = function(x)
{
  colors = c("#E69F00", "#56B4E9")
  colors = colors[as.numeric(x$my.gender)]
  plot = scatterplot3d(x$hand.length,x$age,x$height, angle=50, pch = 16, color=adjustcolor(colors,alpha.f=0.6),  box=FALSE,main="Male and Female Regression Plot")
  plot$plane3d((lm(height ~ ., getnumeric(x))))
  legend("topright", legend = levels(x$my.gender), col = c("#E69F00", "#56B4E9"), pch = 16)
}
dataRegressionPlotSeperateGender = function(x,y,z)
{
  colors = c("#E69F00", "#56B4E9")
  colors = colors[as.numeric(x$my.gender)]
  plot = scatterplot3d(x$hand.length,x$age,x$height, angle=50, pch = 16, color=adjustcolor(colors,alpha.f=0.6),  box=FALSE,main=z)
  plot$plane3d(y)
  legend("topright", legend = levels(x$my.gender), col = c("#E69F00", "#56B4E9"), pch = 16)
}
