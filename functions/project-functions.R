library(tidyverse)
getdata = function()
{
  #open the file and parse into df
  path.to.secret = "C:/Users/blue/Desktop/STAT419/project/";
  measure_raw = utils::read.csv( paste0(path.to.secret, "cm.final.measure.txt"), header=TRUE, quote="", sep="|");
  return(measure_raw)
}
prepareMeasureData = function(x)
{
  return(x[-c(1,8:12,14,15,16,17,18,28)])
}

