dataclean = function()
{
  #open the file and parse into df
  path.to.secret = "C:/Users/blue/Desktop/STAT419/project/";
  measure_raw = utils::read.csv( paste0(path.to.secret, "cm.final.measure.txt"), header=TRUE, quote="", sep="|");
  return(measure_raw)
}
