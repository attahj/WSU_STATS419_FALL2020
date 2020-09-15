library(tidyverse)
library(matrixStats)

my_data = read.table("C:/Users/blue/Desktop/STAT419/personality-raw.txt", sep ="|", header = TRUE, dec =".")
my_data = my_data[,-3]
my_data$date_test = as.Date(sub(" .*", "", my_data$date_test), format = '%m/%d/%Y')
my_data$WEEK = format(as.Date(my_data$date_test), "%W")
my_data$YEAR = format(as.Date(my_data$date_test), "%Y")
my_data = my_data[order(my_data$YEAR,my_data$WEEK, decreasing = TRUE),]
my_data_clean = distinct(my_data, md5_email, .keep_all = TRUE)
#dim(my_data) 838
#dim(my_data_clean) 678

write.table(my_data_clean, file = "C:/Users/blue/Desktop/STAT419/personality-clean.txt", sep = "|",
            row.names = TRUE)

#monte.shaffer@gmail.com
#b62c73cdaf59e0a13de495b84030734e

monte = my_data_clean[my_data_clean$md5_email == 'b62c73cdaf59e0a13de495b84030734e',]
monte

doSummary = function(x)
{
  #length
  print("dimension")
  print(dim(x))
  #number of NAs
  print("number of NAs")
  print(sum(is.na(x)))
  #mean
  print("mean of each column")
  print(colMeans(x))
  #median
  print("median of each column")
  print(colMedians(data.matrix(x)))
  #mode
  print("mode of each column")
  print(sapply(x,doMode))
  #variance
  print("variance of each column")
  print(sapply(x,doSampleVariance))
  #sd
  print("sd of each column")
  print(sapply(x,sd))
}

doSampleVariance = function(x,method='two-pass')
{
  #return sum, sum sq , var
  if(method=='naive')
  {
    sum((x-mean(x))^2)/(length(x)-1);
  }
  #return sum, sum2, var
  else
  {
    #two-pass algo
    sumsq = 0
    for (i in x)
    {
      sumsq = sumsq + ((i - mean(x))*(i-mean(x)))
    }
    sumsq / (length(x)-1);
  }
}

doMode = function(x) #from stackoverflow https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode
{
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}

doSummary(my_data_clean[,3:62])


