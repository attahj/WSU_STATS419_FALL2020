library(devtools);
my.source = 'local';
local.path = "C:/_git_/WSU_STATS419_FALL2020/";
local.data.path = ""
source( paste0(local.path,"functions/functions-imdb.R"), local=T );


#preinflation

nmid = "nm0000226"
will = grabFilmsForPerson(nmid)
str(will)
will$movies.50[,c(1,6,8:10)]
plot(will$movies.50[,c(1,6,8:10)])
boxplot(will$movies.50$millions)
widx =  which.max(will$movies.50$millions)
will$movies.50[widx,]
summary(will$movies.50$year)  # bad boys for life ... did data change?

nmid = "nm0000243"
denzel = grabFilmsForPerson(nmid)
plot(denzel$movies.50[,c(1,6,7:10)])
boxplot(denzel$movies.50$millions)
didx =  which.max(denzel$movies.50$millions)
denzel$movies.50[didx,]
summary(denzel$movies.50$year)

par(mfrow=c(1,2))
boxplot(will$movies.50$millions, main=will$name, ylim=c(0,360), ylab="Raw Millions" )
boxplot(denzel$movies.50$millions, main=denzel$name, ylim=c(0,360), ylab="Raw Millions" )

#postinflation

inflation = function(x)
{
  infl = read.csv("C:/Users/blue/Desktop/STAT419/inflation.csv",header=FALSE)
  list = x[,c('millions','year')]
  newmoney = c()
  for (i in 1:nrow(list))
  {
    currentCPI = infl[infl$V1 == '2020',]$V2
    yearCPI = infl[infl$V1 == toString(list[i,]$year) ,]$V2
    newvalue = (currentCPI / yearCPI) * list[i,]$millions
    newmoney = c(newmoney,newvalue)
    
  }
  return(newmoney)
}

will$movies.50$millions.2020 = inflation(will$movies.50)
will$movies.50
denzel$movies.50$millions.2020 = inflation(denzel$movies.50)
denzel$movies.50

par(mfrow=c(1,2))
boxplot(will$movies.50$millions, main=will$name, ylim=c(0,360), ylab="Adjusted for inflation raw Millions" )
boxplot(denzel$movies.50$millions, main=denzel$name, ylim=c(0,360), ylab="Adjusted for inflation raw Millions" )
