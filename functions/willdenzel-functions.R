library(humanVerseWSU);

path.github = "https://raw.githubusercontent.com/MonteShaffer/humanVerseWSU/master/";

include.me = paste0(path.github, "misc/functions-nlp.R");
source_url( include.me );
include.me = paste0(path.github, "misc/functions-nlp-str.R");
source_url( include.me );
include.me = paste0(path.github, "misc/functions-nlp-stack.R");
source_url( include.me );
include.me = paste0(path.github, "misc/functions-nlp-pos.R");
source_url( include.me );

include.me = paste0(path.github, "humanVerseWSU/R/functions-encryption.R");
source_url( include.me );
library(tidyverse)



###### UPDATES TO dataframe subset function ######
# inflation adjustments for NA ... and improvements on subsetting
include.me = paste0(path.github, "humanVerseWSU/R/functions-dataframe.R");
source_url( include.me );

include.me = paste0(path.github, "humanVerseWSU/R/functions-inflation.R");
source_url( include.me );


library(imdb);

getdata = function(ttid)
{
  #get movies for actor
  actor.movies = IMDB.getMoviesForPerson(ttid);
  actor = merge(actor.movies, imdb.data$movies.df$info, by="ttid");
  actor = standardizeDollarsInDataFrame(actor, 2000, "millions", "year", "millions2000");
  actor = standardizeDollarsInDataFrame(actor, 2000, "usa.opening", "year", "usa.opening2000"); 
  actor = standardizeDollarsInDataFrame(actor, 2000, "usa.gross", "year", "usa.gross2000");
  actor = standardizeDollarsInDataFrame(actor, 2000, "world.gross", "year", "world.gross2000");
  actor.rank = subsetDataFrame(imdb.data$movies.df$cast, "nmid", "==", "nm0000226"); 
  actor = merge(actor, actor.rank[c("ttid","actor.rank")], by="ttid",all.x = TRUE);
  num.actors = actor.rank[FALSE,]
  for (i in 1:length(actor$ttid))
    num.actors = rbind(num.actors,subsetDataFrame(imdb.data$movies.df$cast, "ttid", "==", actor$ttid[i]))
  num.actors = num.actors %>% filter(nmid != "nm0000226")
  num.actors = num.actors %>% count(ttid)
  num.actors = num.actors %>% rename( num.actors.worked.with=n)
  actor = merge(actor, num.actors, by="ttid",all.x = TRUE);
  actor = subset(actor, select = -c(title,millions,usa.opening,usa.gross,world.gross,country,language,release.date,aka,filming.location,production.name,runtime,sound,color,aspect,release.location,budget.est,usa.opening.date,production.co))
  return(actor)
}
movieWatchRating = function(actor)
{
  actor$rated[is.na(actor$rated)] = "Not Rated"
  actor$rated[actor$rated=="TV-14"] = "PG-13"
  actor$rated[actor$rated=="TV-MA"] = "R"
  actor$rated = as.factor(actor$rated)
  plot <- ggplot(actor, aes(rated)) + geom_bar(aes(y = (..count..)/sum(..count..))) + scale_y_continuous(labels=scales::percent) + ylab("percentage of films")
  plot
}
