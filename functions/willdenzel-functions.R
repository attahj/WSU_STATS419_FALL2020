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
