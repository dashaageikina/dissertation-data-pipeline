#This directory constructs the data on so-called "big news" pertaining to significant events between 1999 and 2014
#The input used (in input/built) was built on my own using Wikipedia*
cd /main_project/build_all_data/big_news/code
Rscript build_big_news.R
Rscript build_incident_big_news.R


#*References:
#https://en.wikipedia.org/wiki/Timeline_of_the_20th_century#1945
#https://en.wikipedia.org/wiki/Timeline_of_the_21st_century
