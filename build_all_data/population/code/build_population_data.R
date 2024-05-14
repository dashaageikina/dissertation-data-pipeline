#prepare population data for merging with incidents
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

rm(list=ls())
path <- "/home/dashaa/wildfire_politics/data/build_population"
input_path <- paste0(path,"/input/raw/nhgis0001_csv")
setwd(path)

pop <- read.csv(paste0(input_path,"/nhgis0001_ts_nominal_tract.csv"))
