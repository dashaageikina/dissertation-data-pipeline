#put land covers together
if (!require(tidyverse)) install.packages('tidyverse')
install.packages("plyr")
library(tidyverse)
library(plyr)

rm(list=ls())

path <- "/home/dashaa/wildfire_politics/data/build_land_cover"

built_files <-  list.files(paste0(path,"/input/built"),pattern=".csv",
                           full.names=T,recursive=T)

for (file in built_files) {
  if (file==built_files[1]) {
    alldata <- read.csv(file)
  } else {
    alldata <- rbind.fill(alldata,read.csv(file))
  }
}

alldata2 <- alldata %>%
  group_by(incident_id) %>%
  dplyr::summarize(
    tree_cover_p = ifelse(all(is.na(tree_cover_p)), NA, na.omit(tree_cover_p)[1]),  
    shrub_cover_p = ifelse(all(is.na(shrub_cover_p)), NA, na.omit(shrub_cover_p)[1]),
    herb_cover_p = ifelse(all(is.na(herb_cover_p)), NA, na.omit(herb_cover_p)[1])
  )

write.csv(alldata2,paste0(path,"/output/incidents_land_cover.csv"))



