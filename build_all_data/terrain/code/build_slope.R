if (!require(terra)) install.packages('terra')
if (!require(sf)) install.packages('sf')
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(R.utils)) install.packages('R.utils')
library(sf)
library(tidyverse)
library(terra)
library(R.utils)

rm(list=ls())

path <- "/home/dashaa/wildfire_politics/data/build_terrain"
input_path <- paste0(path,"/input/raw/copernicus")
output_path <- paste0(path,"/output")
#unzip gz files
# files <- list.files(input_path,pattern=".gz",full.names = T,recursive=T)
# for (file in files) {
#   gunzip(file)
# }
# files <- list.files(input_path,pattern=".tar",full.names = T,recursive=T)
# for (file in files) {
#   setwd(dirname(file))
#   untar(file)
#   unlink(file)
# }
slope_files <- list.files(input_path,pattern="slope.tif",full.names=T,recursive=T)
slope_rasts <- lapply(slope_files, rast)

incidents <- st_read("/home/dashaa/wildfire_politics/data/build_wildfire_reports/output/incidents_spatial.gpkg")
incidents <- subset(incidents,select=c(incident_id,final_acres))
incidents <- st_transform(incidents, "+proj=utm +zone=12 +datum=WGS84")
incidents$final_sq_m <- incidents$final_acres*4046.8564224
incidents$radius <- sqrt(incidents$final_sq_m/pi)
incidents <- st_buffer(incidents,incidents$radius)
incidents <- st_transform(incidents, st_crs(slope_rasts[[1]]))

incidents$mean_slope <- 0
incidents$max_slope <- NA
incidents$min_slope <- NA
incidents$total_cells <- 0

#now extract slope mean, max and min for each incident
for (slope_rast in slope_rasts) {
  
  slope_means <- terra::extract(slope_rast,incidents,mean,na.rm=T)
  slope_n_cells <- terra::extract(slope_rast,incidents,function(x) sum(!is.na(x)))
  
  added_means <- rowMeans(slope_n_cells[2]*slope_means[2])
  added_means[is.na(added_means)] <- 0
  
  incidents$total_cells <- incidents$total_cells + rowMeans(slope_n_cells[2])
  incidents$mean_slope <- incidents$mean_slope + added_means
  
  slope_maxs <- terra::extract(slope_rast,incidents,max,na.rm=T)
  incidents$max_slope <- pmax(incidents$max_slope,rowMeans(slope_maxs[2]),na.rm=T)
  
  slope_mins <- terra::extract(slope_rast,incidents,min,na.rm=T)
  incidents$min_slope <- pmin(incidents$min_slope,rowMeans(slope_mins[2]),na.rm=T)
}

incidents$mean_slope <- incidents$mean_slope/incidents$total_cells

incidents <- st_set_geometry(incidents,NULL)
incidents <- subset(incidents,select=c(incident_id,mean_slope,max_slope,min_slope,total_cells,final_acres))
write.csv(incidents,paste0(output_path,"/incidents_slope.csv"))

