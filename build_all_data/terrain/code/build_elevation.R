#building a land cover database
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
elevation_files <- list.files(input_path,pattern="output_COP90.tif",full.names=T,recursive=T)
elevation_rasts <- lapply(elevation_files, rast)

incidents <- st_read("/home/dashaa/wildfire_politics/data/build_wildfire_reports/output/incidents_spatial.gpkg")
incidents <- subset(incidents,select=c(incident_id,final_acres))
incidents <- st_transform(incidents, "+proj=utm +zone=12 +datum=WGS84")
incidents$final_sq_m <- incidents$final_acres*4046.8564224
incidents$radius <- sqrt(incidents$final_sq_m/pi)
incidents <- st_buffer(incidents,incidents$radius)
incidents <- st_transform(incidents, st_crs(elevation_rasts[[1]]))

incidents$mean_elevation <- 0
incidents$max_elevation <- NA
incidents$min_elevation <- NA
incidents$total_cells <- 0

#now extract elevation mean, max and min for each incident
for (elevation_rast in elevation_rasts) {
  
  elevation_means <- terra::extract(elevation_rast,incidents,mean,na.rm=T)
  elevation_n_cells <- terra::extract(elevation_rast,incidents,function(x) sum(!is.na(x)))
  
  added_means <- rowMeans(elevation_n_cells[2]*elevation_means[2])
  added_means[is.na(added_means)] <- 0
  
  incidents$total_cells <- incidents$total_cells + rowMeans(elevation_n_cells[2])
  incidents$mean_elevation <- incidents$mean_elevation + added_means
  
  elevation_maxs <- terra::extract(elevation_rast,incidents,max,na.rm=T)
  incidents$max_elevation <- pmax(incidents$max_elevation,rowMeans(elevation_maxs[2]),na.rm=T)
  
  elevation_mins <- terra::extract(elevation_rast,incidents,min,na.rm=T)
  incidents$min_elevation <- pmin(incidents$min_elevation,rowMeans(elevation_mins[2]),na.rm=T)
}

incidents$mean_elevation <- incidents$mean_elevation/incidents$total_cells

incidents <- st_set_geometry(incidents,NULL)
incidents <- subset(incidents,select=c(incident_id,mean_elevation,max_elevation,min_elevation,total_cells,final_acres))
write.csv(incidents,paste0(output_path,"/incidents_elevation.csv"))


