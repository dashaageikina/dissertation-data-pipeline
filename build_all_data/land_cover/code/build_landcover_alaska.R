#building a land cover database
#the documentation of the data is at https://daac.ornl.gov/ABOVE/guides/Annual_Landcover_ABoVE.html
if (!require(terra)) install.packages('terra')
if (!require(sf)) install.packages('sf')
if (!require(tidyverse)) install.packages('tidyverse')
library(terra)
library(sf)
library(tidyverse)

rm(list=ls())

#upload data on incidents
incidents <- st_read("/home/dashaa/wildfire_politics/data/build_wildfire_reports/output/incidents_spatial.gpkg")
#get only 1999 contiguous US incidents
incidents <- filter(incidents,POO_state=="AK")

incidents <- subset(incidents,select=c(incident_id,start_year,final_acres))
incidents <- st_transform(incidents, crs = st_crs("+proj=utm +zone=12 +datum=WGS84"))
incidents$final_sq_m <- incidents$final_acres*4046.8564224
incidents$radius <- sqrt(incidents$final_sq_m/pi)
incidents <- st_buffer(incidents,incidents$radius)
incidents <- st_transform(incidents, 
                          crs = "+proj=aea +lat_0=40 +lon_0=-96 +lat_1=50 +lat_2=70 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

incidents$tree_cover_p <- 0
incidents$shrub_cover_p <- 0
incidents$herb_cover_p <- 0

path <- "/home/dashaa/wildfire_politics/data/build_land_cover"
input_path <- paste0(path,"/input/raw/above/Annual_Landcover_ABoVE_1691/data")
output_path <- paste0(path,"/output")

files <- list.files(input_path,full.names = T,pattern=".tif")

count_cells_equal_to_value <- function(x, values) {
  
  value_proportion <- 0
  for (value in values) {
    value_proportion <- value_proportion + (sum(x == value, na.rm = TRUE) / ncell(x))
  }
  
  return(value_proportion)
}


for (year in 1998:2013) {
  rasts_year <- lapply(files, rast, lyrs=year-1983) #each layer resembles a year from 1984 to 2014
  incidents_year <- filter(incidents,start_year==year+1)
  
  for (rast_year in rasts_year) {
    
    #rast_year[rast_year==255] <- NA
    
    tree_values <- terra::extract(rast_year,incidents_year,count_cells_equal_to_value,values=c(1,2))
    shrub_values <- terra::extract(rast_year,incidents_year,count_cells_equal_to_value,values=3)
    herb_values <- terra::extract(rast_year,incidents_year,count_cells_equal_to_value,values=4)
    
    
    incidents_year$tree_cover_p <- incidents_year$tree_cover_p + rowMeans(tree_values[2])
    incidents_year$shrub_cover_p <- incidents_year$shrub_cover_p + rowMeans(shrub_values[2])
    incidents_year$herb_cover_p <- incidents_year$herb_cover_p + rowMeans(herb_values[2])
  }
  
  if (year==1998) {
    new_incidents <- incidents_year
  } else {
    new_incidents <- rbind(new_incidents,incidents_year)
  }
}

new_incidents$tree_cover_p <- new_incidents$tree_cover_p * 100
new_incidents$shrub_cover_p <- new_incidents$shrub_cover_p* 100
new_incidents$herb_cover_p <- new_incidents$herb_cover_p * 100 


new_incidents <- st_set_geometry(new_incidents,NULL)
new_incidents <- subset(new_incidents,select=c(incident_id,tree_cover_p,shrub_cover_p,herb_cover_p))
write.csv(new_incidents,paste0(path,"/input/built/alaska/land_cover.csv"))