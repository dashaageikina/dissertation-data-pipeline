#building median housing values at the place level
if (!require(sf)) install.packages('sf')
if (!require(tidyverse)) install.packages('tidyverse')
library(sf)
library(tidyverse)

rm(list=ls())

path <- "/home/dashaa/wildfire_politics/data"
#load incidents data
incidents <- st_read(paste0(path,"/build_wildfire_reports/output/incidents_spatial.gpkg"))
incidents <- subset(incidents,select=c(incident_id,start_year,final_acres))
incidents <- st_transform(incidents, "+proj=utm +zone=12 +datum=WGS84")
incidents$final_sq_m <- incidents$final_acres*4046.8564224
incidents$radius <- sqrt(incidents$final_sq_m/pi)
incidents <- st_buffer(incidents,2*incidents$radius) #take extra to make sure there are housing values
incidents <- st_transform(incidents,crs=2163)

cities_path <- paste0(path,"/build_cities/input/raw/nhgis0003_shape")
housing_path <- paste0(path,"/build_home_value/input/raw/nhgis0003_csv")

# Function to calculate distances to cities based on population threshold
calculate_distance <- function(incidents_df, cities_df, population_threshold) {
  incident_distances <- sapply(seq_len(nrow(incidents_df)), function(i) {
    incident <- incidents_df[i, ]
    cities_within_range <- cities_df %>% filter(CL8AA >= population_threshold)
    min_distance <- min(st_distance(incident, cities_within_range))
    return(min_distance)
  })
  return(incident_distances)
}

years <- c("1990","2000","2010")
incidents$median_housing <- NA

for (year in years) {
  
  sub_incidents <- filter(incidents,start_year>as.numeric(year) & start_year<=as.numeric(year)+10)
  #load cities data
  setwd(cities_path)
  cities_year <- st_read(paste0("US_place_",year,".shp"))

  #load housing data
  setwd(housing_path)
  if (year=="1990") {
    housing <- read.csv(paste0(path,"/build_home_value/input/raw/nhgis0003_csv/nhgis0003_ds120_1990_place.csv"))
  } else if (year=="2000") {
    housing <- read.csv(paste0(path,"/build_home_value/input/raw/nhgis0003_csv/nhgis0003_ds151_2000_place.csv"))
    housing <- housing %>%
      rename(EST001 = GB7001)
  } else {
    housing <- read.csv(paste0(path,"/build_home_value/input/raw/nhgis0003_csv/nhgis0004_ds175_2010_place.csv"))
    housing <- housing %>%
      rename(EST001 = JFJE001)
  }
  housing <- subset(housing,select=c(GISJOIN,EST001))
  
  cities_year <- merge(cities_year,housing,all.x=T)
  cities_year <- st_transform(cities_year,st_crs(incidents))
  cities_year <- st_make_valid(cities_year)
  cities_year <- subset(cities_year,select=c(GISJOIN,EST001))
  
  # Step 1: Perform spatial join
  joined_data <- st_join(sub_incidents, cities_year, join = st_intersects)
  # Step 2: Compute average EST001 for incidents with overlapping geometries
  averages_overlap <- joined_data %>%
    group_by(incident_id) %>%
    summarise(average_EST001 = mean(EST001,na.rm = T))
  # Step 3: For incidents without overlapping geometries, find closest 5 values and compute average
  incidents_no_overlap <- filter(joined_data,is.na(GISJOIN))
  
  closest_values <- lapply(1:nrow(incidents_no_overlap), function(i) {
    distances <- st_distance(incidents_no_overlap[i, ], cities_year)
    nearest_indices <- order(distances)[1:5]  # Get indices of the 5 nearest features
    mean(cities_year$EST001[nearest_indices])  # Extract EST001 values using nearest_indices
  })
  
  # Combine results
  final_result <- rbind(
    data.frame(incident_id = unique(averages_overlap$incident_id), EST001 = averages_overlap$average_EST001),
    data.frame(incident_id = incidents_no_overlap$incident_id, EST001 = unlist(closest_values))
  )
  

  if (year==years[1]) {
    new_incidents <- final_result
  } else {
    new_incidents <- rbind(new_incidents,final_result)
  }
  
}

write.csv(new_incidents,paste0(path,"/build_home_value/output/incidents_median_home_value_place.csv"))








