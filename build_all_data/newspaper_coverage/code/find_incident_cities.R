#getting data on wildfire citations by locations in the newspapers from newslibrary.com
if (!require(snowfall)) install.packages('snowfall')
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(sf)) install.packages('sf')
library(snowfall)

# initialize the CPUs
sfInit(parall=TRUE, cpus = 10, type = "SOCK")
sfClusterSetupRNG(type="RNGstream")

sfLibrary(tidyverse)
sfLibrary(sf)

rm(list=ls())
path <- "/home/dashaa/wildfire_politics/data"
incidents <- st_read(paste0(path,"/build_wildfire_reports/output/incidents_spatial.gpkg"))
incidents <- subset(incidents,select=c(incident_id,discovery_date,wf_cessation_date,POO_fips,poo_short_location_desc))

cities <- st_read(paste0(path,"/build_newspaper_coverage/input/raw/stanford-bx729wr3020-shapefile"))
cities <- st_transform(cities,4269)

#find two closest cities/towns to the incident point of origin
find_incident_locations <- function(incidents,cities,this_incident_id) {
  
  incident_row <- filter(incidents,incident_id==this_incident_id)
  
  city1_index <- st_nearest_feature(incident_row,cities)
  city1 <- cities$name[city1_index]
  county1 <- cities$county[city1_index]
  
  newcities <- cities[-city1_index,]
  
  city2_index <- st_nearest_feature(incident_row,newcities)
  city2 <- newcities$name[city2_index]
  county2 <- newcities$county[city2_index]
  
  return(data.frame(this_incident_id,city1,county1,city2,county2))
  
}

# for (i in 1:nrow(incidents)) {
#   
#   incident <- incidents[i,]
#   
#   city1_index <- st_nearest_feature(incident,cities)
#   incidents$city1[i] <- cities$name[city1_index]
#   incidents$county1[i] <- cities$county[city1_index]
#   
#   newcities <- cities[-city1_index,]
#   
#   city2_index <- st_nearest_feature(incident,newcities)
#   incidents$city2[i] <- newcities$name[city2_index]
#   incidents$county2[i] <- newcities$county[city2_index]
#   
# }

incident_locations <- sfLapply(incidents$incident_id, fun = find_incident_locations,incidents=incidents,cities=cities)

all_incident_locations <- incident_locations[[1]]

for (x in 2:length(incident_locations)) {
    all_incident_locations <- rbind(all_incident_locations,incident_locations[[x]])
}
names(all_incident_locations) <- c("incident_id","city1","county1","city2","county2")

incidents <- merge(st_set_geometry(incidents,NULL),all_incident_locations)
#sfClusterApplyLB(incidents$incident_id, fun = function(y) {input2Means(x=variables, month=y, zones=zones)})


saveRDS(incidents,paste0(path,"/build_newspaper_coverage/input/built/incidents_with_cities.rds"))
write.csv(incidents,paste0(path,"/build_newspaper_coverage/input/built/incidents_with_cities.csv"))

# Stop the clusters
sfStop()




