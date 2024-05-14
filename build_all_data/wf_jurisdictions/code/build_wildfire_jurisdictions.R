if (!require(snowfall)) install.packages('snowfall')
if (!require(sf)) install.packages('sf')
if (!require(raster)) install.packages('raster')
if (!require(foreign)) install.packages('foreign')
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(ggplot2)) install.packages('ggplot2')
if (!require(datasets)) install.packages('datasets')
library(snowfall)
# initialize the CPUs
sfInit(parall=TRUE, cpus = 41, type = "SOCK")
sfClusterSetupRNG(type="RNGstream")

#load other libraries
sfLibrary(sf)
sfLibrary(raster)
sfLibrary(foreign)
sfLibrary(tidyverse)
sfLibrary(ggplot2)
sfLibrary(datasets)

rm(list=ls())
path <- "/home/dashaa/wildfire_politics/data"
sf_use_s2(FALSE)

ensure_multipolygons <- function(X) {
  tmp1 <- tempfile(fileext = ".gpkg")
  tmp2 <- tempfile(fileext = ".gpkg")
  st_write(X, tmp1)
  ogr2ogr(tmp1, tmp2, f = "GPKG", nlt = "MULTIPOLYGON")
  Y <- st_read(tmp2)
  st_sf(st_drop_geometry(X), geom = st_geometry(Y))
}

incidents <- st_read(paste0(path,"/build_wildfire_reports/output/incidents_spatial.gpkg"))
incidents <- st_transform(incidents,crs=2163)

#need to determine in which agency jurisdiction each fire's point of origin lies first
#so downloading agency data
agencies <- st_read(paste0(path,"/build_wf_jurisdictions/input/built/agencies_valid.gpkg"))
agencies <- st_transform(agencies,crs=2163)

#now determining POO landowner
# POO_landowner <- apply(st_intersects(agencies,incidents,sparse=FALSE),2,
#                        function(col) {
#                          agencies[which(col),]$LandownerCategory
#                        })
# POO_landowner[lengths(POO_landowner)==0] <- NA_character_
# incidents$POO_landowner <- unlist(POO_landowner)
# write.csv(subset(st_set_geometry(incidents,NULL),select=c(incident_id,POO_landowner)),paste0(path,"/build_wf_jurisdictions/output/incidents_POO_landowners.csv"))

#now determining all landowners around the POO too
incidents <- st_transform(incidents, "+proj=utm +zone=12 +datum=WGS84")
incidents$final_sq_m <- incidents$final_acres*4046.8564224
incidents$radius <- sqrt(incidents$final_sq_m/pi)
incidents <- st_buffer(incidents,incidents$radius)
incidents <- subset(incidents,select=c(incident_id,final_sq_m))
incidents <- st_transform(incidents,crs=2163) 

find_fire_landowners <- function(incident_id_num) {
  incident <- filter(incidents,incident_id==incident_id_num)
  
  # Compute the intersection between the incident and agencies
  intersection <- st_intersection(incident, agencies)
  
  # Calculate the area of the intersection and the incident
  intersection_area <- st_area(intersection)
  incident_area <- incident$final_sq_m
  
  # Calculate the proportion and store it in the results dataframe
  intersection$proportions <- intersection_area / incident_area
  intersection <- st_set_geometry(intersection,NULL)
  intersection <- subset(intersection,select=c(incident_id,LandownerCategory,proportions))
  
  write.csv(intersection,paste0(path,"/build_wf_jurisdictions/input/built/landowners_",incident_id_num,".csv"))
}

# Export all the globals 
sfExportAll()

# Run the job
sfClusterApplyLB(incidents$incident_id, fun = find_fire_landowners)

# Stop the clusters
sfStop()

# #merging in cooperating agencies from sitreps
# sitreps <- read.csv(paste0(path,"/build_wildfire_reports/output/cleaned_sitreps.csv"))
# sitreps <- subset(sitreps,select=c(incident_id,addtnl_coop_orgz))
# incidents <- merge(incidents,sitreps,all.x=TRUE)
# rm(sitreps)
# 
# ####now trying to identify whether 1) the federal agency was participating 2) the state agency was participating
# #first by checking mtbs data
# incidents$federal_ind <- incidents$mtbs_federal_ind
# incidents$state_ind <- incidents$mtbs_state_ind
# #then by checking POO data
# incidents$federal_ind[(is.na(incidents$federal_ind)) & (incidents$POO_land_type=="Federal")] <- 1
# incidents$state_ind[(is.na(incidents$state_ind)) & (incidents$POO_landowner=="State")] <- 1
# #lastly by checking data on cooperating agencies
# #first federal
# fed_agencies <- c("federal","usfws"," dod "," nps ","usfs"," fs ","usda","us forest service","united states"," blm "," bia "," bor "," doi "," doe "," ancsa ")
# fed_agencies_ind <- as.data.frame(sapply(fed_agencies, grepl,x=incidents$addtnl_coop_orgz,ignore.case=TRUE))
# fed_agencies_ind <- as.data.frame(sapply(fed_agencies_ind,as.integer))
# fed_agencies_ind$fed_agencies_assist_ind <- apply(fed_agencies_ind,1,max)
# incidents$fed_agencies_assist_ind <- fed_agencies_ind$fed_agencies_assist_ind
# incidents$federal_ind[(is.na(incidents$federal_ind)) & (incidents$fed_agencies_assist_ind==1)] <- 1
# #then state
# state_abbs <- paste0(" ",state.abb," ")
# state_agencies <- c("state","calfire",state_abbs,state.name)
# state_agencies_ind <- as.data.frame(sapply(state_agencies, grepl,x=incidents$addtnl_coop_orgz,ignore.case=TRUE))
# state_agencies_ind <- as.data.frame(sapply(state_agencies_ind,as.integer))
# state_agencies_ind$state_agencies_assist_ind <- apply(state_agencies_ind,1,max)
# incidents$state_agencies_assist_ind <- state_agencies_ind$state_agencies_assist_ind
# incidents$state_ind[(is.na(incidents$state_ind)) & (incidents$state_agencies_assist_ind==1)] <- 1
# 
# fire_departments <- readLines(paste0(path,"/build_wf_jurisdictions/input/raw/usfa-registry-national.txt"))
