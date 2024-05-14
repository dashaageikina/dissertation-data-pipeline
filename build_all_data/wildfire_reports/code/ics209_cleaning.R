######clean and transform ICS-209-PLUS dataset
if (!require(sf)) install.packages('sf')
if (!require(raster)) install.packages('raster')
if (!require(foreign)) install.packages('foreign')
if (!require(ggplot2)) install.packages('ggplot2')
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(geodata)) install.packages('geodata')
library(sf)
library(raster)
library(foreign)
library(ggplot2)
library(tidyverse)
library(geodata)

rm(list=ls())
path <- "main_project/build_all_data"
sf_use_s2(FALSE)

###function checking the validity of multipolygons
ensure_multipolygons <- function(X) {
  tmp1 <- tempfile(fileext = ".gpkg")
  tmp2 <- tempfile(fileext = ".gpkg")
  st_write(X, tmp1)
  ogr2ogr(tmp1, tmp2, f = "GPKG", nlt = "MULTIPOLYGON")
  Y <- st_read(tmp2)
  st_sf(st_drop_geometry(X), geom = st_geometry(Y))
}

###upload the data
url <- "https://figshare.com/ndownloader/files/20183699"
input_path <- paste0(path,"/wildfire_reports/input/raw")
download.file(url, paste0(input_path,"/ics209plus.zip"),mode="wb")
setwd(input_path)
unzip(paste0(input_path,"/ics209plus.zip"))
setwd(paste0(input_path,"/ics209_cleaned"))
files <- list.files(getwd(), full.names = TRUE)
sapply(files,unzip)
sapply(files,unlink)

###import and clean the main dataset
incidents <- st_read("ics209_incidents_spatial/ics209-plus_incidents_1999to2014.gpkg")
incidents <- st_transform(incidents,crs=2163)
incidents <- incidents[incidents$inctyp_abbreviation=="WF",]
#removing variables I won't need
incidents <- subset(incidents,select=-c(expected_containment_date,ll_confidence,
                                        ll_update,projected_final_im_cost,
                                        us_l3name,na_l2name,na_l1name,hexid50k,
                                        inc_mgmt_num_sitreps))
#removing rows with no data on dates or incident_id + those for which discovery is
#after cessation
incidents <- incidents %>%
  filter(discovery_date != "" & wf_cessation_date != "" & !is.na(incident_id)) %>%
  mutate(discovery_date2 = as.Date(substr(discovery_date, 1, 10), format = "%Y-%m-%d"),
         wf_cessation_date2 = as.Date(substr(wf_cessation_date, 1, 10), format = "%Y-%m-%d")) %>%
  filter(discovery_date2 <= wf_cessation_date2)
#add the number of days between cessation and discovery
incidents <- incidents %>%
  mutate(Ndays = as.numeric(wf_cessation_date2 - discovery_date2 + 1)) %>%
  select(-c(discovery_date2, wf_cessation_date2))

#removing duplicate incidents
incidents <- incidents[!duplicated(incidents), ]
duplicate_ids <- incidents$incident_id[duplicated(incidents$incident_id)]
incidents <- filter(incidents,!(incident_id %in% duplicate_ids))

#fix missing aerial and personnel where it should be 0
setwd(input_path)
sitreps <- read.csv("ics209-plus/ics209-plus_sitreps_1999to2014.csv")
sitreps <- sitreps[sitreps$INCTYP_ABBREVIATION=="WF",]
summarized_sitreps <- sitreps %>%
  group_by(INCIDENT_ID) %>%
  summarise(
    TOTAL_AERIAL_SUM = sum(TOTAL_AERIAL),
    TOTAL_PERSONNEL_SUM = sum(TOTAL_PERSONNEL)
  )
zero_aerial <- filter(summarized_sitreps,TOTAL_AERIAL_SUM==0)
zero_personnel <- filter(summarized_sitreps,TOTAL_PERSONNEL_SUM==0)
for (inc_id in zero_aerial$INCIDENT_ID) {
  incidents$total_aerial_sum[incidents$incident_id==inc_id] <- 0
}
for (inc_id in zero_personnel$INCIDENT_ID) {
  incidents$total_personnel_sum[incidents$incident_id==inc_id] <- 0
}

incidents$wf_peak_personnel[incidents$total_personnel_sum==0] <- 0
incidents$wf_peak_aerial[incidents$total_aerial_sum==0] <- 0

###add new variables
#seasons for start and for end of each fire
incidents <- incidents %>%
  mutate(discovery_month = as.numeric(substr(discovery_date, 6, 7)),
         cessation_month = as.numeric(substr(wf_cessation_date, 6, 7)),
         discovery_season = case_when(
           discovery_month >= 9 & discovery_month <= 11 ~ "fall",
           discovery_month == 12 | discovery_month <= 2 ~ "winter",
           discovery_month >= 3 & discovery_month <= 5 ~ "spring",
           TRUE ~ "summer"
         ),
         cessation_season = case_when(
           cessation_month >= 9 & cessation_month <= 11 ~ "fall",
           cessation_month == 12 | cessation_month <= 2 ~ "winter",
           cessation_month >= 3 & cessation_month <= 5 ~ "spring",
           TRUE ~ "summer"
         )) %>%
  select(-c(discovery_month, cessation_month))

#construct approximate incident areas
incidents_radii <- incidents %>%
  st_transform("+proj=utm +zone=12 +datum=WGS84") %>%
  mutate(final_sq_m = final_acres * 4046.8564224,
         radius = sqrt(final_sq_m / pi)) %>%
  st_buffer(dist = .$radius) %>%
  select(incident_id, geom) %>%
  st_transform(crs = 2163)

#generating own POO_state - state of the point of origin
state_input_path <- paste0(input_path,"/states")
dir.create(state_input_path)
#download GIS data on states from geodata library
state_boundaries <- st_as_sf(gadm("USA", path=state_input_path, level = 1))
state_boundaries <- state_boundaries %>%
  st_transform(crs=2163) %>%
  mutate(state_abbr=substr(HASC_1,4,5))
#find state for each incident
POO_state <- apply(st_intersects(state_boundaries,incidents,sparse=FALSE),2,
                   function(col) {
                     state_boundaries[which(col),]$state_abbr
                   })
POO_state[lengths(POO_state)==0] <- NA_character_
names(incidents)[names(incidents)=="poo_state"] <- "poo_state_original"
incidents$POO_state <- unlist(POO_state)

#now POO_county - county of the point of origin
#download GIS data on counties with fips codes online
url2 <- "https://www.weather.gov/source/gis/Shapefiles/County/c_19se23.zip"
download.file(url2, paste0(input_path,"/c_19se23.zip"),mode="wb")
setwd(input_path)
unzip(paste0(input_path,"/c_19se23.zip"))
counties <- st_read("c_19se23/c_19se23.shp")
counties <- counties %>%
  mutate(FIPS=as.numeric(FIPS)) %>%
  st_transform(crs=2163)
#find county for each incident
POO_fips <- apply(st_intersects(counties,incidents,sparse=FALSE),2,
                  function(col) {
                    counties[which(col),]$FIPS
                  })
POO_fips[lengths(POO_fips)==0] <- NA_character_
incidents$POO_fips <- unlist(POO_fips)

#POO_zipcode - zipcode of the point of origin
zipcodes <- st_read("zipcodes/USA_ZIP_Code_Boundaries.kml")
zipcodes <- st_transform(zipcodes,crs=2163)

POO_zipcode <- apply(st_intersects(zipcodes,incidents,sparse=FALSE),2,
                   function(col) {
                     zipcodes[which(col),]$ZIP_CODE
                   })
POO_zipcode[lengths(POO_zipcode)==0] <- NA_character_
incidents$POO_zipcode <- unlist(POO_zipcode)

#alse add other zipcodes that intersect the approximate fire area
zipcodes <- st_make_valid(zipcodes)
intersection_result <- st_intersection(incidents_radii, zipcodes)
result <- intersection_result %>%
  group_by(incident_id) %>%
  summarize(nearby_zipcodes = list(unique(ZIP_CODE)))

result <- st_set_geometry(result,NULL)
result$nearby_zipcodes <- sapply(result$nearby_zipcodes, function(x) paste(x, collapse = ","))

incidents_radii2 <- left_join(incidents_radii, result, by = c("incident_id" = "incident_id"))
incidents_radii2 <- st_set_geometry(incidents_radii2,NULL)
incidents <- merge(incidents,incidents_radii2)

#add census tracts of the point of origin
years <- c("1990","2000","2010","2020")
for (year in years) {
  tracts <- st_read(paste0("census_tracts/nhgis0001_shape/",year,"/US_tract_",year,".shp"))
  tracts <- st_transform(tracts,crs=2163)
  if ("Shape_area" %in% colnames(tracts)) {
    colnames(tracts)[colnames(tracts) == "Shape_area"] <- "SHAPE_AREA"
  }
  if ("Shape_Area" %in% colnames(tracts)) {
    colnames(tracts)[colnames(tracts) == "Shape_Area"] <- "SHAPE_AREA"
  }
  
  POO_tract <- apply(st_intersects(tracts,incidents,sparse=FALSE),2,
                    function(col) {
                      tracts[which(col),]$GISJOIN
                    })
  POO_tract[lengths(POO_tract)==0] <- NA_character_
  incidents$POO_tract <- unlist(POO_tract)
  
  tracts_area <- subset(tracts,select=c(GISJOIN,SHAPE_AREA))
  tracts_area <- st_set_geometry(tracts_area,NULL)
  incidents <- incidents %>%
    left_join(tracts_area, by = c("POO_tract" = "GISJOIN"))
  
  colnames(incidents)[colnames(incidents) == "POO_tract"] <- paste0("POO_tract",year)
  colnames(incidents)[colnames(incidents) == "SHAPE_AREA"] <- paste0("POO_tract_area",year)

  intersection_result <- st_intersection(incidents_radii, tracts)
  
  # Group by incidents and create a list of intersected GISJOIN values
  result <- intersection_result %>%
    group_by(incident_id) %>%
    summarize(nearby_tracts = list(unique(GISJOIN)),
              census_tracts_area=sum(SHAPE_AREA))
  result <- st_set_geometry(result,NULL)
  result <- subset(result,select=c(incident_id,nearby_tracts,census_tracts_area))

  result$nearby_tracts <- sapply(result$nearby_tracts, function(x) paste(x, collapse = ","))
  
  # Merge the result back into the incidents_radii dataframe
  incidents_radii <- left_join(incidents_radii, result, by = c("incident_id" = "incident_id"))
  colnames(incidents_radii)[colnames(incidents_radii) == "nearby_tracts"] <- paste0("nearby_tracts",year)
  colnames(incidents_radii)[colnames(incidents_radii) == "census_tracts_area"] <- paste0("census_tracts_area",year)
  
}
#now assign unique tract numbers based on the year of the fire
incidents_radii <- st_set_geometry(incidents_radii,NULL)
incidents <- merge(incidents,incidents_radii)
incidents$census_year <- 1990
incidents$POO_tract <- incidents$POO_tract1990
incidents$POO_tract_area <- incidents$POO_tract_area1990
incidents$nearby_tracts <- incidents$nearby_tracts1990
incidents$census_tracts_area <- incidents$census_tracts_area1990
incidents <- incidents %>%
  mutate(census_year = ifelse(start_year >= 2001 & start_year <= 2010, 2000, census_year),
         census_year = ifelse(start_year >= 2011, 2010, census_year),
         POO_tract = ifelse(start_year >= 2001 & start_year <= 2010, POO_tract2000, POO_tract),
         POO_tract = ifelse(start_year >= 2011, POO_tract2010, POO_tract),
         POO_tract_area = ifelse(start_year >= 2001 & start_year <= 2010, POO_tract_area2000, POO_tract_area),
         POO_tract_area = ifelse(start_year >= 2011, POO_tract_area2010, POO_tract_area),
         nearby_tracts = ifelse(start_year >= 2001 & start_year <= 2010, nearby_tracts2000, nearby_tracts),
         nearby_tracts = ifelse(start_year >= 2011, nearby_tracts2010, nearby_tracts),
         census_tracts_area = ifelse(start_year >= 2001 & start_year <= 2010, census_tracts_area2000, census_tracts_area),
         census_tracts_area = ifelse(start_year >= 2011, census_tracts_area2010, census_tracts_area))

#get an alternative tract number for merging with housing later (in merge.R) based on the most recent 2020 census tract
tracts$STATEFP <- as.character(as.numeric(tracts$STATEFP))
tracts$housing_tract <- paste0(tracts$STATEFP,tracts$COUNTYFP,tracts$TRACTCE)
tracts <- subset(tracts,select=c(GISJOIN,housing_tract))
tracts <- st_set_geometry(tracts,NULL)

incidents2 <- incidents %>%
  separate_rows(nearby_tracts2020, sep = ",")

result <- incidents2 %>%
  left_join(tracts, by = c("nearby_tracts2020" = "GISJOIN")) %>%
  group_by(incident_id) %>%
  summarise(housing_tracts = paste0(housing_tract, collapse=","))

result <- st_set_geometry(result,NULL)

incidents <- merge(incidents,result,all.x=TRUE)

#save spatial data
incidents <- st_transform(incidents,crs=4269)
st_write(incidents,dsn=paste0(path,"/build_wildfire_reports/output/incidents_spatial.gpkg"),append=FALSE)
#save just csv data
write.csv(st_set_geometry(incidents,NULL),paste0(path,"/build_wildfire_reports/output/incidents_nonspatial.csv"))
