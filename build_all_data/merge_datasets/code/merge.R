#merging data
if (!require(foreign)) install.packages('foreign')
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(haven)) install.packages('haven')
if (!require(lubridate)) install.packages('lubridate')
if (!require(haven)) install.packages('haven')
if (!require(readxl)) install.packages('readxl')
library(foreign)
library(tidyverse)
library(haven)
library(lubridate)
library(haven)
library(readxl)

rm(list=ls())
path <- "/home/dashaa/wildfire_politics/data"

#uploading data on incidents and sitreps
incidents <- read.csv(paste0(path,"/build_wildfire_reports/output/incidents_nonspatial.csv"))
incidents <- incidents %>%
  subset(select=-X) %>%
  mutate(mean_aerial=total_aerial_sum/Ndays,
         mean_personnel=total_personnel_sum/Ndays,
         wf_max_growth_date=as.Date(substr(wf_max_growth_date,1,10),format="%Y-%m-%d"),
         discovery_date=as.Date(substr(discovery_date,1,10),format="%Y-%m-%d"),
         wf_cessation_date=as.Date(substr(wf_cessation_date,1,10),format="%Y-%m-%d"),
         wf_peak_personnel_date=as.Date(substr(wf_peak_personnel_date,1,10),format="%Y-%m-%d"),
         wf_peak_aerial_date=as.Date(substr(wf_peak_aerial_date,1,10),format="%Y-%m-%d"))
#create a variable representing the number of other wildfires that were happening 
#in the same GACC in a period coinciding with a given wildfire
# Create an empty column to store the count
incidents <- incidents %>%
  group_by(gacc_label) %>%
  mutate(
    other_wildfires = map_dbl(row_number(), function(row) {
      start_date <- discovery_date[row]
      end_date <- wf_cessation_date[row]
      sum(discovery_date <= end_date & wf_cessation_date >= start_date) - 1
    }),
    other_wildfires_pa = map_dbl(row_number(), function(row) {
      start_date <- discovery_date[row]
      end_date <- wf_peak_aerial_date[row]
      sum(discovery_date <= end_date & wf_peak_aerial_date >= start_date) - 1
    }),
    other_wildfires_pp = map_dbl(row_number(), function(row) {
      start_date <- discovery_date[row]
      end_date <- wf_peak_personnel_date[row]
      sum(discovery_date <= end_date & wf_peak_personnel_date >= start_date) - 1
    })
  ) %>%
  ungroup()

#assign NYT_fire=1 to fires that were mentioned on the New York Times front pages
incidents$NYT_fire <- 0
incidents$NYT_fire[incidents$discovery_date<="2000-08-12" & incidents$wf_cessation_date>"2000-08-12" & incidents$POO_state %in% c("CA","AK","AZ","ID","MT","CO","NV","OR","UT","WA","WY")] <- 1
incidents$NYT_fire[incidents$incident_id=="2001_CA-SHU-07984_OREGON"] <- 1
incidents$NYT_fire[incidents$discovery_date<="2000-06-23" & incidents$wf_cessation_date>"2000-06-23" & incidents$POO_state %in% c("CA","AK","AZ","ID","MT","CO","NV","OR","UT","WA","WY")] <- 1
incidents$NYT_fire[incidents$discovery_date<="2003-10-28" & incidents$wf_cessation_date>"2003-10-28" & incidents$POO_state=="CA"] <- 1

#adding data on population
pop <- read.csv(paste0(path,"/build_population/input/raw/nhgis0001_csv/nhgis0001_ts_nominal_tract.csv"))
pop <- filter(pop,YEAR!="2010-2014")
pop <- subset(pop,select=c("GISJOIN","YEAR","AV0AA"))
pop$YEAR <- as.numeric(pop$YEAR)

merged <- incidents %>%
  left_join(pop, by = c("POO_tract" = "GISJOIN","census_year" = "YEAR"))

merged$pop_density <- merged$AV0AA/merged$POO_tract_area
merged$pop_density2 <- merged$pop_density*10000

#adding data on housing
housing <- read.csv(paste0(path,"/build_home_value/output/incident_hpis.csv"))
housing <- subset(housing,select=-X)
merged <- merge(merged,housing,all.x=TRUE)

#adding data on cities
cities <- read.csv(paste0(path,"/build_cities/output/incidents_to_cities.csv"))
merged <- merge(merged,cities,all.x=TRUE)

#adding data on landowners
landowners <- read.csv(paste0(path,"/build_wf_jurisdictions/output/incidents_POO_landowners.csv"))
landowners <- subset(landowners,select=-X)
merged <- merge(merged,landowners,all.x=TRUE)

#adding data on elevation and slope
elevation <- read.csv(paste0(path,"/build_terrain/output/incidents_elevation.csv"))
elevation <- subset(elevation,select=-c(X,final_acres,total_cells))
slope <- read.csv(paste0(path,"/build_terrain/output/incidents_slope.csv"))
slope <- subset(slope,select=-c(X,final_acres,total_cells))
merged <- merge(merged,elevation,all.x=TRUE)
merged <- merge(merged,slope,all.x=TRUE)

#add vegetation data
vegetation <- read.csv(paste0(path,"/build_land_cover/output/incidents_land_cover.csv"))
vegetation <- subset(vegetation,select=-X)
merged <- merge(merged,vegetation,all.x=TRUE)

#adding data on crime
crime <- read.csv(paste0(path,"/build_crime/output/incident_level_crime.csv"))
crime <- subset(crime,select=-X)

merged <- merge(merged,crime,all.x=TRUE)

#merging in newspaper coverage and adding an additional variable for newspaper coverage
news <- readRDS(paste0(path,"/build_newspaper_coverage/output/N_fire_name_articles.rds"))

merged <- merge(merged,news,all.x=TRUE)
merged <- merged %>%
  mutate_at(c("N_all_articles","N_mean_daily_articles","N_median_daily_articles",
              "N_max_daily_articles","N_75p_daily_articles"), ~replace_na(.,0)) %>%
  mutate(across(ends_with("_pp")|ends_with("_pa"),~replace(.,N_all_articles==0,0))) %>%
  mutate(Fire_name_mentioned=ifelse(N_all_articles>0,1,0))

#merging in entropies data
nyt <- readRDS(paste0(path,"/build_info_entropy/output/entropies.rds"))
merged <- merge(merged,nyt,by="incident_id",all=TRUE)

#merging in big news data
big_news <- readRDS(paste0(path,"/build_big_news/output/big_news_incidents.rds"))
merged <- merge(merged,big_news,all.X=TRUE)

#merging in data on sports events
#sports <- read.csv(paste0(path,"/build_big_news/output/sports_incidents.csv"))
#sports <- subset(sports,select=-X)
#merged <- merge(merged,sports,all.X=TRUE)

#merge in weather
weather <- readRDS(paste0(path,"/build_merra2/output/incident_weather.rds"))
merged <- merge(merged,weather,all.X=TRUE)

#create a variable for season of the wildfire for fixed effects
merged  <- merged %>%
  mutate(season=
           case_when(month(wf_max_growth_date) >= 12 | month(wf_max_growth_date) <= 2 ~ "winter",
                     month(wf_max_growth_date) >= 3 & month(wf_max_growth_date) <= 5 ~ "spring",
                     month(wf_max_growth_date) >= 6 & month(wf_max_growth_date) <= 8 ~ "summer",
                     month(wf_max_growth_date) >= 9 & month(wf_max_growth_date) <= 11 ~ "fall"))

merged[is.na(merged$season),] <- merged[is.na(merged$season),] %>%
  mutate(season=case_when(month(discovery_date) >= 12 | month(discovery_date) <= 2 ~ "winter",
                          month(discovery_date) >= 3 & month(discovery_date) <= 5 ~ "spring",
                          month(discovery_date) >= 6 & month(discovery_date) <= 8 ~ "summer",
                          month(discovery_date) >= 9 & month(discovery_date) <= 11 ~ "fall"))

#adding a controls for huge wildfires, gacc fixed effects and landowners fixed effects
merged <- merged %>%
  mutate(huge340=ifelse(final_acres>340000,1,0),
         huge185=ifelse(final_acres>185000,1,0),
         huge115=ifelse(final_acres>115000,1,0),
         huge100=ifelse(final_acres>100000,1,0),
         huge95=ifelse(final_acres>95000,1,0),
         gacc_gb=ifelse(gacc_label=="Great Basin",1,0),
         gacc_s=ifelse(gacc_label=="Southern",1,0),
         gacc_sw=ifelse(gacc_label=="Southwest",1,0),
         gacc_so=ifelse(gacc_label=="South Ops",1,0),
         gacc_no=ifelse(gacc_label=="North Ops",1,0),
         gacc_rm=ifelse(gacc_label=="Rocky Mountain",1,0),
         gacc_nr=ifelse(gacc_label=="Nothern Rockies",1,0),
         gacc_e=ifelse(gacc_label=="Eastern",1,0),
         gacc_nw=ifelse(gacc_label=="Northwest",1,0),
         private=ifelse(POO_landowner=="Private",1,0),
         state=ifelse(POO_landowner=="State",1,0),
         federal=ifelse(private==0 & state==0,1,0))


#make some fixes
merged <- merged %>%
  mutate(nearby_tracts=ifelse(is.na(nearby_tracts),"",nearby_tracts),
         housing_tracts=ifelse(is.na(housing_tracts),"",housing_tracts),
         nearby_zipcodes=ifelse(is.na(nearby_zipcodes),"",nearby_zipcodes)) %>%
  subset(select=-c(nearby_tracts1990,nearby_tracts2000,nearby_tracts2010,nearby_tracts2020))

write.csv(merged,paste0(path,"/merge_datasets/output/merged_incidents_only.csv"))
write_dta(merged,paste0(path,"/merge_datasets/output/merged.dta"))
