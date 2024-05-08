#merging in prepped ics and sitreps
if (!require(sf)) install.packages('sf')
if (!require(foreign)) install.packages('foreign')
if (!require(dplyr)) install.packages('dplyr')
if (!require(tidyr)) install.packages('tidyr')
if (!require(stringr)) install.packages('stringr')
library(sf)
library(foreign)
library(dplyr)
library(tidyr)
library(stringr)

rm(list=ls())
path <- "/home/dashaa/wildfire_politics/data/build_wildfire_reports"
sf_use_s2(FALSE)

ics <- st_read(paste0(path,"/output/incidents_spatial.gpkg"))

sitreps <- read.csv(paste0(path,"/output/cleaned_sitreps.csv"))
sitreps <- subset(sitreps,select=-c(X,INCIDENT_NUMBER,INCIDENT_NAME,CAUSE,COMPLEX,CURR_INC_AREA_UOM,CURR_INCIDENT_AREA,
                                  DISCOVERY_DATE,END_YEAR,FATALITIES,FOD_LATITUDE,
                                  FOD_LONGITUDE,FUEL_MODEL,IMT_MGMT_ORG_DESC,
                                  INC209R_IDENTIFIER,INCIDENT_DESCRIPTION,INC_IDENTIFIER,
                                  POO_CITY,POO_COUNTY,LOCAL_TIMEZONE,POO_SHORT_LOCATION_DESC,
                                  POO_STATE,POO_STATE_NAME,START_YEAR,SENT_DATE,
                                  SENT_TO,SRATEGIC_DISCUSSION,SRATEGIC_OBJECTIVES,
                                  STRATEGIC_NARR,INCTYP_DESC,INCTYP_ABBREVIATION,
                                  DISCOVERY_DOY))
                                  #ADDTNL_COOP_ASSIST_ORG_NARR,COMPLEXITY_LEVEL_NARR,
                                  #CRIT_RES_NEEDS_NARR,CURRENT_THREAT_NARR,
                                  #HAZARDS_MATLS_INVOLVMENT_NARR,INCIDENT_COMMANDERS_NARR,
                                  #INC_MGMT_ORG_DESC,LIFE_SAFETY_HEALTH_STATUS_NARR,
                                  #MAJOR_PROBLEMS,OBS_FIRE_BEHAVE,PLANNED_ACTIONS,
                                  #REMARKS,SIGNIF_EVENTS_SUMMARY,TARGETS_MET,
                                  #UNIT_OR_OTHER_NARR,WEATHER_CONCERNS_NARR))
merged <- merge(ics,sitreps,all.x=T)

#st_write(merged,dsn=paste0(path,"/output/incidents_with_sitreps_spatial.gpkg"),append=FALSE)
write.csv(st_set_geometry(merged,NULL),paste0(path,"/output/incidents_with_sitreps_nonspatial.csv"))