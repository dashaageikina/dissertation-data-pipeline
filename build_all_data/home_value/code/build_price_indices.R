#building HPIs at the tract/zipcode/county/state level
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(readxl)) install.packages('readxl')
library(tidyverse)
library(readxl)

rm(list=ls())

incidents <- read.csv("/home/dashaa/wildfire_politics/data/build_wildfire_reports/output/incidents_nonspatial.csv")

path <- "/home/dashaa/wildfire_politics/data/build_home_value"

#first get tracts hpi's
hpi_tracts <- read.csv(paste0(path,"/input/raw/FHFA/HPI_AT_BDL_tract.csv"))

housing <- subset(hpi_tracts,select=c(tract,year,hpi))
housing <- housing %>%
  mutate(tract=as.character(tract),
         year=year+1)

incidents_hpi <- incidents %>%
  separate_rows(housing_tracts, sep = ",")

result <- incidents_hpi %>%
  left_join(housing, by = c("housing_tracts" = "tract","start_year" = "year")) %>%
  group_by(incident_id) %>%
  summarise(hpi = mean(hpi),na.rm=TRUE) %>%
  subset(select=c(incident_id,hpi)) %>%
  rename("hpi_tract"="hpi")

#now zipcode-level
hpi_zips <- read_excel(paste0(path,"/input/raw/FHFA/HPI_AT_BDL_ZIP5.xlsx"),skip=6)
housing <- subset(hpi_zips,select=c(`Five-Digit ZIP Code`,Year,HPI))
housing <- housing %>%
  mutate(Year=as.numeric(Year)+1,
          HPI=as.numeric(HPI))
  
incidents_hpi <- incidents %>%
  separate_rows(nearby_zipcodes, sep = ",")
  
result2 <- incidents_hpi %>%
  left_join(housing, by = c("nearby_zipcodes" = "Five-Digit ZIP Code","start_year" = "Year")) %>%
  group_by(incident_id) %>%
  summarise(HPI = mean(HPI),na.rm=TRUE) %>%
  subset(select=c(incident_id,HPI)) %>%
  rename("hpi_zipcode"="HPI")
  
result <- merge(result,result2)
  
#now county
hpi_counties <- read_excel(paste0(path,"/input/raw/FHFA/HPI_AT_BDL_county.xlsx"),skip=6)
housing <- subset(hpi_counties,select=c(`FIPS code`,Year,HPI))
housing <- housing %>%
  mutate(Year=as.numeric(Year)+1,
         HPI=as.numeric(HPI),
         `FIPS code`=as.numeric(`FIPS code`))

result2 <- incidents %>%
  left_join(housing, by = c("POO_fips" = "FIPS code","start_year" = "Year"))

result2 <- result2 %>%
  subset(select=c(incident_id,HPI)) %>%
  rename("hpi_county"="HPI")

result <- merge(result,result2)

#now state
hpi_states <- read_excel(paste0(path,"/input/raw/FHFA/HPI_AT_BDL_state.xlsx"),skip=6)
housing <- subset(hpi_states,select=c(Abbreviation,Year,HPI))
housing <- housing %>%
  mutate(Year=as.numeric(Year)+1,
         HPI=as.numeric(HPI))

result2 <- incidents %>%
  left_join(housing, by = c("POO_state" = "Abbreviation","start_year" = "Year"))

result2 <- result2 %>%
  subset(select=c(incident_id,HPI)) %>%
  rename("hpi_state"="HPI")

result <- merge(result,result2)

#now adjust hpi's so that they all correspond to the same level

result$hpi <- result$hpi_tract

incident_example <- "1999_AK-STA-901106_HELMAUR (CLARK WOLVERINE)"
tract_example <- result$hpi_tract[result$incident_id==incident_example]
zip_example <- result$hpi_zipcode[result$incident_id==incident_example]
county_example <- result$hpi_county[result$incident_id==incident_example]
state_example <- result$hpi_state[result$incident_id==incident_example]

result$hpi[is.na(result$hpi)] <- result$hpi_zipcode[is.na(result$hpi)]*tract_example/zip_example
result$hpi[is.na(result$hpi)] <- result$hpi_county[is.na(result$hpi)]*tract_example/county_example
result$hpi[is.na(result$hpi)] <- result$hpi_state[is.na(result$hpi)]*tract_example/state_example

result <- subset(result,select=c(incident_id,hpi))
write.csv(result,paste0(path,"/output/incident_hpis.csv"))




