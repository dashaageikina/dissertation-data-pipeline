if (!require(tidyverse)) install.packages('tidyverse')
if (!require(lubridate)) install.packages('lubridate')
library(tidyverse)
library(lubridate)

rm(list=ls())


path <- "/home/dashaa/wildfire_politics/data"
big_news_days <- readRDS(paste0(path,"/build_big_news/output/big_news_days.rds"))

###now importing the incidents
incidents <- read.csv(paste0(path,"/build_wildfire_reports/output/incidents_nonspatial.csv"))
#get date variables
date_vars <- c("discovery_date","wf_peak_personnel_date","wf_peak_aerial_date","wf_cessation_date")

incidents <- incidents %>%
    select(all_of(c("incident_id",date_vars)))

for (date_var in date_vars) {
  incidents[[date_var]] <- substr(incidents[[date_var]],1,10)
  incidents[[date_var]] <- as.Date(incidents[[date_var]],format="%Y-%m-%d")
}

#get column names of big_news_days
news_columns <- setdiff(colnames(big_news_days),"day")
#create a vector with instrument types
instrum_types <- c("dur_share","pre_n","prepa_n","prepp_n")

for (news_column in news_columns) {
  for (instrum_type in instrum_types) {
    incidents[[paste(news_column,instrum_type,sep="_")]] <- NA
  }
}

compute_bignews <- function(incident,instrum_type,news_column) {
  
  split_parts=str_split(instrum_type,"_")
  type=split_parts[[1]][1]
  variable=split_parts[[1]][2]
  
  incidents_subset <- incidents[incidents$incident_id==incident,]
  
  discovery_date <- incidents_subset$discovery_date
  wf_peak_personnel_date <- incidents_subset$wf_peak_personnel_date
  wf_peak_aerial_date <- incidents_subset$wf_peak_aerial_date
  wf_cessation_date <- incidents_subset$wf_cessation_date
  if (type=="prepp" & is.na(wf_peak_personnel_date)) {
      return(NA)
  }
  if (type=="prepa" & is.na(wf_peak_aerial_date)) {
      return(NA)
  }
  
  start <- discovery_date
  end <- wf_cessation_date
  if (type=="pre") {
    start <- discovery_date-7
    end <- discovery_date-1
  } else if (type=="prepa") {
    start <- wf_peak_aerial_date - 7
    end <- wf_peak_aerial_date - 1
  } else if (type=="prepp") {
    start <- wf_peak_personnel_date - 7
    end <- wf_peak_personnel_date - 1
  }
  
  if (start>end) {
    return(NA)
  }
  
  bignews_subset <- filter(big_news_days,day>=start & day<=end)
  bignews_subset <- bignews_subset %>%
    select(day, !!sym(news_column)) %>%
    rename(all_articles = !!sym(news_column))
  
  if (variable=="share") {
    n_news <- length(unique(unlist(bignews_subset$news_column)))/nrow(bignews_subset)
  } else {
    n_news <- length(unique(unlist(bignews_subset$news_column)))
  }
  
    return(n_news)

}


#assigning indices to the wildfires
for (incident in unique(incidents$incident_id)) {

  for (news_column in news_columns) {
    for (instrum_type in instrum_types) {
      incidents[[paste(news_column,instrum_type,sep="_")]][incidents$incident_id==incident] <- compute_bignews(incident,instrum_type,news_column)
    }
  }


}

incidents <- incidents %>%
  select(-all_of(date_vars))

saveRDS(incidents,paste0(path,"/build_big_news/output/big_news_incidents.rds"))
