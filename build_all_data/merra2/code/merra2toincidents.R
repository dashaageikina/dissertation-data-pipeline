#connect merra2 data to incidents
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

rm(list=ls())

path <- "/home/dashaa/wildfire_politics/data"

weather_files <- c(list.files(paste0(path,"/build_merra2/input/built"),full.names=T),
                   list.files(paste0(path,"/build_merra2/input/built/fixed"),full.names=T))
index_to_remove <- which(weather_files == "/home/dashaa/wildfire_politics/data/build_merra2/input/built/fixed")
weather_files <- weather_files[-index_to_remove]

fire_dates <- read.csv(paste0(path,"/build_wildfire_reports/output/incidents_nonspatial.csv"))
fire_dates <- subset(fire_dates,select=c(incident_id,wf_peak_aerial_date,wf_peak_personnel_date,wf_cessation_date))

for (weather_file in weather_files) {
  
  weather <- readRDS(weather_file)
  
  incidents <- weather[[1]]
  for (i in 2:length(weather)) {
    incidents <- rbind(incidents,weather[[i]])
  }

  if (weather_file==weather_files[1]) {
    allincidents <- incidents
  } else {
    allincidents <- rbind(allincidents,incidents)
  }

}

#change units of measurement from Kelvin to Celsius
allincidents <- allincidents %>%
  mutate(T2M_mean=T2M_mean-273.15,
         T2M_max=T2M_max-273.15,
         T2M_mean2=(sqrt(T2M_mean2)-273.15)**2,
         T2M_max2=(sqrt(T2M_max2)-273.15)**2,
         T2M_mean3=(T2M_mean3^(1/3)-273.15)**3,
         T2M_max3=(T2M_max3^(1/3)-273.15)**3,
         T2M_mean4=(T2M_mean4^0.25-273.15)**4,
         T2M_max4=(T2M_max4^0.25-273.15)**4)

allincidents <- merge(allincidents,fire_dates,all.x=TRUE)
  
allincidents <- allincidents %>%
    group_by(incident_id) %>% summarise(wind_speed_mean=mean(wind_speed_2M_mean),
                                        wind_speed_max=max(wind_speed_2M_max),
                                        temp_mean=mean(T2M_mean),
                                        temp_max=max(T2M_max),
                                        temp_mean2=mean(T2M_mean2),
                                        temp_max2=max(T2M_max2),
                                        temp_mean3=mean(T2M_mean3),
                                        temp_max3=max(T2M_max3),
                                        temp_mean4=mean(T2M_mean4),
                                        temp_max4=max(T2M_max4),
                                        precip_mean=mean(TQL_mean),
                                        water_vapor_mean=mean(TQV_mean),
                                        wind_speed_mean_pp=mean(wind_speed_2M_mean[date==wf_peak_personnel_date]),
                                        wind_speed_max_pp=max(wind_speed_2M_max[date==wf_peak_personnel_date]),
                                        temp_mean_pp=mean(T2M_mean[date==wf_peak_personnel_date]),
                                        temp_max_pp=max(T2M_max[date==wf_peak_personnel_date]),
                                        temp_mean2_pp=mean(T2M_mean2[date==wf_peak_personnel_date]),
                                        temp_max2_pp=max(T2M_max2[date==wf_peak_personnel_date]),
                                        temp_mean3_pp=mean(T2M_mean3[date==wf_peak_personnel_date]),
                                        temp_max3_pp=max(T2M_max3[date==wf_peak_personnel_date]),
                                        temp_mean4_pp=mean(T2M_mean4[date==wf_peak_personnel_date]),
                                        temp_max4_pp=max(T2M_max4[date==wf_peak_personnel_date]),
                                        precip_mean_pp=mean(TQL_mean[date==wf_peak_personnel_date]),
                                        water_vapor_mean_pp=mean(TQV_mean[date==wf_peak_personnel_date]),
                                        wind_speed_mean_pa=mean(wind_speed_2M_mean[date==wf_peak_aerial_date]),
                                        wind_speed_max_pa=max(wind_speed_2M_max[date==wf_peak_aerial_date]),
                                        temp_mean_pa=mean(T2M_mean[date==wf_peak_aerial_date]),
                                        temp_max_pa=max(T2M_max[date==wf_peak_aerial_date]),
                                        temp_mean2_pa=mean(T2M_mean2[date==wf_peak_aerial_date]),
                                        temp_max2_pa=max(T2M_max2[date==wf_peak_aerial_date]),
                                        temp_mean3_pa=mean(T2M_mean3[date==wf_peak_aerial_date]),
                                        temp_max3_pa=max(T2M_max3[date==wf_peak_aerial_date]),
                                        temp_mean4_pa=mean(T2M_mean4[date==wf_peak_aerial_date]),
                                        temp_max4_pa=max(T2M_max4[date==wf_peak_aerial_date]),
                                        precip_mean_pa=mean(TQL_mean[date==wf_peak_aerial_date]),
                                        water_vapor_mean_pa=mean(TQV_mean[date==wf_peak_aerial_date]),
                                        wind_speed_mean_pre_pp=mean(wind_speed_2M_mean[date<wf_peak_personnel_date]),
                                        wind_speed_max_pre_pp=max(wind_speed_2M_max[date<wf_peak_personnel_date]),
                                        temp_mean_pre_pp=mean(T2M_mean[date<wf_peak_personnel_date]),
                                        temp_max_pre_pp=max(T2M_max[date<wf_peak_personnel_date]),
                                        temp_mean2_pre_pp=mean(T2M_mean2[date<wf_peak_personnel_date]),
                                        temp_max2_pre_pp=max(T2M_max2[date<wf_peak_personnel_date]),
                                        temp_mean3_pre_pp=mean(T2M_mean3[date<wf_peak_personnel_date]),
                                        temp_max3_pre_pp=max(T2M_max3[date<wf_peak_personnel_date]),
                                        temp_mean4_pre_pp=mean(T2M_mean4[date<wf_peak_personnel_date]),
                                        temp_max4_pre_pp=max(T2M_max4[date<wf_peak_personnel_date]),
                                        precip_mean_pre_pp=mean(TQL_mean[date<wf_peak_personnel_date]),
                                        water_vapor_mean_pre_pp=mean(TQV_mean[date<wf_peak_personnel_date]),
                                        wind_speed_mean_pre_pa=mean(wind_speed_2M_mean[date<wf_peak_aerial_date]),
                                        wind_speed_max_pre_pa=max(wind_speed_2M_max[date<wf_peak_aerial_date]),
                                        temp_mean_pre_pa=mean(T2M_mean[date<wf_peak_aerial_date]),
                                        temp_max_pre_pa=max(T2M_max[date<wf_peak_aerial_date]),
                                        temp_mean2_pre_pa=mean(T2M_mean2[date<wf_peak_aerial_date]),
                                        temp_max2_pre_pa=max(T2M_max2[date<wf_peak_aerial_date]),
                                        temp_mean3_pre_pa=mean(T2M_mean3[date<wf_peak_aerial_date]),
                                        temp_max3_pre_pa=max(T2M_max3[date<wf_peak_aerial_date]),
                                        temp_mean4_pre_pa=mean(T2M_mean4[date<wf_peak_aerial_date]),
                                        temp_max4_pre_pa=max(T2M_max4[date<wf_peak_aerial_date]),
                                        precip_mean_pre_pa=mean(TQL_mean[date<wf_peak_aerial_date]),
                                        water_vapor_mean_pre_pa=mean(TQV_mean[date<wf_peak_aerial_date]))
  

vars <- c("wind_speed_mean","wind_speed_max","temp_mean","temp_max","temp_mean2",
          "temp_max2","temp_mean3","temp_max3","temp_mean4","temp_max4","precip_mean",
          "water_vapor_mean")

for (var in vars) {
  var_pp <- paste0(var,"_pp")
  var_pre_pp <- paste0(var,"_pre_pp")
  var_pa <- paste0(var,"_pa")
  var_pre_pa <- paste0(var,"_pre_pa")
  
  allincidents[[var_pp]][is.na(allincidents$wf_peak_personnel_date)] <- NA
  allincidents[[var_pre_pp]][is.na(allincidents$wf_peak_personnel_date)] <- NA
  allincidents[[var_pa]][is.na(allincidents$wf_peak_aerial_date)] <- NA
  allincidents[[var_pre_pa]][is.na(allincidents$wf_peak_aerial_date)] <- NA
  
}

# Loop through each column and replace Inf and -Inf with NA
for (col in names(allincidents)) {
  allincidents[[col]][is.infinite(allincidents[[col]]) | is.nan(allincidents[[col]])] <- NA
}

saveRDS(allincidents,paste0(path,"/build_merra2/output/incident_weather_new.rds"))


