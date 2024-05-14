#prep reanalysis data
if (!require(ncdf4)) install.packages('ncdf4')
if (!require(raster)) install.packages('raster')
if (!require(foreign)) install.packages('foreign')
if (!require(sf)) install.packages('sf')
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(haven)) install.packages('haven')
if (!require(rlecuyer)) install.packages("rlecuyer")
library(ncdf4)
library(raster)
library(foreign)
library(sf)
library(tidyverse)
library(haven)

rm(list=ls())
path <- "/home/dashaa/wildfire_politics/data/build_merra2"

url_base = "https://goldsmr4.gesdisc.eosdis.nasa.gov/data/MERRA2/M2T1NXSLV.5.12.4/"
credentials = "--http-user=username --http-password=password " #put your username and password here
extra_options = "--load-cookies ~/.urs_cookies --save-cookies ~/.urs_cookies --keep-session-cookies -r -c -nH -nd -np -A nc4 --content-disposition "

#upload data on incidents
incidents <- st_read(paste0("/home/dashaa/wildfire_politics/data/build_wildfire_reports/output/incidents_spatial.gpkg"))
#remove that later
incidents <- filter(incidents,wf_cessation_date<wf_peak_personnel_date | wf_cessation_date<wf_peak_aerial_date) 

incidents <- subset(incidents,select=c(incident_id,final_acres,discovery_date,
                                       wf_cessation_date,wf_peak_personnel_date,
                                       wf_peak_aerial_date))
incidents <- incidents[order(incidents$discovery_date),]
incidents <- st_transform(incidents, "+proj=utm +zone=12 +datum=WGS84")
incidents$final_sq_m <- incidents$final_acres*4046.8564224
incidents$radius <- sqrt(incidents$final_sq_m/pi)
incidents <- st_buffer(incidents,incidents$radius)
incidents <- st_transform(incidents, "+proj=longlat +datum=WGS84 +no_defs")

#checking which days had wildfires
incidents$discovery_date <- as.Date(substr(incidents$discovery_date,1,10),format="%Y-%m-%d")
incidents$wf_cessation_date <- as.Date(substr(incidents$wf_cessation_date,1,10),format="%Y-%m-%d")
incidents$wf_peak_personnel_date <- as.Date(substr(incidents$wf_peak_personnel_date,1,10),format="%Y-%m-%d")
incidents$wf_peak_aerial_date <- as.Date(substr(incidents$wf_peak_aerial_date,1,10),format="%Y-%m-%d")
dates <- seq(from=as.Date("1999-01-01",format="%Y-%m-%d"),to=as.Date("2014-12-31",format="%Y-%m-%d"),by="day")
dates <- as.data.frame(dates,col.names="date")
dates$any_incident <- 0

for (id in incidents$incident_id) {
  
  start_day_id <- incidents$discovery_date[incidents$incident_id==id]
  end_day_id <- max(incidents$wf_cessation_date[incidents$incident_id==id],
                    incidents$wf_peak_personnel_date[incidents$incident_id==id],
                    incidents$wf_peak_aerial_date[incidents$incident_id==id],
                    na.rm=TRUE)
  
  #edit that later:
  #dates$any_incident[dates$dates>=start_day_id & dates$dates<=end_day_id] <- 1
  cessation_date <- incidents$wf_cessation_date[incidents$incident_id==id]
  dates$any_incident[dates$dates>cessation_date & dates$dates<=end_day_id] <- 1
  
}

dates <- dates[dates$any_incident==1,]
dates <- dates$dates

variables <- list("U2M", "U50M","T2M","TQL","TQV")

directory <- paste0(path,"/temp/daily_files")
setwd(directory)
files <- list.files(directory,full.names=T)
unlink(files)

#get the data from start to finish
getdailymeans <- function(date,x_varlist){ #the date must follow a format yyyy-mm-dd
  
  setwd(directory)
  
  Sys.sleep(2)
  
  #index <- match(date,dates)
  #Sys.sleep(5*index%%5)
  
  year <- substr(date,1,4)
  month <- substr(date,6,7)
  day <- substr(date,9,10)
  url_base <- "https://goldsmr4.gesdisc.eosdis.nasa.gov/data/MERRA2/M2T1NXSLV.5.12.4/"
  if (year<=2000) {
    url <- paste0(url_base,year,"/",month,"/MERRA2_200.tavg1_2d_slv_Nx.",year,month,day,".nc4")
  } else if (year>=2001 & year<=2010) {
    url <- paste0(url_base,year,"/",month,"/MERRA2_300.tavg1_2d_slv_Nx.",year,month,day,".nc4")
  } else {
    url <- paste0(url_base,year,"/",month,"/MERRA2_400.tavg1_2d_slv_Nx.",year,month,day,".nc4")
  }
  
  cmd = paste0("wget ", credentials, extra_options, url)
  
  bo <- 0
  while(bo!=10){
    x = try(system(cmd))
    if (class(x)=="try-error") {
      cat("ERROR1: ", x, "\n")
      Sys.sleep(3)
      print("reconnecting...")
      bo <- bo+1
      print(bo)
    } else break 
  }
  
  if (year<=2000) {
    file <- paste0(directory,"/MERRA2_200.tavg1_2d_slv_Nx.",year,month,day,".nc4")
  } else if (year>=2001 & year<=2010) {
    file <- paste0(directory,"/MERRA2_300.tavg1_2d_slv_Nx.",year,month,day,".nc4")
  } else {
    file <- paste0(directory,"/MERRA2_400.tavg1_2d_slv_Nx.",year,month,day,".nc4")
  }
  
  #edit this later
  #check which incidents are involved to cut the file
  #zones <- filter(incidents,discovery_date<=date & max(wf_cessation_date,wf_peak_aerial_date,wf_peak_personnel_date) >= date)
  zones <- filter(incidents,(wf_peak_aerial_date >= date | wf_peak_personnel_date>=date) & wf_cessation_date<date)
  
  for (i in 1:nrow(zones)) {
    
    #extract daily means (and min and max) for each variable
    for (x_var in x_varlist) {
      
      var_daily_means <- extract_incident_day_var(file=file,incident=zones[i,],x_var=x_var)
      
      if (x_var==x_varlist[1]) {
        daily_means <- var_daily_means
      } else {
        daily_means <- merge(daily_means,var_daily_means)
      }
    }
    
    if (i==1) {
      all_daily_means <- daily_means
    } else {
      all_daily_means <- rbind(all_daily_means,daily_means)
    }
    
  }
  
  all_daily_means$date <- date
  all_daily_means$T2M_mean2 <- (all_daily_means$T2M_mean)^2
  all_daily_means$T2M_mean3 <- (all_daily_means$T2M_mean)^3
  all_daily_means$T2M_mean4 <- (all_daily_means$T2M_mean)^4
  all_daily_means$T2M_max2 <- (all_daily_means$T2M_max)^2
  all_daily_means$T2M_max3 <- (all_daily_means$T2M_max)^3
  all_daily_means$T2M_max4 <- (all_daily_means$T2M_max)^4
  #delete input file to save space
  unlink(file)
  
  return(all_daily_means)
}

extract_incident_day_var <- function(file,incident,x_var) {
  
  
  variable_brick <- get_the_brick(files=file,x_var=x_var,zones=incident)
  
  if (x_var=="U2M" | x_var=="U50M") {
    
    height <- substr(x_var,2,nchar(x_var)) #for the file and variable names
    v_var_name <- paste0("V",height) #the name of the analagous variable for V wind
    
    u_brick <- variable_brick
    v_brick <- get_the_brick(files=file,x_var=v_var_name,zones=incident)
    wind_speed_brick <- sqrt(u_brick^2+v_brick^2)
    names(wind_speed_brick) <- names(u_brick)
    wind_direction_brick <- (180 + (180/pi * atan2(v_brick,u_brick)))%%360
    # extract daily means
    daily_means_speed <- extract_means(variable_brick=wind_speed_brick,zones=incident)
    daily_means_direction <- extract_means(variable_brick=wind_direction_brick,zones=incident)
    #get daily average, and max and min
    daily_means_speed <- get_mean_max_min(daily_means=daily_means_speed,x_var=paste0("wind_speed_",height))
    daily_means_direction <- get_mean_max_min(daily_means=daily_means_direction,x_var=paste0("wind_direction_",height))
    
    daily_means <- merge(daily_means_speed,daily_means_direction)
    
    rm(daily_means_speed, daily_means_direction, variable_brick, u_brick, v_brick, wind_speed_brick, wind_direction_brick)
    return(daily_means)
    
  } else {
    daily_means <- extract_means(variable_brick=variable_brick,zones=incident)
    #get the variable name and units
    #ncfile <- nc_open(file)
    #var_name <- ncatt_get(ncfile,x_var,"long_name")[[2]]
    #var_units <- ncatt_get(ncfile,x_var,"units")[[2]]
    #get daily mean,max and min for the variable
    daily_means <- get_mean_max_min(daily_means=daily_means,x_var=x_var)
    
    rm(variable_brick)
    return(daily_means)
  }
  
}

get_the_brick <- function(files, x_var, zones) {
  #get the brick
  variable_brick <- brick(stack(files,varname=x_var))
  variable_brick <- crop(variable_brick,extent(zones))
  return(variable_brick)
}

extract_means <- function(variable_brick,zones) {
  
  #if a brick has only point, use the following command:
  #daily_means <- rasterToPoints(variable_brick)
  
  # extract daily means
  daily_means <- raster::extract(variable_brick, zones, fun = mean, na.rm = T)
  daily_means <- data.frame(incident_id=zones$incident_id, daily_means)
  
  #if a brick has only point, use the following command:
  #daily_means <- daily_means[ , -c(2,3)]
  
  return(daily_means)
}

get_mean_max_min <- function(daily_means,x_var) {
  
  daily_means$var_mean <- rowMeans(daily_means[,-1])
  daily_means$var_max <- max(daily_means[,-1])
  daily_means$var_min <- min(daily_means[,-1])
  
  daily_means <- subset(daily_means,select=c(incident_id,var_mean,var_max,var_min))
  
  #change the names of the variables
  names(daily_means)[names(daily_means)=="var_mean"] <- paste0(x_var,"_mean")
  names(daily_means)[names(daily_means)=="var_max"] <- paste0(x_var,"_max")
  names(daily_means)[names(daily_means)=="var_min"] <- paste0(x_var,"_min")
  
  return(daily_means)
}

# Run the job
#set up batches
cutoffs <- seq(from=50,to=3000,by=50)
i <- 55 #set the last number done in the built files
for (cutoff in cutoffs[(i+1):length(cutoffs)]) {
  i <- i + 1
  if (cutoff>length(dates)) {
    cutoff <- length(dates)
  }
  dates_batch <- dates[(cutoff-49):cutoff]
  
  incident_days_weather <- lapply(dates_batch, function(y) {getdailymeans(date=y, x_varlist=variables)})
  saveRDS(incident_days_weather,paste0(path,"/input/built/fixed/incident_days_weather",i,".rds"))
  if (cutoff==length(dates)) {
    break
  }
}

