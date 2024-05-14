#This directory builds weather features using NASA's geospatial MERRA-2 data with high temporal resolution
cd /main_project/build_all_data/merra2/code
Rscript weather_at_POO3.R
Rscript merra2toincidents.R
