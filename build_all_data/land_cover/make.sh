#This directory build land cover features for the wildfire incidents
#Separately for Contiguous U.S. and Alaska
cd /main_project/build_all_data/land_cover/code
Rscript build_land_cover_conus.R
Rscript build_landcover_alaska.R
Rscript put_land_covers_together.R
