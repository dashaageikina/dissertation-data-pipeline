#This directory build land cover features for the wildfire incidents
#Separately for Contiguous U.S. and Alaska
cd /main_project/build_all_data/land_cover/code
python3 build_land_cover_conus.py
python3 build_land_cover_alaska.py
python3 put_land_covers_together.py
