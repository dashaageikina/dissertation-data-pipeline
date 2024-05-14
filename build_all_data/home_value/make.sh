#This directory cleans housing data for merging it with incident data
cd /main_project/build_all_data/home_value/code
Rscript build_price_indices.R
Rscript build_housing_value_place.R
