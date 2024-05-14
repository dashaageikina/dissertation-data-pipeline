#This directory builds and cleans data on newspaper articles to merge it with data on wildfire incidents
#The main dataset comes from the NewsBank and cannot be shared
cd /main_project/build_all_data/newspaper_coverage/code
Rscript find_incident_cities.R
python3 clean_news_by_locations.py
python3 openai_api.py
