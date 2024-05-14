#This directory cleans geospatial data with wildfire jurisdictions and assigns them to wildfire incidents

cd /main_project/build_all_data/wf_jurisdictions/code
python3 make_wf_jurisdictions_valid.py
Rscript build_wildfire_jurisdictions.R
