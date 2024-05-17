#This directory cleans geospatial data with wildfire jurisdictions and assigns them to wildfire incidents
#Upload the data on jurisdictional units from WFDSS (to input/raw) here 
first:
#https://nifc.maps.arcgis.com/home/item.html?id=9dfd2f68922d428eb4a9f61a1eba4d93&view=list&sortOrder=desc&sortField=defaultFSOrder

cd /sociopolitical-pressure-wildfires/build_all_data/wf_jurisdictions/code
python3 build_wf_jurisdictions.py
