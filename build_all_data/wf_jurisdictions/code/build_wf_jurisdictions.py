#find which jurisdiction each fire belongs to
import os
import geopandas as gpd
from shapely.geometry import shape
import pandas as pd

path = "sociopolitical-pressure-wildfires/build_all_data"

agencies = gpd.read_file(path+'/wf_jurisdictions/input/raw/WFDSS_Jurisdictional_Agency.gdb')

###first make agencies data valid (some flaws in data - invalid multipolygons)
# Create a new column to mark invalid geometries
agencies['not_valid'] = 0

# Iterate through each row and check validity
for index, row in agencies.iterrows():
    try:
        # Attempt to make the geometry valid
        geom = row['geometry'].buffer(0)  # This operation often fixes invalid geometries
        row['geometry'] = geom
    except:
        # If an error occurs, mark the geometry as invalid
        agencies.at[index, 'not_valid'] = 1

# Filter out invalid geometries
agencies = agencies[agencies['not_valid'] == 0]
agencies = agencies.to_crs(epsg=2163)

###load point incident data and find landowner category at the point of origin
incidents = gpd.read_file(os.path.join(path, "/wildfire_reports/output/incidents_spatial.gpkg"))
incidents = incidents.to_crs(epsg=2163)
incidents_with_agencies = gpd.sjoin(incidents, agencies[['geometry', 'LandownerCategory']], how="left", op="within")
incidents_with_agencies = incidents_with_agencies.rename(columns={'LandownerCategory': 'POO_incident'})

# Drop unnecessary columns and save the data
incidents_with_agencies = incidents_with_agencies[['incident_id','POO_landowner']]
incidents_with_agencies.to_csv(f'{path}/wf_jurisdictions/output/incidents_POO_landowners.csv', index=False)

