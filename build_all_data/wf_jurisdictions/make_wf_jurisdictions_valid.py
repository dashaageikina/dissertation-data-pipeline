#fix the polygon data to make it valid for analysis

import geopandas as gpd
from shapely.geometry import shape

path = "/main_project/build_all_data/build_wf_jurisdictions"

agencies = gpd.read_file(path+'/input/raw/WFDSS_Jurisdictional_Agency.gdb')

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
agencies_valid = agencies[agencies['not_valid'] == 0]

# Write the valid geometries to a new file
agencies_valid.to_file(path+"/input/built/agencies_valid.gpkg", driver='GPKG')