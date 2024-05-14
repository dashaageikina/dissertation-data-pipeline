#building land cover variables for contiguous U.S.
import os
import geopandas as gpd
import rasterio
import numpy as np
import pandas as pd
from shapely.geometry import Point, Polygon
from rasterio.mask import mask
from pyproj import Transformer

# Define paths
path = "/home/dashaa/wildfire_politics/data/build_land_cover"
input_path = os.path.join(path, "input/raw/above/Annual_Landcover_ABoVE_1691/data")
output_path = os.path.join(path, "output")

# Load data on incidents
incidents = gpd.read_file("/home/dashaa/wildfire_politics/data/build_wildfire_reports/output/incidents_spatial.gpkg")

# Filter for 1999 contiguous US incidents in Alaska
incidents = incidents[incidents['POO_state'] == "AK"]

# Subset and transform
incidents = incidents[['incident_id', 'start_year', 'final_acres']]
incidents = incidents.to_crs(epsg=32612)  # UTM zone 12N
incidents['final_sq_m'] = incidents['final_acres'] * 4046.8564224
incidents['radius'] = np.sqrt(incidents['final_sq_m'] / np.pi)
incidents['geometry'] = incidents.geometry.buffer(incidents['radius'])

# Transform to Albers Equal Area
incidents = incidents.to_crs("+proj=aea +lat_0=40 +lon_0=-96 +lat_1=50 +lat_2=70 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

# Initialize cover percentage columns
incidents['tree_cover_p'] = 0
incidents['shrub_cover_p'] = 0
incidents['herb_cover_p'] = 0

# List .tif files
files = [os.path.join(input_path, f) for f in os.listdir(input_path) if f.endswith('.tif')]

def count_cells_equal_to_value(raster, geom, values):
    with rasterio.open(raster) as src:
        out_image, out_transform = mask(src, [geom], crop=True)
        out_image = out_image[0]  # Take the first band
        cell_count = np.sum(np.isin(out_image, values))
        total_cells = out_image.size
        value_proportion = cell_count / total_cells
    return value_proportion

# Loop through years
new_incidents = None

for year in range(1998, 2013 + 1):
    rasts_year = [(raster, year - 1983) for raster in files]
    incidents_year = incidents[incidents['start_year'] == year + 1].copy()
    
    for raster, layer in rasts_year:
        tree_values = np.array([count_cells_equal_to_value(raster, geom, [1, 2]) for geom in incidents_year.geometry])
        shrub_values = np.array([count_cells_equal_to_value(raster, geom, [3]) for geom in incidents_year.geometry])
        herb_values = np.array([count_cells_equal_to_value(raster, geom, [4]) for geom in incidents_year.geometry])
        
        incidents_year['tree_cover_p'] += tree_values
        incidents_year['shrub_cover_p'] += shrub_values
        incidents_year['herb_cover_p'] += herb_values
    
    if new_incidents is None:
        new_incidents = incidents_year
    else:
        new_incidents = pd.concat([new_incidents, incidents_year], ignore_index=True)

# Convert cover percentages to percentages
new_incidents['tree_cover_p'] *= 100
new_incidents['shrub_cover_p'] *= 100
new_incidents['herb_cover_p'] *= 100

# Drop geometry and select necessary columns
new_incidents = new_incidents.drop(columns='geometry')
new_incidents = new_incidents[['incident_id', 'tree_cover_p', 'shrub_cover_p', 'herb_cover_p']]

# Save to CSV
new_incidents.to_csv(os.path.join(path, "input/built/alaska/land_cover.csv"), index=False)

