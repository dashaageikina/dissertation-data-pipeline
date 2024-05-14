#build slope features
import os
import geopandas as gpd
import rasterio
from rasterio import features
import numpy as np
import pandas as pd
from shapely.geometry import Point, Polygon

# Paths
path = "/home/dashaa/wildfire_politics/data/build_terrain"
input_path = os.path.join(path, "input/raw/copernicus")
output_path = os.path.join(path, "output")

# Read slope files
slope_files = [os.path.join(root, file)
               for root, _, files in os.walk(input_path)
               for file in files if file.endswith("slope.tif")]

# Read incidents data
incidents = gpd.read_file("/home/dashaa/wildfire_politics/data/build_wildfire_reports/output/incidents_spatial.gpkg")
incidents = incidents[['incident_id', 'final_acres']]
incidents = incidents.to_crs(epsg=32612)
incidents['final_sq_m'] = incidents['final_acres'] * 4046.8564224
incidents['radius'] = np.sqrt(incidents['final_sq_m'] / np.pi)
incidents['geometry'] = incidents.buffer(incidents['radius'])
incidents = incidents.to_crs(crs=rasterio.open(slope_files[0]).crs)

# Initialize slope columns
incidents['mean_slope'] = 0
incidents['max_slope'] = np.nan
incidents['min_slope'] = np.nan
incidents['total_cells'] = 0

# Extract slope data
for slope_file in slope_files:
    with rasterio.open(slope_file) as src:
        for idx, incident in incidents.iterrows():
            geometry = [incident['geometry']]
            masked, _ = rasterio.mask.mask(src, geometry, crop=True, nodata=np.nan)
            masked = masked[0]

            if masked.size > 0:
                mean_val = np.nanmean(masked)
                max_val = np.nanmax(masked)
                min_val = np.nanmin(masked)
                num_cells = np.sum(~np.isnan(masked))

                if not np.isnan(mean_val):
                    incidents.at[idx, 'mean_slope'] += mean_val * num_cells
                    incidents.at[idx, 'total_cells'] += num_cells
                if not np.isnan(max_val):
                    incidents.at[idx, 'max_slope'] = max(max_val, incidents.at[idx, 'max_slope'])
                if not np.isnan(min_val):
                    incidents.at[idx, 'min_slope'] = min(min_val, incidents.at[idx, 'min_slope'])

# Finalize mean slope calculation
incidents['mean_slope'] /= incidents['total_cells']

# Save to CSV
incidents = incidents[['incident_id', 'mean_slope', 'max_slope', 'min_slope', 'total_cells', 'final_acres']]
incidents.to_csv(os.path.join(output_path, "incidents_slope.csv"), index=False)

