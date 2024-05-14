#build distance from each incident to nearby cities
import geopandas as gpd
from shapely.geometry import Point
import pandas as pd
import os

path = "/main_project/build_all_data"
output_path = os.path.join(path, "cities", "output")
input_path = os.path.join(path, "cities", "input", "raw", "nhgis0003_shape")

# Load incidents data
incidents = gpd.read_file(os.path.join(path, "wildfire_reports", "output", "incidents_spatial.gpkg"))
incidents = incidents[['incident_id', 'start_year', 'geometry']]
incidents = incidents.to_crs('EPSG:9712')

# Load population data
population = pd.read_csv(os.path.join(path, "population", "input", "raw", "nhgis0003_csv", "nhgis0003_ts_geog2010_place.csv"), encoding='latin1')
population = population[['GISJOIN', 'DATAYEAR', 'CL8AA']]

years = ["1990", "2000", "2010"]

pop_cutoffs = {
    0: "any",
    100: "100",
    250: "250",
    500: "500",
    1000: "1k",
    5000: "5k",
    10000: "10k",
    25000: "25k",
    50000: "50k",
    100000: "100k",
    250000: "250k",
    500000: "500k"
}

for pop_cutoff, label in pop_cutoffs.items():
    incidents[f'dist_to_{label}'] = None

for year in years:
    sub_incidents = incidents[(incidents['start_year'] > int(year)) & (incidents['start_year'] <= int(year) + 10)]
    
    cities_year = gpd.read_file(os.path.join(input_path, f"US_place_{year}.shp"))
    cities_year['DATAYEAR'] = int(year)
    cities_year = cities_year.merge(population, how='left', on=['GISJOIN', 'DATAYEAR'])
    cities_year = cities_year.to_crs('EPSG:9712')
    cities_year['geometry'] = cities_year['geometry'].buffer(0)
    cities_year = gpd.GeoDataFrame(cities_year, geometry='geometry')
    
    for pop_cutoff, label in pop_cutoffs.items():
        print(f"{year}, distance to {label} is being computed")
        sub_incidents.loc[:,f'dist_to_{label}'] = sub_incidents.apply(
            lambda x: min(cities_year[cities_year['CL8AA'] >= pop_cutoff].distance(x.geometry)),
            axis=1
        )
        #convert to miles
        sub_incidents.loc[:,f'dist_to_{label}'] = sub_incidents.loc[:,f'dist_to_{label}']/1609.34
    
    sub_incidents = sub_incidents.drop(columns='start_year')
    sub_incidents.to_csv(os.path.join(output_path, f'incidents_to_cities{year}.csv'), index=False)
    
#append all files together
dfs = []

for year in years:
    sub_incidents = pd.read_csv(os.path.join(output_path, f'incidents_to_cities{year}.csv'))
    
    dfs.append(sub_incidents)
    
allyears = pd.concat(dfs, ignore_index=True)

allyears.to_csv(os.path.join(output_path, 'incidents_to_cities.csv'), index=False)   
    
    
    
    