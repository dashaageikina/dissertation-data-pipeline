#put land covers together
import os
import pandas as pd

# Define path
path = "/home/dashaa/wildfire_politics/data/build_land_cover"

# List all .csv files in the input/built directory
built_files = [os.path.join(dp, f) for dp, dn, filenames in os.walk(os.path.join(path, "input/built")) for f in filenames if f.endswith('.csv')]

# Initialize an empty DataFrame
alldata = pd.DataFrame()

# Read and concatenate all CSV files
for file in built_files:
    df = pd.read_csv(file)
    alldata = pd.concat([alldata, df], ignore_index=True)

# Group by incident_id and summarize the data
alldata2 = alldata.groupby('incident_id').agg({
    'tree_cover_p': lambda x: x.dropna().iloc[0] if not x.dropna().empty else None,
    'shrub_cover_p': lambda x: x.dropna().iloc[0] if not x.dropna().empty else None,
    'herb_cover_p': lambda x: x.dropna().iloc[0] if not x.dropna().empty else None
}).reset_index()

# Write the summarized data to a CSV file
output_file = os.path.join(path, "output/incidents_land_cover.csv")
alldata2.to_csv(output_file, index=False)
