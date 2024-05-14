#run endogenous and instrumental variable regressions
import pandas as pd
import numpy as np
import os
from statsmodels.regression.linear_model import OLS
from statsmodels.tools.tools import add_constant
from statsmodels.api import WLS
from linearmodels.iv import IV2SLS

# Setup paths
path = "/home/dashaa/wildfire_politics/data"
output_path = os.path.join(path, "data_analysis/output")
input_path = os.path.join(path, "merge_datasets/output/merged.csv")

# Load data
df = pd.read_csv(input_path)

# Label variables (comments for clarity)
df.rename(columns={
    'total_personnel_sum': 'Total staff',
    'total_aerial_sum': 'Total aerial',
    'wf_peak_personnel': 'Peak staff',
    'wf_peak_aerial': 'Peak aerial',
    'N_mean_daily_articles': 'Mean daily news',
    'N_median_daily_articles': 'Median daily news',
    'N_max_daily_articles': 'Max daily news',
    'N_mean_daily_articles_pp': 'Mean news pp',
    'N_median_daily_articles_pp': 'Median news pp',
    'N_max_daily_articles_pp': 'Max news pp',
    'N_mean_daily_articles_pa': 'Mean news pa',
    'N_median_daily_articles_pa': 'Median news pa',
    'N_max_daily_articles_pa': 'Max news pa',
    'hpi': 'Home price index',
    'pop_density2': 'Pop density',
    'state': 'State land origin',
    'private': 'Private land origin',
    'dist_to_any': 'Dist. to any town/city',
    'dist_to_1k': 'Dist. to town/city with >=1000 people',
}, inplace=True)

# Generate new variables
df['huge115'] = df['acres'] > 115000
df['other_wildfires2'] = df['other_wildfires'] / df['Ndays']
df['pop_density2_sq'] = df['Pop density'] ** 2

# Encoding categorical variables
df['landowner'] = df['POO_landowner'].astype('category').cat.codes
df['state_season'] = df['POO_state'].astype(str) + df['season'].astype(str)

# Remove outliers
df = df[df['Total staff'] <= 100000]

# Grouping variables
terrain = ['mean_elevation', 'max_elevation', 'mean_slope', 'max_slope']
vegetation = ['tree_cover_p', 'shrub_cover_p', 'herb_cover_p']
cities = ['Dist. to any town/city', 'Dist. to town/city with >=1000 people']
landowners = ['Private land origin', 'State land origin']
population = ['Pop density', 'pop_density2_sq']

# Instruments
crime_vars = [
    'total_crime_mean_US', 'violent_crime_mean_US', 'total_crime_mean_plus_US',
    'violent_crime_mean_plus_US', 'total_crime_mean_local', 'violent_crime_mean_local',
    'total_crime_mean_plus_local', 'violent_crime_mean_plus_local'
]

entropies_total = [col for col in df.columns if 'entropy_total' in col or 'entropy_prefire' in col]
entropies_pp = [col for col in df.columns if 'entropy_aerial' in col]
entropies_pa = [col for col in df.columns if 'entropy_personnel' in col]

bignews_vars = [col for col in df.columns if 'news' in col or 'sameday_articles' in col or 'all_articles' in col]
bignews_total = []
bignews_pp = []
bignews_pa = []

for var in bignews_vars:
    if '_pp' in var:
        bignews_pp.append(var)
    elif '_pa' in var:
        bignews_pa.append(var)
    else:
        bignews_total.append(var)

# Function for running endogenous regressions
def run_endogenous(outcomes, regressors, weather_controls):
    results = []
    for outcome in outcomes:
        for regressor in regressors:
            # Specification 1 - only fixed effects as controls
            model = OLS(df[outcome], add_constant(df[[regressor, 'state_season', 'start_year']])).fit(cov_type='cluster', cov_kwds={'groups': df['POO_state']})
            results.append(model.summary())
            
            # Specification 2 - control for big fires (indicator)
            model = OLS(df[outcome], add_constant(df[[regressor, 'huge115', 'state_season', 'start_year']])).fit(cov_type='cluster', cov_kwds={'groups': df['POO_state']})
            results.append(model.summary())
            
            # Specification 3 - 2 + all weather, vegetation and terrain controls
            model = OLS(df[outcome], add_constant(df[[regressor, 'huge115'] + weather_controls + vegetation + terrain + ['state_season', 'start_year']])).fit(cov_type='cluster', cov_kwds={'groups': df['POO_state']})
            results.append(model.summary())
            
            # Specification 4 - 3 + GACC and landowner controls
            model = OLS(df[outcome], add_constant(df[[regressor, 'huge115'] + weather_controls + vegetation + terrain + ['other_wildfires2'] + ['state_season', 'start_year']])).fit(cov_type='cluster', cov_kwds={'groups': df['POO_state']})
            results.append(model.summary())
            
            # Specification 5 - 4 + population and housing value (all controls)
            model = OLS(df[outcome], add_constant(df[[regressor, 'huge115'] + weather_controls + vegetation + terrain + ['other_wildfires2'] + ['state_season', 'start_year'] + cities + population])).fit(cov_type='cluster', cov_kwds={'groups': df['POO_state']})
            results.append(model.summary())
            
    return results

# Define outcomes and regressors
outcomes_alldays = ['Total staff', 'Total aerial']
outcomes_alldays_logs = ['log_total_personnel_sum', 'log_total_aerial_sum']
regressors_alldays = ['Mean daily news', 'Median daily news', 'Max daily news']
regressors_alldays_logs = ['log_N_mean_daily_articles', 'log_N_median_daily_articles', 'log_N_max_daily_articles']

weather_main = ['wind_speed_mean', 'wind_speed_max', 'temp_mean', 'temp_max', 'precip_mean', 'water_vapor_mean']
weather_polynomials = ['temp_mean2', 'temp_max2', 'temp_mean3', 'temp_max3', 'temp_mean4', 'temp_max4']

# Create log variables
for var in outcomes_alldays:
    df[f'log_{var}'] = np.log(df[var] + 1)

for var in regressors_alldays:
    df[f'log_{var}'] = np.log(df[var] + 1)

# Run endogenous regressions
results = run_endogenous(outcomes_alldays, regressors_alldays, weather_main + weather_polynomials)

# Save results to a text file
with open(os.path.join(output_path, 'OLS_results.txt'), 'w') as f:
    for result in results:
        f.write(result.as_text())
        f.write('\n\n')
