#This directory cleans and transforms the main dataset in our project - 
the dataset on the wildfire reports
#The main input data comes from St Denis et al (2020)
#It has been uploaded from 
https://figshare.com/articles/dataset/ICS209-PLUS_Cleaned_databases/8048252/10
#The input data is stored in three directories: 
ics209_incidents_spatial, ics209-plus, ics209-plus-wf
#The auxiliary datasets to generate new features: 
#census_tracts - GIS data with U.S. census tracts
#county_boundaries - GIS data with U.S. county boundaries
#zipcodes - GIS data with U.S. zipcodes

cd Desktop/Directories/GitHub
submit sitreps_cleaning.R
submit ics209plus_prep.R
submit merge_ics_and_sitreps.R

#References:
St. Denis, Lise A., Nathan P. Mietkiewicz, Karen C. Short, Mollie 
Buckland, and Jennifer K. Balch. “All-Hazards Dataset Mined from the US 
National Incident Management System 1999–2014.” Scientific Data 7, no. 1 
(December 2020): 64. https://doi.org/10.1038/s41597-020-0403-0.


