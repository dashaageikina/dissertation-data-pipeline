#This directory cleans and transforms the main dataset in the project - 
the dataset on the wildfire reports
#The main input data comes from St Denis et al (2020)
#It is uploaded from a url in ics209_cleaning.R
#Upload these datasets first before running the code (to their respective directories)
#1.census_tracts - GIS data with U.S. census tracts:
#import the data from https://data2.nhgis.org
#Specifically, GIS files for Census Tracts for 1990, 2000, 2010, 2020
#2.zipcodes - GIS data with U.S. zipcodes:
#import KML file from https://hub.arcgis.com/datasets/d6f7ee6129e241cc9b6f75978e47128b/explore?showTable=true

cd /main_project/build_all_data/wildfire_reports/code
Rscript sitreps_cleaning.R
Rscript ics209plus_prep.R
Rscript merge_ics_and_sitreps.R

#References:
St. Denis, Lise A., Nathan P. Mietkiewicz, Karen C. Short, Mollie 
Buckland, and Jennifer K. Balch. “All-Hazards Dataset Mined from the US 
National Incident Management System 1999–2014.” Scientific Data 7, no. 1 
(December 2020): 64. https://doi.org/10.1038/s41597-020-0403-0.


