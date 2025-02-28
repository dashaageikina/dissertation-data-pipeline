# The Effects of Sociopolitical Pressure on Wildfire Suppression Efforts in the U.S.: Evidence from 18,000 incidents

Author: Daria Ageikina, PhD (UC Davis)

Supervised by: Pierre Mérel, Jamie Hansen-Lewis, Michael Springborn

[Full text here](https://arefiles.ucdavis.edu/uploads/pub/2025/01/16/jmp_daria_ageikina.pdf)

This repository presents the data pipeline for the paper.

# Abstract
Wildfires present an increasing threat to many parts of the United States due to climate change. The paper examines whether sociopolitical pressure, measured by the newspaper coverage of wildfires, affects the allocation of personnel and aerial resources during wildfire suppression. Using a rich dataset of wildfire events from the Incident Status Summary (ICS-209) reports between 1999 and 2014, the study identifies causal effects using (1) a comprehensive set of variables controlling for wildfire threat, (2) the exogenous variation in the publication of significant historical events occurring just before the discovery of the wildfires. The analysis reveals that increased newspaper coverage of wildfires leads to significantly higher personnel usage. The paper does not find a significant effect of newspaper coverage on aircraft usage.

# The structure of the repository
master.sh is the main script that puts all data together from start to finish, conducts econometric (causal inference) analysis, and provides data summary with data visualizations.

build_all_data directory contains the scripts building the data pipeline. Each subdirectory has a shell script that also documents the data and provides instructions for downloading data if needed.

data_analysis directory contains the scripts that analyze and visualize the data.
