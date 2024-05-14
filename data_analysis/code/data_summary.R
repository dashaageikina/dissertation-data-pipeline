if (!require(tidyverse)) install.packages('tidyverse')
if (!require(knitr)) install.packages('knitr')
if (!require(xtable)) install.packages('xtable')
if (!require(kableExtra)) install.packages('kableExtra')
library(tidyverse)
library(xtable)
library(knitr)
library(kableExtra)

rm(list=ls())
path <- "/home/dashaa/wildfire_politics/data"
alldata <- read.csv(paste0(path,"/merge_datasets/output/merged_incidents_only.csv"))

setwd(paste0(path,"/data_analysis/output/data_summary"))

regional_averages <- alldata %>%
  group_by(gacc_label) %>%
  summarise(
    Final_Acres = mean(final_acres, na.rm = TRUE),
    Total_Personnel = mean(total_personnel_sum, na.rm = TRUE),
    Total_Aerial = mean(total_aerial_sum, na.rm = TRUE),
    Peak_Personnel = mean(wf_peak_personnel, na.rm = TRUE),
    Peak_Aerial = mean(wf_peak_aerial, na.rm = TRUE),
    All_Articles = mean(N_all_articles, na.rm = TRUE),
    Mean_Daily_Articles = mean(N_mean_daily_articles, na.rm = TRUE),
    Median_Daily_Articles = mean(N_median_daily_articles, na.rm = TRUE),
    Max_Daily_Articles = mean(N_max_daily_articles, na.rm = TRUE),
    Mean_Daily_Articles_pre_PP = mean(N_mean_daily_articles_pp, na.rm = TRUE),
    Median_Daily_Articles_pre_PP = mean(N_median_daily_articles_pp, na.rm = TRUE),
    Max_Daily_Articles_pre_PP = mean(N_max_daily_articles_pp, na.rm = TRUE),
    #IVs will be here
    Temp_Mean = mean(temp_mean, na.rm = TRUE),
    Temp_Max = mean(temp_max, na.rm = TRUE),
    Wind_Speed_Mean = mean(wind_speed_mean, na.rm = TRUE),
    Wind_Speed_Max = mean(wind_speed_max, na.rm = TRUE),
    Precipitation = mean(precip_mean, na.rm = TRUE),
    Water_Vapor_Mean = mean(water_vapor_mean, na.rm = TRUE),
  )

# Calculate overall averages
overall_averages <- alldata %>%
  summarise(
    gacc_label = "Overall",
    Final_Acres = mean(final_acres, na.rm = TRUE),
    Total_Personnel = mean(total_personnel_sum, na.rm = TRUE),
    Total_Aerial = mean(total_aerial_sum, na.rm = TRUE),
    # Peak_Personnel = mean(wf_peak_personnel, na.rm = TRUE),
    # Peak_Aerial = mean(wf_peak_aerial, na.rm = TRUE),
    # All_Articles = mean(N_all_articles, na.rm = TRUE),
    # Mean_Daily_Articles = mean(N_mean_daily_articles, na.rm = TRUE),
    # Median_Daily_Articles = mean(N_median_daily_articles, na.rm = TRUE),
    # Max_Daily_Articles = mean(N_max_daily_articles, na.rm = TRUE),
    # Mean_Daily_Articles_pre_PP = mean(N_mean_daily_articles_pp, na.rm = TRUE),
    # Median_Daily_Articles_pre_PP = mean(N_median_daily_articles_pp, na.rm = TRUE),
    # Max_Daily_Articles_pre_PP = mean(N_max_daily_articles_pp, na.rm = TRUE),
    # #IVs will be here
    # Temp_Mean = mean(temp_mean, na.rm = TRUE),
    # Temp_Max = mean(temp_max, na.rm = TRUE),
    # Wind_Speed_Mean = mean(wind_speed_mean, na.rm = TRUE),
    # Wind_Speed_Max = mean(wind_speed_max, na.rm = TRUE),
    # Precipitation = mean(precip_mean, na.rm = TRUE),
    # Water_Vapor_Mean = mean(water_vapor_mean, na.rm = TRUE),
  )

# Combine regional and overall averages
averages <- bind_rows(regional_averages, overall_averages) %>%
  filter(!is.na(gacc_label)) %>%
  rownames_to_column(var = "GACC") # Add row names

# Create custom LaTeX-formatted table
latex_table <- paste0(
  "\\begin{table}[H]\n",
  "\\centering\n",
  "\\caption{Data Summary}\n",
  "\\renewcommand{\\arraystretch}{1.5}\n",
  "\\rotatebox{90}{\n",
  "\\begin{tabular}{lccccccccccc}\n",
  "\\toprule\n",
  " & Alaska & Eastern & Great  & North  & Northern  & Northwest & Rocky  & South  & Southern & Southwest & All \\\\\n",
  " &  &  &  Basin &  CA &  Rockies &  &  Mountain &  CA &  & & \\\\\n",
  "\\midrule\n"
)

averages_transposed <- t(averages)
rownames(averages_transposed) <- gsub("_"," ",rownames(averages_transposed))
averages_transposed <- averages_transposed[-c(1,2), ]
averages_transposed <- matrix(as.numeric(as.character(averages_transposed)), 
                              ncol = ncol(averages_transposed), 
                              dimnames = list(rownames(averages_transposed), colnames(averages_transposed)))

# Iterate through rows and populate LaTeX table
for (metric in rownames(averages_transposed)) {
  row_data <- c(metric, sprintf("%.2f",averages_transposed[metric, ]))
  latex_table <- paste0(latex_table, paste(row_data, collapse = " & "), " \\\\\n")
}

# Add bottom rule and end the table
latex_table <- paste0(latex_table, "\\bottomrule\n", "\\end{tabular}\n", "}\n", "\\end{table}")

# Write to LaTeX file
writeLines(latex_table, "data_summary.tex")