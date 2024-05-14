# #merging data
if (!require(sf)) install.packages('sf')
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(ggplot2)) install.packages('ggplot2')
if (!require(ggspatial)) install.packages('ggspatial')
if (!require(rnaturalearth)) install.packages('rnaturalearth')
if (!require(patchwork)) install.packages('patchwork')
library(sf)
library(tidyverse)
library(ggplot2)
library(ggspatial)
library(rnaturalearth)
library(patchwork)

rm(list=ls())
path <- "/home/dashaa/wildfire_politics/data"
incidents <- st_read(paste0(path,"/build_wildfire_reports/output/incidents_spatial.gpkg"))
incidents <- st_transform(incidents,4269)
incidents <- filter(incidents,!is.na(gacc_label))
alldata <- read.csv(paste0(path,"/merge_datasets/output/merged_incidents_only.csv"))
alldata$gacc_label[alldata$gacc_label=="North Ops"] <- "North CA"
alldata$gacc_label[alldata$gacc_label=="South Ops"] <- "South CA"
big_news_days <- readRDS(paste0(path,"/build_big_news/output/big_news_days.rds"))
big_news_days$day <- as.Date(big_news_days$day,format="%Y-%m-%d")

setwd(paste0(path,"/data_analysis/output/data_summary/graphs"))

###a map of fires in the U.S.
us <- ne_states(
  country = "United States of America",
  returnclass = "sf"
)
us <- filter(us,name!="Hawaii")
us <- st_transform(us,4269)

gacc_centroids <- incidents %>%
  group_by(gacc_label) %>%
  summarise(avg_x = mean(st_coordinates(geom)[,1]),
            avg_y = mean(st_coordinates(geom)[,2]))

gacc_centroids$avg_x[gacc_centroids$gacc_label=="Rocky Mountain"] <- gacc_centroids$avg_x[gacc_centroids$gacc_label=="Rocky Mountain"] + 1
gacc_centroids$gacc_label[gacc_centroids$gacc_label=="North Ops"] <- "North CA"
gacc_centroids$gacc_label[gacc_centroids$gacc_label=="South Ops"] <- "South CA"

incidents_map <- ggplot() +
  geom_sf(data = us) +
  geom_sf(data = incidents, aes(size = final_acres, color = gacc_label), alpha = 0.2) +
  scale_size_continuous(name = "Acres burnt") +
  scale_color_discrete(name = NULL) +
  guides(color = "none") +
  theme_minimal() + 
  theme(panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.background = element_blank(),
        plot.background = element_rect(fill = "white", color="white"),
        axis.text = element_text(size = 12),
        axis.title = element_blank()) +
  xlim(c(-180,-60)) +
  geom_text(data = gacc_centroids, aes(label = gacc_label, x = avg_x, y = avg_y), size = 3, nudge_y = 0.1)

ggsave("incidents_map.png", incidents_map, width = 12, height = 8, units = "in")


###trends of suppression resources through time by GACC
alldata <- filter(alldata,!is.na(gacc_label))
alldata$start_year <- factor(alldata$start_year)

personnel <- ggplot(alldata, aes(x = start_year, y = total_personnel_sum, color = gacc_label, group = gacc_label)) +
  stat_summary(fun = "mean", geom = "line", linewidth = 1, na.rm = TRUE) +
  #stat_summary(fun = "mean", geom = "point", size = 3, na.rm=TRUE) +
  labs(x = "Year", y = "Average Total Staff", color="GACC") +
  theme_bw() +
  theme(panel.grid = element_blank())

ggsave("personnel.png", personnel, width = 12, height = 8, units = "in")

aerial <- ggplot(alldata, aes(x = start_year, y = total_aerial_sum, color = gacc_label, group = gacc_label)) +
  stat_summary(fun = "mean", geom = "line", linewidth = 1, na.rm = TRUE) +
  #stat_summary(fun = "mean", geom = "point", size = 3, na.rm=TRUE) +
  labs(x = "Year", y = "Average Total Aerial", color="GACC") +
  theme_bw() +
  theme(panel.grid = element_blank())

ggsave("aerial.png", aerial, width = 12, height = 8, units = "in")

#weather by GACC
temperature <- ggplot(data = alldata, aes(x = gacc_label, y = temp_mean, fill = gacc_label)) +
  geom_boxplot() +
  labs(title = NULL,
       x = NULL,
       y = "Temperature (◦C)") +
  #scale_x_continuous(breaks = unique(alldata$gacc_label)) +  # Set x-axis breaks for every unique year
  theme_minimal() +
  theme(panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.background = element_blank(),
        plot.background = element_rect(fill = "white", color="white"),
        axis.text.x = element_blank(),  # Rotate x-axis labels and set size
        axis.text.y = element_text(size = 12),  # Set y-axis labels size
        #axis.title.x = element_text(size = 14),  # Set x-axis title size
        axis.title.y = element_text(size = 14)) +
  guides(fill=FALSE)

precipitation_plot <- ggplot(data = alldata, aes(x = gacc_label, y = precip_mean, fill = gacc_label)) +
  geom_boxplot() +
  labs(title = NULL,
       x = NULL,
       y = "Precipitation (kg/m2)") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "white", color="white"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14)) +
  guides(fill = FALSE)

water_vapor_plot <- ggplot(data = alldata, aes(x = gacc_label, y = water_vapor_mean, fill = gacc_label)) +
  geom_boxplot() +
  labs(title = NULL,
       x = NULL,
       y = "Water Vapor (kg/m2)") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "white", color="white"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14)) +
  guides(fill = FALSE)

wind_speed_plot <- ggplot(data = alldata, aes(x = gacc_label, y = wind_speed_mean, fill = gacc_label)) +
  geom_boxplot() +
  labs(title = NULL,
       x = NULL,
       y = "Wind Speed (m/s)") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "white", color="white"),
        axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14)) +
  guides(fill = FALSE)

weather_plots <- temperature / precipitation_plot / water_vapor_plot / wind_speed_plot
ggsave("weather.png", weather_plots, width = 12, height = 16, units = "in")

#vegetation
tree_cover <- ggplot(alldata, aes(x = start_year, y = tree_cover_p, color = gacc_label, group = gacc_label)) +
  stat_summary(fun = "mean", geom = "line", linewidth = 1, na.rm = TRUE) +
  #stat_summary(fun = "mean", geom = "point", size = 3, na.rm=TRUE) +
  labs(x = "Year", y = "Average Tree Cover %", color="GACC") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "white", color="white"),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14)) +
  guides(color = FALSE)
shrub_cover <- ggplot(alldata, aes(x = start_year, y = shrub_cover_p, color = gacc_label, group = gacc_label)) +
  stat_summary(fun = "mean", geom = "line", linewidth = 1, na.rm = TRUE) +
  #stat_summary(fun = "mean", geom = "point", size = 3, na.rm=TRUE) +
  labs(x = "Year", y = "Average Shrub Cover %", color="GACC") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "white", color="white"),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14)) 
herb_cover <- ggplot(alldata, aes(x = start_year, y = herb_cover_p, color = gacc_label, group = gacc_label)) +
  stat_summary(fun = "mean", geom = "line", linewidth = 1, na.rm = TRUE) +
  #stat_summary(fun = "mean", geom = "point", size = 3, na.rm=TRUE) +
  labs(x = "Year", y = "Average Herbaceous Cover %", color="GACC") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "white", color="white"),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14)) +
  guides(color = FALSE)
land_cover_plots <- tree_cover / shrub_cover / herb_cover
ggsave("land_cover.png", land_cover_plots, width = 12, height = 16, units = "in")

#elevation and slope
elevation <- ggplot(data = alldata, aes(x = gacc_label, y = mean_elevation, fill = gacc_label)) +
  geom_boxplot() +
  labs(title = NULL,
       x = NULL,
       y = "Elevation (m)") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.background = element_blank(),
        plot.background = element_rect(fill = "white", color="white"),
        axis.text.x = element_blank(),  # Rotate x-axis labels and set size
        axis.text.y = element_text(size = 12),  # Set y-axis labels size
        #axis.title.x = element_text(size = 14),  # Set x-axis title size
        axis.title.y = element_text(size = 14)) +
  guides(fill=FALSE)

slope <- ggplot(data = alldata, aes(x = gacc_label, y = mean_slope, fill = gacc_label)) +
  geom_boxplot() +
  labs(title = NULL,
       x = NULL,
       y = "Slope (degrees)") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "white", color="white"),
        axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14)) +
  guides(fill = FALSE)

terrain <- elevation / slope
ggsave("terrain.png", terrain, width = 12, height = 8, units = "in")

#distances to cities
#create a variable for landowners
alldata <- alldata %>%
  mutate(landowner=ifelse(private == 1, "Private",
                          ifelse(federal == 1, "Federal",
                                 ifelse(state == 1, "State", NA))))
alldata_filtered <- alldata[!is.na(alldata$landowner), ]

cities <- ggplot(alldata_filtered, aes(x = start_year, y = dist_to_any, color = landowner, group = landowner)) +
  stat_summary(fun = "mean", geom = "line", linewidth = 1, na.rm = TRUE) +
  #stat_summary(fun = "mean", geom = "point", size = 3, na.rm=TRUE) +
  labs(x = "Year", y = "Average Distance to cities (miles)", color="Land") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "white", color="white"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14))

ggsave("cities.png", cities, width = 12, height = 8, units = "in")


#newspaper coverage
news <- ggplot(alldata, aes(x = start_year, y = N_mean_daily_articles)) +
  stat_summary(fun = "mean", geom = "line", linewidth = 1, na.rm = TRUE) +
  stat_summary(fun = "mean", geom = "point", size = 3, na.rm=TRUE) +
  labs(x = "Year", y = "Average of Mean Daily Articles") +
  theme_bw() +
  theme(panel.grid = element_blank())

ggsave("news.png", news, width = 12, height = 8, units = "in")


# 
# ###make illustrations for data on wildfire resources
# #number of fires
# n_fires_hist <- ggplot(data = alldata, aes(x = start_year)) +
#   geom_histogram(binwidth = 1, fill = "#FF6600", color = "black", alpha = 0.7) +
#   labs(title = NULL,
#        x = NULL,
#        y = "#Fires") +
#   scale_x_continuous(breaks = unique(alldata$start_year)) +  # Set x-axis breaks for every unique year
#   theme_minimal() +
#   theme(panel.grid.major = element_blank(),  # Remove major grid lines
#         panel.grid.minor = element_blank(),  # Remove minor grid lines
#         panel.background = element_blank(),
#         plot.background = element_rect(fill = "white", color="white"),
#         axis.text.x = element_text(size = 12, angle = 45, hjust = 1),  # Rotate x-axis labels and set size
#         axis.text.y = element_text(size = 12),  # Set y-axis labels size
#         #axis.title.x = element_text(size = 14),  # Set x-axis title size
#         axis.title.y = element_text(size = 14))
# 
# ggsave("histogram_n_fires.png", n_fires_hist, width = 12, height = 8, units = "in")
# 
# #total acres burnt each year
# total_acres_by_year <- aggregate(final_acres ~ start_year, data = alldata, sum)
# 
# acres_hist <- ggplot(data = total_acres_by_year, aes(x = start_year, y = final_acres)) +
#   geom_bar(stat="identity", fill = "black", color = "black", alpha = 0.7, position = "identity", width=1) +
#   labs(title = NULL,
#        x = NULL,
#        y = "Total Acres Burned") +
#   scale_x_continuous(breaks = unique(alldata$start_year)) +  # Set x-axis breaks for every unique year
#   theme_minimal() +
#   theme(panel.grid.major = element_blank(),  # Remove major grid lines
#         panel.grid.minor = element_blank(),  # Remove minor grid lines
#         panel.background = element_blank(),
#         plot.background = element_rect(fill = "white", color="white"),
#         axis.text.x = element_text(size = 12, angle = 45, hjust = 1),  # Rotate x-axis labels and set size
#         axis.text.y = element_text(size = 12),  # Set y-axis labels size
#         #axis.title.x = element_text(size = 14),  # Set x-axis title size
#         axis.title.y = element_text(size = 14))
# 
# ggsave("histogram_acres.png", acres_hist, width = 12, height = 8, units = "in")
# 
# #average personnel
# mean_personnel_by_year <- aggregate(total_personnel_sum ~ start_year, data = alldata, mean)
# 
# personnel_hist <- ggplot(data = mean_personnel_by_year, aes(x = start_year, y = total_personnel_sum)) +
#   geom_bar(stat="identity", fill = "darkgreen", color = "black", alpha = 0.7, position = "identity", width=1) +
#   labs(title = NULL,
#        x = NULL,
#        y = "Average person-hours in suppression") +
#   scale_x_continuous(breaks = unique(alldata$start_year)) +  # Set x-axis breaks for every unique year
#   theme_minimal() +
#   theme(panel.grid.major = element_blank(),  # Remove major grid lines
#         panel.grid.minor = element_blank(),  # Remove minor grid lines
#         panel.background = element_blank(),
#         plot.background = element_rect(fill = "white", color="white"),
#         axis.text.x = element_text(size = 12, angle = 45, hjust = 1),  # Rotate x-axis labels and set size
#         axis.text.y = element_text(size = 12),  # Set y-axis labels size
#         #axis.title.x = element_text(size = 14),  # Set x-axis title size
#         axis.title.y = element_text(size = 14))
# 
# ggsave("histogram_personnel.png", personnel_hist, width = 12, height = 8, units = "in")
# 
# #average aerial
# mean_aerial_by_year <- aggregate(total_aerial_sum ~ start_year, data = alldata, mean)
# 
# aerial_hist <- ggplot(data = mean_aerial_by_year, aes(x = start_year, y = total_aerial_sum)) +
#   geom_bar(stat="identity", fill = "#F5F5F5", color = "black", alpha = 0.7, position = "identity", width=1) +
#   labs(title = NULL,
#        x = NULL,
#        y = "Average airplane-hours in suppression") +
#   scale_x_continuous(breaks = unique(alldata$start_year)) +  # Set x-axis breaks for every unique year
#   theme_minimal() +
#   theme(panel.grid.major = element_blank(),  # Remove major grid lines
#         panel.grid.minor = element_blank(),  # Remove minor grid lines
#         panel.background = element_blank(),
#         plot.background = element_rect(fill = "white", color="white"),
#         axis.text.x = element_text(size = 12, angle = 45, hjust = 1),  # Rotate x-axis labels and set size
#         axis.text.y = element_text(size = 12),  # Set y-axis labels size
#         #axis.title.x = element_text(size = 14),  # Set x-axis title size
#         axis.title.y = element_text(size = 14))
# 
# ggsave("histogram_aerial.png", aerial_hist, width = 12, height = 8, units = "in")
# 
# 
# ###Big news data
# #days with big news
# days_with_articles <- big_news_days %>%
#   filter(all_articles!="NULL") %>%
#   mutate(year = lubridate::year(day)) %>%
#   group_by(year) %>%
#   summarise(num_days_with_articles = n())
# 
# bigarticles_hist <- ggplot(data = days_with_articles, aes(x = year, y = num_days_with_articles)) +
#   geom_bar(stat="identity", fill = "#F5F5F5", color = "black", alpha = 0.7, position = "identity", width=1) +
#   labs(title = NULL,
#        x = NULL,
#        y = "#days with important NYT articles") +
#   scale_x_continuous(breaks = unique(days_with_articles$year)) +  # Set x-axis breaks for every unique year
#   theme_minimal() +
#   theme(panel.grid.major = element_blank(),  # Remove major grid lines
#         panel.grid.minor = element_blank(),  # Remove minor grid lines
#         panel.background = element_blank(),
#         plot.background = element_rect(fill = "white", color="white"),
#         axis.text.x = element_text(size = 12, angle = 45, hjust = 1),  # Rotate x-axis labels and set size
#         axis.text.y = element_text(size = 12),  # Set y-axis labels size
#         #axis.title.x = element_text(size = 14),  # Set x-axis title size
#         axis.title.y = element_text(size = 14))
# 
# ggsave("histogram_bigarticles.png", bigarticles_hist, width = 12, height = 8, units = "in")
# 
# #sameday news
# days_with_sameday_articles <- big_news_days %>%
#   filter(sameday_articles!="NULL") %>%
#   mutate(year = lubridate::year(day)) %>%
#   group_by(year) %>%
#   summarise(num_days_with_sameday_articles = n())
# 
# bigarticles_hist <- ggplot(data = days_with_articles, aes(x = year, y = num_days_with_articles)) +
#   geom_bar(stat="identity", fill = "#F5F5F5", color = "black", alpha = 0.7, position = "identity", width=1) +
#   labs(title = NULL,
#        x = NULL,
#        y = "#days with important NYT articles") +
#   scale_x_continuous(breaks = unique(days_with_articles$year)) +  # Set x-axis breaks for every unique year
#   theme_minimal() +
#   theme(panel.grid.major = element_blank(),  # Remove major grid lines
#         panel.grid.minor = element_blank(),  # Remove minor grid lines
#         panel.background = element_blank(),
#         plot.background = element_rect(fill = "white", color="white"),
#         axis.text.x = element_text(size = 12, angle = 45, hjust = 1),  # Rotate x-axis labels and set size
#         axis.text.y = element_text(size = 12),  # Set y-axis labels size
#         #axis.title.x = element_text(size = 14),  # Set x-axis title size
#         axis.title.y = element_text(size = 14))
# 
# ggsave("histogram_bigarticles.png", bigarticles_hist, width = 12, height = 8, units = "in")
# 
# 
# 


