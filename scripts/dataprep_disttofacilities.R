library(tidyverse)
library(sf)

setwd("~/Rabies SES")

##-----------------------------------------------------------------------------
# 1. Load and prep data
##-----------------------------------------------------------------------------

# Load sample data
samps <- read_csv("data/AQPsamples_centroids_6.24.24.csv") %>%
  filter(! is.na(lon)) %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  st_set_crs(4326)
nrow(samps)  # 2119
length(unique(samps$PDL))  # 545

# Load health facilities and only keep those that fall within rectangle
#posts <- st_read("~/RabiesLabPeru/spatial_data/Spatial_data/Health_centers_AQP/Puestos_de_salud_AQP_12ene2024.kml")
#posts <- st_intersection(posts, rect_poly)
#hf <- read_rds("data/healthfacilities.rds")
hf <- read_delim("~/RabiesLabPeru/spatial_data/Spatial_data/Health_centers_AQP/Puestos_de_salud_APQ_12marzo2024.csv", delim = ";") %>%
  st_as_sf(coords=c("long", "lat")) %>%
  st_set_crs(4326)
hf <- st_intersection(hf, rect_poly)
plot(st_geometry(hf))

##-----------------------------------------------------------------------------
# 2. Find nearest health facility for each locality centroid
#    Adapted from: https://stackoverflow.com/questions/57571218/r-how-to-calculate-distance-between-two-sets-of-coordinate-points
##-----------------------------------------------------------------------------

# Calculate distance to nearest facility
samps_dist <- samps %>%
  group_by(samp_id) %>%
  mutate(nearest_facility = st_nearest_feature(geometry, hf),
         dist_euc = as.numeric(st_distance(geometry, hf[nearest_facility,])))
summary(samps_dist$dist_euc)

# Boxplot of sample positivity vs. distance to the nearest health facility
boxplot(samps_dist$dist_euc ~ samps_dist$RESULTADO.FINAL)

# Save sample data with calculated euclidean distances
write_rds(samps_dist, "data/AQPsamples_eucdisthf_7.31.24.rds")
