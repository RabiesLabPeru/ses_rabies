library(tidyverse)
library(sf)

# Load health post data from Mica
# Note csv is a semicolon-delimited file
setwd("/Users/sxs/Dropbox/Postdoc/RabiesLabPeru/peru_spatial_data/data_original/05_healthcenters/")
posts <- read_delim("PUESTOS DE SALUD AQP_2018.csv", delim = ";")

# Covert data frame to sf
posts_sf <- posts %>%
  st_as_sf(coords=c("long", "lat")) %>%
  st_set_crs(4326)

# Save
setwd("~/Rabies SES/data/")
write_rds(posts_sf, "healthfacilities.rds")
