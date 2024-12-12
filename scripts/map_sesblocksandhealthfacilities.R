library(tidyverse)
library(sf)
library(ggmap)
library(ggsn)
library(RColorBrewer)

register_google(key="AIzaSyBrwTLG8vz3RPeoVUrM4e6wcWMGOimtTrs")
register_stadiamaps(key="07107bab-7f12-480f-812b-6ed99f0cb3e2")

##-----------------------------------------------------------------------------
# 1. Load and prep data
##-----------------------------------------------------------------------------

# Load and clean SES blocks
setwd("~/Rabies SES/")
blocks <- read_rds("data/AQPSESblocks_4.3.2024.rds")
blocks <- st_zm(blocks)  # Drop Z dimension

table(blocks$SES)
blocks$SES <- factor(blocks$SES)  # convert to factor
table(blocks$SES)

# Draw rectangle for plotting
bottom <- -16.54
top <- -16.275
left <- -71.68
right <- -71.45
rect_df <- data.frame(
  X = c(left, left, right, right),
  Y = c(bottom, top, top, bottom))
rect_poly <- rect_df %>%
  st_as_sf(coords = c("X", "Y")) %>%
  summarise((geometry = st_combine(geometry))) %>%
  st_cast("POLYGON") %>%
  st_set_crs(st_crs(blocks))
plot(rect_poly)

# Load health facilities and only keep those that fall within rectangle
#posts <- st_read("~/RabiesLabPeru/spatial_data/Spatial_data/Health_centers_AQP/Puestos_de_salud_AQP_12ene2024.kml")
#posts <- st_intersection(posts, rect_poly)
#hf <- read_rds("data/healthfacilities.rds")
hf <- read_delim("~/RabiesLabPeru/spatial_data/Spatial_data/Health_centers_AQP/Puestos_de_salud_APQ_12marzo2024.csv", delim = ";") %>%
  st_as_sf(coords=c("long", "lat")) %>%
  st_set_crs(4326)
hf <- st_intersection(hf, rect_poly)
plot(st_geometry(hf))

# Intersect blocks 
sf_use_s2(FALSE)
blocks <- st_intersection(st_make_valid(blocks), rect_poly)

##-----------------------------------------------------------------------------
# 2. Map SES blocks
##-----------------------------------------------------------------------------

pdf("figures/R_output/map_SESblockshealthfacilities.pdf")
ggplot() +
  geom_sf(data = rect_poly, fill = "#F4F4F4") +
  geom_sf(data = blocks, aes(fill = SES), inherit.aes = F, 
          color = NA) +
  scale_fill_manual(values = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", 
                               "#d7191c", "darkgrey")) +
  geom_sf(data = hf, shape = 17, size = 3.5) +
  theme_void()

ggplot() +
  geom_sf(data = rect_poly, fill = "#F4F4F4") +
  geom_sf(data = blocks, aes(fill = SES), inherit.aes = F, 
          color = NA) +
  scale_fill_manual(values = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", 
                               "#d7191c", "darkgrey")) +
  geom_sf(data = hf, shape = 17, size = 3.5) +
  scalebar(location = "bottomleft", data = blocks, dist_unit = "km",
           dist = 5, transform = TRUE, model = 'WGS84') +
  theme_void()
dev.off()

