library(tidyverse)
library(sf)
library(ggmap)
library(ggsn)

register_google(key="AIzaSyBrwTLG8vz3RPeoVUrM4e6wcWMGOimtTrs")
register_stadiamaps(key="07107bab-7f12-480f-812b-6ed99f0cb3e2")

##-----------------------------------------------------------------------------
# 0. User-defined functions
##-----------------------------------------------------------------------------

# Custom ggplot theme for mapping
my_theme <- function(){
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title =element_blank(),
        axis.text =element_blank(),
        axis.ticks =element_blank(),
        legend.title=element_text(size=24),
        legend.text = element_text(size = 22),
        plot.title = element_text(size = 26, face = "bold"))
}  

##-----------------------------------------------------------------------------
# 1. Load rabies case data
##-----------------------------------------------------------------------------

# Load AQP localities 
setwd("~/Rabies SES/")
localities <- read_rds("data/AQPSESlocalities_9.19.2023.rds")

# Load AQP districts
dist <- st_read("data/dist_AQPprov.shp")
plot(dist$geometry)

# Loadist# Load case coordinates by year
setwd("~/RabiesLabPeru")
cases <- read.csv("surveillance_rabies/data/merged_dataset/old/rabiescases_coords_15feb22.csv") %>%
  filter(! is.na(long)) %>%
  st_as_sf(coords=c("long", "lat")) %>%
  st_set_crs(4326) %>%
  select(year, ident_CASE, direcc)
table(cases$year)

setwd("~/RabiesLabPeru/Rabies data/Coordenadas casos rabia/")
# Confirm 2021 cases are the same in both datasets
cases21 <- st_read("Casos Rabia AQP 2021.kml")  
cases21_og <- filter(cases, year == 2021)
plot(st_geometry(cases21))                                                
plot(st_geometry(cases21_og), add = T, col = "red")  

# Load 2022 and 2023 cases
cases22 <- st_read("Casos Rabia AQP 2022.kml") %>%
  # Remove negative samples (re: Elvis WhatsApp 2/19/24 - these were IFD+ but PCR-)
  filter(! Name %in% c("Caso010(-)", "Caso032(-)")) %>%
  mutate(year = 2022,
         # Extract the numeric part of the name string
         id = gsub(".*?([0-9]+).*", "\\1", Name),
         ident_CASE = paste0(year, "_", id)) %>%
  rename(direcc = Description) %>%
  select(year, ident_CASE, direcc) %>%
  st_zm(.)

cases23 <- st_read("Casos Rabia AQP 2023 (19-02-2024).kml") %>%
  mutate(year = 2023,
         id = gsub(".*?([0-9]+).*", "\\1", Name),
         ident_CASE = paste0(year, "_", id)) %>%
  rename(direcc = Description) %>%
  select(year, ident_CASE, direcc) %>%
  st_zm(.)
cases23$ident_CASE[cases23$ident_CASE == "2023_15"] <- "2023_015"  # correct ID 

# Combine case data for 2015-2021, 2022, and 2023
cases_final <- bind_rows(cases, cases22, cases23) %>%
  bind_cols(., st_coordinates(.)) %>%
  rename(longitude = X, latitude = Y)

# Save combined case data
setwd("~/RabiesLabPeru/Rabies data")
write_rds(cases_final, "data_cleaned/rabies_casecoordinates_2.19.2024.rds")

# Remove coordinates outside of the city
hist(cases_final$longitude)
cases_aqp <- filter(cases_final, longitude > -71.8)
plot(st_geometry(cases_final))
plot(st_geometry(cases_aqp), col = "red", add = T)

# Remove cases in 2023
cases_no23 <- cases_aqp %>%
  filter(year != 2023)

##-----------------------------------------------------------------------------
# 2. Prep for mapping (color palette, basemaps, etc.)
##-----------------------------------------------------------------------------

# Custom pale red --> dark red VIBRANT color palette
cols8 <- c("#faafaf", "#f29696", "#f57d7d", "#e64747", "#cc2b2b", "#b31d1d", 
          "#940f0f", "#610101")

cols9 <- c("#faafaf", "#f57d7d", "#e64747", "#db3b3b", "#cc2b2b", "#b31d1d", 
          "#940f0f", "#7a0909", "#610101")

# Get basemap 
st_bbox(cases_no23)
map_borders <- c(bottom = -16.54, top = -16.275, left = -71.68, right = -71.45)
#aqp_satellite <- get_googlemap(bbox = map_borders, zoom = 12, maptype = "satellite")
aqp_terrain <- get_stadiamap(bbox = map_borders, zoom = 12, maptype = "stamen_terrain")
basemap <- ggmap(aqp_terrain)


##-----------------------------------------------------------------------------
# 3a. Make version of map that includes 2015-2022 data
##-----------------------------------------------------------------------------

setwd("~/Rabies SES/")
pdf("figures/R_output/map_cases.pdf")

basemap +
  geom_point(data = cases_no23, 
             mapping = aes(x = longitude, y = latitude, color = factor(year)),
             size = 1.5) +
  scale_color_manual(values = cols8) +
  my_theme()

basemap +
  geom_point(data = cases_no23, 
             mapping = aes(x = longitude, y = latitude, color = factor(year)),
             size = 1.5) +
  scale_color_manual(values = cols8) +
  scalebar(location = "bottomright", data = cases_aqp, dist_unit = "km",
           dist = 5, transform = TRUE, model = 'WGS84') +
  my_theme()
dev.off()

pdf("figures/R_output/map_cases_jittered_dist.pdf")

basemap +
  geom_sf(data = dist, inherit.aes = F, fill = NA, linewidth = 0.4, linetype = "dashed") +
  geom_jitter(data = cases_no23, 
             mapping = aes(x = longitude, y = latitude, color = factor(year)),
             size = 1.5, width = 0.002, height = 0.002) +
  scale_color_manual(values = cols8) +
  my_theme()

basemap +
  geom_jitter(data = cases_no23, 
             mapping = aes(x = longitude, y = latitude, color = factor(year)),
             size = 1.5) +
  scale_color_manual(values = cols8) +
  scalebar(location = "bottomright", data = cases_aqp, dist_unit = "km",
           dist = 5, transform = TRUE, model = 'WGS84') +
  my_theme()
dev.off()

##-----------------------------------------------------------------------------
# 3b. Make version of map that includes 2015-2023 data
##-----------------------------------------------------------------------------

basemap +
  geom_point(data = cases_aqp, 
             mapping = aes(x = longitude, y = latitude, color = factor(year)),
             size = 2) +
  #geom_sf(cases_final, aes(color = year)) +
  scale_color_manual(values = cols9) +
  #scalebar(location = "bottomright", data = cases_aqp, dist_unit = "km",
  #         dist = 5, transform = TRUE, model = 'WGS84') +
  my_theme()

basemap +
  scalebar(location = "bottomright", data = cases_aqp, dist_unit = "km",
           dist = 5, transform = TRUE, model = 'WGS84') +
  my_theme()

  