library(tidyverse)
library(sf)
library(ggmap)
library(ggsn)
library(RColorBrewer)
library(viridis)

register_google(key="AIzaSyBrwTLG8vz3RPeoVUrM4e6wcWMGOimtTrs")
register_stadiamaps(key="07107bab-7f12-480f-812b-6ed99f0cb3e2")

##-----------------------------------------------------------------------------
# 1. Load and prep data
##-----------------------------------------------------------------------------

# Load samples SES data and remove all SES = NA
setwd("~/Rabies SES/")
samps <- readRDS("data/AQPsamples_SES_6.24.2024.rds") %>%
  # Remove samples that were not successfully assigned to a PDL code
  filter(! is.na(SES_median)) %>%
  mutate(year = format(date, "%Y")) %>%
  rename(SES = SES_median)
loc_summary <- samps %>%
  group_by(PDL) %>%
  summarise(samp_count = n())
summary(loc_summary$samp_count)

# Load localities shapefile and merge with sample counts
loc <- read_rds("data/AQPSESlocalities_9.19.2023.rds") %>%
  left_join(., loc_summary)

# Replace NA sample counts with 0's
loc$samp_count[is.na(loc$samp_count)] <- 0

# Make another sample count where we add 1 so 0's don't become NA's
loc$samp_count2 <- loc$samp_count + 1

##-----------------------------------------------------------------------------
# 2. Draw map boundary
##-----------------------------------------------------------------------------

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
  st_set_crs(st_crs(loc))
plot(rect_poly)

##-----------------------------------------------------------------------------
# 3. Map locality-level sample counts
##-----------------------------------------------------------------------------

pdf("figures/R_output/map_surveillance.pdf")

ggplot() +
  geom_sf(data = rect_poly, fill = "#F4F4F4") +
  geom_sf(data = loc, aes(fill = samp_count2), color = "NA", inherit.aes = F) +
  scale_fill_distiller(palette = "YlGnBu", trans = "log",
  #                    breaks = c(1, 3, 5, 9, 17, 33, 65, 129)) +
                       breaks = c(1, 3, 9, 33, 129)) +
  theme_void()

dev.off()
