library(tidyverse)
library(sf)
library(irr)

setwd("~/Rabies SES/")

# Load blocks and localities shapefiles with SES assignment and remove spatial
block <- read_rds("data/AQPSESblocks_4.3.2024.rds") %>% st_drop_geometry()
loc <- read_rds("data/AQPSESlocalities_4.8.2024.rds") %>% st_drop_geometry()

# Match blocks to localities based on PDL codes
block <- block %>%
  rename(block_ses = SES) %>%
  mutate(PDL = sub("(^[^-]+)-.*", "\\1", BlockID)) %>%
  left_join(., loc) %>%
  rename(locality_ses = SES_median) %>%
  select(BlockID, PDL, block_ses, locality_ses)
summary(is.na(block$locality_ses))
table(block$block_ses, block$locality_ses)
kappa2(block[,c("block_ses", "locality_ses")], weight = "unweighted")
# Cohen's Kappa for 2 Raters (Weights: unweighted)
# Subjects = 27218 
#   Raters = 2 
#    Kappa = 0.834 
#        z = 286 
#  p-value = 0 