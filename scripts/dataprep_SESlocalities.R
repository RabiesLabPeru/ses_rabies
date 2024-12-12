library(tidyverse)
library(sf)
library(nngeo)

setwd("~/Rabies SES/")

##-----------------------------------------------------------------------------
# 0. User-defined functions
##-----------------------------------------------------------------------------

FindMedianSES <- function(ses_vec){
  if(sum(ses_vec == "Undef") > length(ses_vec)*0.5){
    median_ses <- "Undef"
  }else{
    ses_vec2 <- ses_vec[ses_vec != "Undef"]  # Remove undef level
    median_num <- median(as.numeric(ses_vec2))
    median_ses <- c("A", "B", "C", "D", "E")[median_num]
  }
  return(median_ses)
}

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

##-----------------------------------------------------------------------------
# 1. Load data
##-----------------------------------------------------------------------------

# Load SES data and create PDL variable from BlockID's
ses_blocks <- readRDS("data/AQPSESblocks_4.3.2024.rds") %>%
  mutate(PDL = sub("(^[^-]+)-.*", "\\1", BlockID))
ses_blocks <- st_zm(ses_blocks)  # Drop Z dimension
ses_blocks$SES[ses_blocks$SES == "NA"] <- "Undef"  # Rename NA level
ses_blocks$SES <- factor(ses_blocks$SES)  # convert to factor

# How many blocks? How many localities?
nrow(ses_blocks)  # 27218
length(unique(ses_blocks$BlockID))  # 27213
length(unique(ses_blocks$PDL))  # 1317

# How 
##-----------------------------------------------------------------------------
# X. Return to this later - need to melt shape and remove gaps between blocks
##-----------------------------------------------------------------------------

# Try aggregation on a single PDL code
tmp <- filter(ses_blocks, PDL == "1.1.1")
tmp_hull <- st_convex_hull(st_union(tmp))
plot(st_geometry(tmp_hull))
plot(tmp, add = T)
tmp2 <- st_union(tmp)
plot(st_geometry(tmp2))
plot(st_geometry(tmp))
tmp <- st_buffer(tmp, .1)
tmp <- st_difference(tmp, tmp)

tmp3 <- tmp %>%
  st_buffer(.3) %>%
  st_union() %>%
  st_remove_holes()
plot(st_geometry(tmp3))

##-----------------------------------------------------------------------------
# 2. Aggregate blocks to form localities based on PDL code and determine 
#    locality-level SES
# -- Spatial aggregation solution from: https://gis.stackexchange.com/questions/316181/how-do-i-combine-geometries-in-a-shapefile-based-on-a-grouping-variable
##-----------------------------------------------------------------------------

tmp <- filter(ses_blocks, PDL == "1.1.1")
FindMedianSES(tmp$SES)  

# Create localities sf
ses_loc <- ses_blocks %>%
  group_by(PDL) %>%
  summarize(SES_median = FindMedianSES(SES),
            SES_mode = getmode(SES),
            geometry = st_union(st_make_valid(geometry)))
#plot(st_geometry(ses_loc))  # looks the same as blocks

# Get count of blocks per locality
loc_block_count <- st_drop_geometry(ses_blocks) %>%
  group_by(PDL) %>%
  summarize(block_count = length(unique(BlockID)))
summary(loc_block_count$block_count)

# View agreement between SES determined via median vs. mode methods
table(ses_loc$SES_median, ses_loc$SES_mode)

# Save localities SES data
setwd("~/Rabies SES/")
saveRDS(ses_loc, "data/AQPSESlocalities_4.8.2024.rds")

# Add locality centroids
loc_centroid <- st_centroid(ses_loc) %>% # get locality centroids
  mutate(geom = gsub(geometry,pattern="(\\))|(\\()|c",replacement = ""))%>%
  tidyr::separate(geom,into=c("lon","lat"),sep=",")%>%
  st_as_sf(.,coords=c("lon","lat"),crs=st_crs(locs)) %>%
  select(PDL, lon, lat) %>%
  st_drop_geometry
# - Merge locality centroids with samples
samps_centroid <- left_join(samps, loc_centroid)
write_csv(samps_centroid, "data/AQPsamples_centroids_6.24.24.csv")

##-----------------------------------------------------------------------------
# 3. Map blocks and localities shaded according to SES
##-----------------------------------------------------------------------------

ggplot(ses_blocks) +
  geom_sf(aes(fill = SES), color = NA) +
  scale_fill_manual(values = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", 
                               "#d7191c", "grey")) +
  theme_void()

ggplot(ses_loc) +
  geom_sf(aes(fill = SES_median), color = NA) +
  scale_fill_manual(values = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", 
                               "#d7191c", "grey")) +
  theme_void()
