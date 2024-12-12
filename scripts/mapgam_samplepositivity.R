library(tidyverse)
library(MapGAM)
library(splancs)
library(sf)
library(sp)
library(raster)
library(RColorBrewer)
library(viridis)
library(PBSmapping)
library(nngeo)
library(ggmap)
library(ggsn)

register_google(key="AIzaSyBrwTLG8vz3RPeoVUrM4e6wcWMGOimtTrs")
register_stadiamaps(key="07107bab-7f12-480f-812b-6ed99f0cb3e2")
setwd("~/Rabies SES/")

##-----------------------------------------------------------------------------
# 0. User-defined functions
##-----------------------------------------------------------------------------

GetColPal <- function(logz){
  minOR <- min(logz)
  maxOR <- max(logz)
  mypal <- colorRampPalette(rev(brewer.pal(n = 11, name = "RdGy")))
  precols <- mypal(100)
  
  if (abs(minOR) < maxOR){
    len <- abs(minOR)/maxOR*50
    mycols <- precols[c(seq(1, 50, length.out = len), 51:100)]
  }else{
    len <- maxOR/abs(minOR)*50
    mycols <- precols[c(1:50, seq(51, 100, length.out = len))]
  }
  
  return(mycols)
}

##-----------------------------------------------------------------------------
# 1. Load data
##-----------------------------------------------------------------------------

# Load sample data with locality centroids
samp <- read_rds("data/AQPsamples_eucdisthf_7.31.24.rds")
nrow(samp)  # 2119

# Load hull polygon 
poly <- read_rds("data/hullfromblocks_7.31.24.rds")
plot(poly)

##-----------------------------------------------------------------------------
# 2. Prepare sample data for MapGAM
##-----------------------------------------------------------------------------

# Prep input data - SES as a binary variable
samp_coords <- st_coordinates(samp)
samp_dt_binaryses <- bind_cols(st_drop_geometry(samp), samp_coords) %>%
  mutate(case = ifelse(RESULTADO.FINAL == "POSITIVO", 1, 0)) %>%
  mutate(lowses = ifelse(SES_median %in% c("D", "E", "Undef"), 1, 0)) %>%
  ungroup() %>%
  dplyr::select(case, X, Y, lowses, dist_euc) %>%
  rename(Xcoord = X, Ycoord = Y, dist_hf = dist_euc)
sum(complete.cases(samp_dt_binaryses))  # 2119 - no missing

# Prep input data - SES as an ORDINAL variable
samp_dt_ordinalses <- bind_cols(st_drop_geometry(samp), samp_coords) %>%
  mutate(case = ifelse(RESULTADO.FINAL == "POSITIVO", 1, 0)) %>%
  mutate(ordinalses = case_when(SES_median == "A" ~ 1,
                                SES_median == "B" ~ 2,
                                SES_median == "C" ~ 3,
                                SES_median == "D" ~ 4,
                                SES_median %in% c("E", "Undef") ~5)) %>%
  ungroup() %>%
  dplyr::select(case, X, Y, ordinalses, dist_euc) %>%
  rename(Xcoord = X, Ycoord = Y, dist_hf = dist_euc)
sum(complete.cases(samp_dt_ordinalses))  # 2119 - no missing

##-----------------------------------------------------------------------------
# 3. Fit spatial GAMs
##-----------------------------------------------------------------------------

# Create rectangular grid of points to cover the study area
gamgrid <- predgrid(samp_dt_binaryses, map = poly)

# Fit CRUDE GAM model to our data 
fit <- modgam(data=samp_dt_binaryses, rgrid=gamgrid, permute=1000, 
              pointwise = TRUE, m="crude")
# --> save output
saveRDS(fit, "output/mapgam_crudepositivity_hull_7.31.24.rds")
colormap(fit, contours = "permrank", alpha=0.01)

# Fit adjusted GAM model to our data - BINARY SES + HF euc dist variables
fit_adj_binhf <- modgam(data=samp_dt_binaryseshf, rgrid=gamgrid, permute=1000, 
                      pointwise = TRUE, m="adjusted")
# --> save output
saveRDS(fit_adj_binhf, "output/mapgam_adjpositivity_hull_binarysesHF_7.31.24.rds")
colormap(fit_adj_binhf, contours = "permrank", alpha=0.01)

# Fit adjusted GAM model to our data - ORDINAL SES + HF euc dist variables
fit_adj_ordhf <- modgam(data=samp_dt_ordinalseshf, rgrid=gamgrid, permute=1000, 
                        pointwise = TRUE, m="adjusted")
# --> save output
saveRDS(fit_adj_ordhf, "output/mapgam_adjpositivity_hull_ordinalsesHF_7.31.24.rds")
colormap(fit_adj_ordhf, contours = "permrank", alpha=0.01)


##-----------------------------------------------------------------------------
# 4. Make map from CRUDE MapGAM output
##-----------------------------------------------------------------------------

fit <- read_rds("output/mapgam_crudepositivity_hull_7.31.24.rds")

# Extract the elements from MapGAM output so we can create our own raster
str(fit, 1)
x <- fit[[1]][[1]]  # x coords
y <- fit[[1]][[2]]  # y coords
z <- fit[[8]]       # OR
summary(z)          # 0.139-28.14
p <- fit[[11]]      # p-values 

# Create OR data frame from MapGAM output and convert to raster
xyz <- data.frame(cbind(x=x, y=y, OR=z))
raster.OR <- rasterFromXYZ(xyz)

# Create p-value data frame 
xyp <- data.frame(cbind(x=x, y=y, p=p))

# Convert raster to shapefiles (which can then be transparently overlaid on a map)
rtp <- rasterToPolygons(raster.OR)
rtp@data$id <- 1:nrow(rtp@data)
rtpFort <- fortify(rtp, data=rtp@data)
rtpFortMer <- merge(rtpFort, rtp@data, by.x = "id", by.y = "id")

# Find colors for plotting. The goal here is to make odds ratio (OR) = 1 white,
# OR > 1 (increased risk) red and OR < 1 blue
mycols <- GetColPal(fit[[7]])

# Get basemap 
st_bbox(poly)
map_borders <- c(bottom = -16.540, top = -16.275, left = -71.680, right = -71.450)
aqp_terrain <- get_stadiamap(bbox = map_borders, zoom = 12, maptype = "stamen_terrain")
basemap <- ggmap(aqp_terrain)

# Plost raster using ggplot
pdf("figures/R_output/mapgam_hull_crude.pdf")
basemap + 
  geom_polygon(data = rtpFortMer, aes(x=long, y = lat, group = group, fill = OR),
               alpha = 0.8) +
  scale_fill_gradientn(colours = mycols, trans = "log",
                       breaks = c(0.2, 1, 5, 25)) +
  # Draw contour line around significant hot spots (p > 0.99)
  # This is equivalent to performing one-sided local significance tests with alpha < 0.01
  stat_contour(data=xyp, aes(x,y,z=p), colour = mycols[76], linewidth=1.2, 
               linetype = "dashed", breaks=c(0.99))  +
  theme_void() +
  theme(legend.title=element_text(size = 22, face = "bold"),
        legend.text = element_text(size = 18)) 

# Make another version of map with scalebar
basemap + 
  geom_polygon(data = rtpFortMer, aes(x=long, y = lat, group = group, fill = OR),
               alpha = 0.8) +
  scale_fill_gradientn(colours = mycols, trans = "log",
                       breaks = c(0.2, 1, 5, 25)) +
  stat_contour(data=xyp, aes(x,y,z=p), colour = mycols[76], linewidth=1, 
               linetype = "dashed", breaks=c(0.99))  +
  theme_void() +
  theme(legend.title=element_text(size = 22, face = "bold"),
        legend.text = element_text(size = 18)) +
  scalebar(location = "bottomright", dist_unit = "km", data = samp, 
         dist = 5, transform = TRUE, model = 'WGS84')
# Make map with scalebar
dev.off()


##-----------------------------------------------------------------------------
# 6. Make map from ADJUSTED MapGAM output with binary SES
##-----------------------------------------------------------------------------

fit_adj <- read_rds("output/mapgam_adjpositivity_hull_binarysesHF_7.31.24.rds")
summary(fit_adj)

# Get OR and 95%-CI for lowses
coef(fit_adj)
exp(coef(fit_adj)[4])  # 1.008342
exp(coef(fit_adj)[4]-1.96*1.747782e-01)  # 0.7158691 
exp(coef(fit_adj)[4]+1.96*1.747782e-01)  # 1.420306 

# Get OR and 95%-CI for 10-m increase in distance to nearest HF
coef(fit_adj)
exp(coef(fit_adj)[5]*10)  # 1.430723 
exp((coef(fit_adj)[5]-1.96*8.773549e-03)*10)  # 1.204685  
exp((coef(fit_adj)[5]+1.96*8.773549e-03)*10)  # 1.699172 
exp(coef(fit_adj))

# Extract the elements from MapGAM output so we can create our own raster
str(fit_adj, 1)
x <- fit_adj[[1]][[1]]  # x coords
y <- fit_adj[[1]][[2]]  # y coords
z <- fit_adj[[8]]       # OR
summary(z)
p <- fit_adj[[11]]      # p-values 

# Create OR data frame from MapGAM output and convert to raster
xyz <- data.frame(cbind(x=x, y=y, OR=z))
raster.OR <- rasterFromXYZ(xyz)

# Create p-value data frame 
xyp <- data.frame(cbind(x=x, y=y, p=p))

# Convert raster to shapefiles (which can then be transparently overlaid on a map)
rtp <- rasterToPolygons(raster.OR)
rtp@data$id <- 1:nrow(rtp@data)
rtpFort <- fortify(rtp, data=rtp@data)
rtpFortMer <- merge(rtpFort, rtp@data, by.x = "id", by.y = "id")

# Get basemap 
st_bbox(poly)
map_borders <- c(bottom = -16.540, top = -16.275, left = -71.680, right = -71.450)
aqp_terrain <- get_stadiamap(bbox = map_borders, zoom = 12, maptype = "stamen_terrain")
basemap <- ggmap(aqp_terrain)

# Plost raster using ggplot
pdf("figures/R_output/mapgam_hull_binarysesHF.pdf")
basemap + 
  geom_polygon(data = rtpFortMer, aes(x=long, y = lat, group = group, fill = OR),
               alpha = 0.8) +
  scale_fill_gradientn(colours = mycols, trans = "log",
                       breaks = c(0.2, 1, 5, 25),
                       limits = c(min(fit[[8]]), max(fit[[8]]))) +
  stat_contour(data=xyp, aes(x,y,z=p), colour = mycols[76], linewidth=1.2, 
               linetype = "dashed", breaks=c(0.99))  +
  theme_void() +
  theme(legend.title=element_text(size = 22, face = "bold"),
        legend.text = element_text(size = 18)) 
dev.off()

# 6. Make map from ADJUSTED MapGAM output with ordinal SES
##-----------------------------------------------------------------------------

fit <- read_rds("output/mapgam_crudepositivity_hull_7.31.24.rds")
fit_adj <- read_rds("output/mapgam_adjpositivity_microred_ordinalsesHF_7.31.24.rds")
summary(fit_adj)

# Get OR and 95%-CI for lowses
coef(fit_adj)
exp(coef(fit_adj)[4])  # 0.9775948
exp(coef(fit_adj)[4]-1.96*7.458419e-02)  # 0.8446398 
exp(coef(fit_adj)[4]+1.96*7.458419e-02)  # 1.131478 

# Get OR and 95%-CI for 100-m increase in distance to nearest HF
coef(fit_adj)
exp(coef(fit_adj)[5]*100)  # 1.036457 
exp((coef(fit_adj)[5]-1.96*8.887216e-05)*100)  # 1.018559  
exp((coef(fit_adj)[5]+1.96*8.887216e-05)*100)  # 1.054669 
round(exp(cbind(OR = coef(fit_adj), confint(fit_adj))), 4)

# Extracfit_adj# Extract the elements from MapGAM output so we can create our own raster
str(fit_adj, 1)
x <- fit_adj[[1]][[1]]  # x coords
y <- fit_adj[[1]][[2]]  # y coords
z <- fit_adj[[8]]       # OR
summary(z)
p <- fit_adj[[11]]      # p-values 

# Create OR data frame from MapGAM output and convert to raster
xyz <- data.frame(cbind(x=x, y=y, OR=z))
raster.OR <- rasterFromXYZ(xyz)

# Create p-value data frame 
xyp <- data.frame(cbind(x=x, y=y, p=p))

# Convert raster to shapefiles (which can then be transparently overlaid on a map)
rtp <- rasterToPolygons(raster.OR)
rtp@data$id <- 1:nrow(rtp@data)
rtpFort <- fortify(rtp, data=rtp@data)
rtpFortMer <- merge(rtpFort, rtp@data, by.x = "id", by.y = "id")

# Find colors for plotting. The goal here is to make odds ratio (OR) = 1 white,
# OR > 1 (increased risk) red and OR < 1 blue
# -- we'll get colors from the crude model
mycols <- GetColPal(fit[[7]])

# Get basemap 
st_bbox(poly)
map_borders <- c(bottom = -16.540, top = -16.275, left = -71.680, right = -71.450)
aqp_terrain <- get_stadiamap(bbox = map_borders, zoom = 12, maptype = "stamen_terrain")
basemap <- ggmap(aqp_terrain)

# Plost raster using ggplot
pdf("figures/R_output/mapgam_hull_ordinalsesHF.pdf")
basemap + 
  geom_polygon(data = rtpFortMer, aes(x=long, y = lat, group = group, fill = OR),
               alpha = 0.8) +
  scale_fill_gradientn(colours = mycols, trans = "log",
                       breaks = c(0.3, 1, 3, 10, 30), 
                       limits = c(min(fit[[8]]), max(fit[[8]]))) +
  #geom_path(data = areaA_pts, aes(x = x, y = y), cex = 1.3) + 
  # Draw contour line around significant hot spots (p > 0.995) and significant cold spots (p < 0.005)
  # This is equivalent of performing local significance tests with alpha < 0.01
  stat_contour(data=xyp, aes(x,y,z=p), colour = "black", size=1, linetype = "dashed",
               breaks=c(0.005, 0.995))  +
  theme_void() +
  theme(legend.title=element_text(size = 22, face = "bold"),
        legend.text = element_text(size = 18)) 
dev.off()

