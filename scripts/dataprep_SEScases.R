library(tidyverse)
library(sf)
library(leaflet)

##-----------------------------------------------------------------------------
# 0. User-defined functions
##-----------------------------------------------------------------------------

# Modified one-way table function to show proportions
# tpt shows levels in decreasing frequency
# tpt2 shows levels in standard order

tb <- function(x){
  t <- rev(sort(table(x, useNA = "ifany")))
  return(t)
}

tb2 <- function(x){
  t <- table(x, useNA = "ifany")
  return(t)
}

ptb <- function(x){
  pt <- round(prop.table(tb(x))*100, 1)
  return(pt)
}

ptb2 <- function(x){
  pt <- round(prop.table(tb2(x))*100, 1)
  return(pt)
}

tpt <- function(x){
  t <- tb(x)
  pt <- ptb(x)
  tpt <- paste0(t, " (", pt,")")
  names(tpt) <- names(t)
  return(noquote(tpt))
}

tpt2 <- function(x){
  t <- tb2(x)
  pt <- ptb2(x)
  tpt <- paste0(t, " (", pt,")")
  names(tpt) <- names(t)
  return(noquote(tpt))
}

##-----------------------------------------------------------------------------
# 1. Integrate SES data with rabies case data -- cases that FALL IN A BLOCK
##-----------------------------------------------------------------------------

setwd("~/Rabies SES/")

ses_all <- readRDS("data/AQPSESblocks_4.3.2024.rds")
ses_all <- st_zm(ses_all)  # Remove Z dimension
table(ses_all$SES, useNA = "ifany")
tpt(ses_all$SES)
tpt2(ses_all$SES)

# Load cases from 2015-2022 
case <- readRDS("data/rabies_casecoordinates_2.19.2024.rds") %>%
  filter(year < 2023) %>%
  filter(longitude > -71.8)  # Remove cases that fall outside the city
table(case$year)
nrow(case)  # coordinates for 347 cases spanning 2015-2022

# Find the intersection of case data with SES blocks
sf_use_s2(FALSE)
case_ses_int <- st_intersection(case, ses_all) %>%
  st_drop_geometry()

# Update SES assignment for cases that fall within blocks
case_ses <- left_join(case, case_ses_int[, c("ident_CASE", "SES")])
table(case_ses$SES, useNA = "ifany")
#  A     B     C     D     E Undef  <NA> 
#  2     9    27    75    77    13   144 
sum(! is.na(case_ses$SES))  # 203
sum(is.na(case_ses$SES))  # 144

# See which cases are not intersecting 
no_int_ID <- case$ident_CASE[! case$ident_CASE %in% case_ses_int$ident_CASE]
no_int <- case %>%
  filter(ident_CASE %in% no_int_ID)
plot(st_geometry(ses_all))
plot(st_geometry(no_int), col = "red", add = T)

# Find center for mapping
bbox <- st_bbox(ses_all)
x_center <- as.numeric(bbox$xmin + bbox$xmax)/2
y_center <- as.numeric(bbox$ymin + bbox$ymax)/2

# Palette function for mapping
mypal <- c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c", "grey")
pal_ses <- colorFactor(palette = mypal, ses_all$SES)

# Map cases with no intersection against all SES blocks
leaflet() %>%
  setView(lng = x_center, lat = y_center, zoom = 11) %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(
    data = ses_all,
    fillColor = ~pal_ses(SES),
    weight = 0.3,
    opacity = 0.7,
    color = "white",
    dashArray = "1",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE)) %>% 
  addLegend(data = ses_all, pal = pal_ses, values = ~SES, opacity = 0.7, 
            title = "Block SES",
            position = "bottomright") %>%
  addCircleMarkers(
    data = no_int,
    radius = 0.002,
    color = "black") %>%
  addScaleBar()
  
# These points are those that don't fall within a block. So we'll have to 
# find the closest polygon to these points.
  
##-----------------------------------------------------------------------------
# 2b. Integrate SES data with rabies case data -- cases that FALL OUTSIDE OF 
#     BLOCKS
##-----------------------------------------------------------------------------
  
# Solution from: https://stackoverflow.com/questions/49200458/find-nearest-features-using-sf-in-r  
# Store the shortest distance between each point and its nearest block, as well
# as the nearest block itself 
shortest_distance <- as.numeric()
closest <- list()
for(i in seq_len(nrow(no_int))){
  shortest_distance[i] <- min(st_distance(ses_all, no_int[i,]), na.rm = T)
  closest[[i]] <- ses_all[which.min(st_distance(ses_all, no_int[i,])),]
}

# Let's figure out a cut-off threshold for which we won't assign a point to an
# SES block category
hist(shortest_distance)
sort(shortest_distance)  
hist(shortest_distance[shortest_distance < 50])

# Let's look on a map all cases that are >20 m from the nearest block
great20index <- which(shortest_distance > 20)
great20 <- no_int[great20index,]
nrow(great20)  # 11 cases

great50index <- which(shortest_distance > 50)
great50 <- no_int[great50index,]
nrow(great50)  # 2 cases

# Map cases that fall >20 m from nearest block
leaflet() %>%
  setView(lng = x_center, lat = y_center, zoom = 11) %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(
    data = ses_all,
    fillColor = ~pal_ses(SES),
    weight = 0.3,
    opacity = 0.7,
    color = "white",
    dashArray = "1",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE)) %>% 
  addLegend(data = ses_all, pal = pal_ses, values = ~SES, opacity = 0.7, 
            title = "Block SES",
            position = "bottomright") %>%
  addCircleMarkers(
    data = great20,
    radius = 0.002,
    color = "black") %>%
addScaleBar()

# Map cases that fall >50 m from nearest block
leaflet() %>%
  setView(lng = x_center, lat = y_center, zoom = 11) %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(
    data = ses_all,
    fillColor = ~pal_ses(SES),
    weight = 0.3,
    opacity = 0.7,
    color = "white",
    dashArray = "1",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE)) %>% 
  addLegend(data = ses_all, pal = pal_ses, values = ~SES, opacity = 0.7, 
            title = "Block SES",
            position = "bottomright") %>%
  addCircleMarkers(
    data = great50,
    radius = 0.002,
    color = "black") %>%
addScaleBar()


# From manually reviewing these points. I feel comfortable setting the threshold at 50 m
# Make SES assignments to no_int and then merge these assignments back to case
for(i in seq_len(nrow(no_int))){
  caseID <- no_int$ident_CASE[i]
  if (shortest_distance[i] < 50){
    case_ses$SES[case_ses$ident_CASE == caseID] <- closest[[i]]$SES
  }
}
table(case_ses$SES, useNA = "ifany")
table(case_ses$year, case_ses$SES, useNA = "ifany")
tpt2(case_ses$SES)
tpt2(ses_all$SES)

# Make map to check that SES assignments worked correctly
leaflet() %>%
  setView(lng = x_center, lat = y_center, zoom = 11) %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(
    data = ses_all,
    fillColor = ~pal_ses(SES),
    weight = 0.3,
    opacity = 0.7,
    color = "white",
    dashArray = "1",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE)) %>% 
  addCircleMarkers(
    data = case_ses,
    radius = 0.005,
    color = ~pal_ses(SES)) %>%
  addLegend(data = case_ses, pal = pal_ses, values = ~SES, opacity = 0.7, 
            title = "SES",
            position = "bottomright") %>%
addScaleBar()

# Save cases with SES data
saveRDS(case_ses, "data/AQPrabiescases_SES_4.3.2024.rds")
