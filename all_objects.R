# Code with all the objects needed for 01, 02, 03

library(maps)
library(raster)
library(sf)
library(tidyverse)

utah <- maps::map("state", plot = F, fill = TRUE) %>%
  # turn into sf obj
  sf::st_as_sf() %>%
  # pull out utah 
  dplyr::filter(ID == "utah")

sites <- read.csv("Data/Examples/Sites.csv")
sites_sf <- st_as_sf(sites, 
                     coords = c("Longitude", "Latitude"), 
                     crs = 4326)
sites_sf_proj <- st_transform(sites_sf, 26912)

fwy_sf <- st_read(dsn = "Data/Examples/utah_freeway", layer = "utah_freeway")
fwy_sf_proj <- st_transform(fwy_sf, crs = 26912)

elev_snow_stk <- stack("Data/demo/elev_snow_nw_stack.tif")
names(elev_snow_stk) <- c("elevation", "swe", "snow_depth")

plot_data <- read.csv("data/Exercises/Plots_data.csv")
plots_sf <- read.csv("Data/Exercises/Plots_location.csv") %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs = 26912)
plots_join <- left_join(plots_sf, plot_data, by = "Plots")

manage_sf <- st_read("data/Exercises/UT_land_management", "UT_land_management",
                     quiet = T) %>%
  st_transform(crs = 26912)

manage_roads <- st_intersects(fwy_sf_proj, manage_sf) 
manage_roads_index <- unique(unlist(manage_roads)) 
manage_roads_intersect <- manage_sf[manage_roads_index, ] 

landcover <- raster("Data/Examples/landcover.tif")
sites_sf_proj$land_value <- raster::extract(landcover, sites_sf_proj)
land_info <- read.csv("Data/Examples/landcover_info.csv")
sites_sf_land <- sites_sf_proj %>%
  left_join(land_info, c("land_value" = "Value"))
landcover_rcl <- raster("Data/Examples/landcover_rcl.tif")

dist <- st_distance(sites_sf_proj, fwy_sf_proj)
near_road <- st_nearest_feature(sites_sf_proj, fwy_sf_proj)
dist_near_road <- st_distance(sites_sf_proj, fwy_sf_proj[near_road,])
dist_near_road <- st_distance(sites_sf_proj, fwy_sf_proj[near_road,],
                              by_element = TRUE)
sites_sf_proj$dist_near_road <- dist_near_road

sites_rast <- raster::extract(elev_snow_stk, sites_sf_proj)
sites_sf_proj <- cbind(sites_sf_proj, as.data.frame(sites_rast)) 
sites_sf_rast <- sites_sf_proj %>%
  filter(!is.na(elevation) & !is.na(swe) & !is.na(snow_depth))
site_sf_rast_01 <- sites_sf_rast[1,]
site_buff_5km <- st_buffer(site_sf_rast_01, 5000)
stack_crop <- crop(elev_snow_stk, site_buff_5km)
stack_stats <- data.frame(
  mean_5km = cellStats(stack_crop, stat = "mean"),
  median_5km = cellStats(stack_crop, stat = "median"),
  min_5km = cellStats(stack_crop, stat = "min"),
  max_5km = cellStats(stack_crop, stat = "max"),
  sd_5km = cellStats(stack_crop, stat = "sd"))
stack_stats$environment <- row.names(stack_stats)
row.names(stack_stats) <- NULL

