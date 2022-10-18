# Prep for R Spatial

library(raster)
library(sf)
library(tidyverse)

## Make Sites dataframe ----

## Make Plots dataframe ----
# I want to make 10 random plots within the extent of the raster data for the NW
#   corner of Utah
(plots_ext_utm <- extent(elev_snow_stk))

# convert to latlong
ext_sf <- data.frame(id = 1:2,
                     x = c(plots_ext_utm@xmin, plots_ext_utm@xmax),
                     y = c(plots_ext_utm@ymin, plots_ext_utm@ymax)) %>%
  st_as_sf(coords = c("x", "y"), crs = 26912) %>%
  st_transform(crs = 4326)
plot(ext_sf, col = "black", pch = 16)
(plots_ext_latlon <- extent(ext_sf))

# make a dataframe of random coordinates within this extent
(plots <- data.frame(Plots = LETTERS[1:10],
           Latitude = runif(10, min = plots_ext_latlon@ymin, 
                            max = plots_ext_latlon@ymax),
           Longitude = runif(10, min = plots_ext_latlon@xmin, 
                             max = plots_ext_latlon@xmax)))
write.csv(plots, "Data/Exercises/Plots_location.csv", row.names = F)

## info for each plot ----
library(lubridate)

startdate <- ymd("2021-04-30") 

plot_data <- lapply(1:10, function(i){
  start <- startdate + period(i, units = "days")
  end <- start + period(4*7, units = "days")
  dates <- seq(start, end, by = 7)
  plot_data <- data.frame(Plots = LETTERS[i],
             Species = c(rep("R. maritimus", 5), 
                         rep("B. cernua", 5), 
                         rep("S. acutus", 5)),
             Date = rep(dates, 3),
             AboveGroundBiomass = runif(15, 0, 100),
             MeanHeight = runif(15, 0, 5),
             PercentCover = runif(15, 0, 100))
  return(plot_data)
}) %>%
  bind_rows() %>%
  arrange(Date)
plot_data
write.csv(plot_data, "data/Exercises/Plots_data.csv", row.names = F)

## Make a landcover dataframe just for Utah ----
box_dir <- "../../Avgar Lab on WILD/UtahEnvironmentalCovariates/"

state_summary <- read.csv(paste0(box_dir, 
    "Landcover/gaplf2011lc_v30_state_summary.csv"))
state_summary <- state_summary %>%
  dplyr::select(-c(intStCode, intAcres, intSqMiles, numPercent)) %>%
  rename(StName = strStName) %>%
  rename(ClassCode = intClassCode) %>%
  rename(ClassName = strClassName) %>%
  rename(FormCode = strFormCode) %>%
  rename(FormName = strFormName) %>%
  rename(MacroCode = strMacroCode) %>%
  rename(MacroName = strMacroName) %>%
  rename(EcoSysCode = intEcoSysCode) %>%
  rename(EcoSysName = strEcoSysName)

land_attr_df <- read.csv(paste0(box_dir, 
                                "Landcover/GAP_LANDFIRE_National_Terrestrial_Ecosystems_2011_Attributes.csv"))
land_attr_df <- land_attr_df %>%
  dplyr::select(-c(Count, RED, GREEN, BLUE, NVCMES, LEVEL3)) %>%
  filter(!(Value == 0)) %>%
  rename(ClassCode = CL) %>%
  rename(ClassName = NVC_CLASS) %>%
  rename(SubClassCode = SC) %>%
  rename(SubClassName = NVC_SUBCL) %>%
  rename(FormCode = FRM) %>%
  rename(FormName = NVC_FORM) %>%
  rename(DivCode = DIV) %>%
  rename(DivName = NVC_DIV) %>%
  rename(MacroCode = MACRO_CD) %>%
  rename(MacroName = NVC_MACRO) %>%
  rename(GroupCode = GR) %>%
  rename(GroupName = NVC_GROUP) %>%
  rename(EcoSysName = ECOLSYS_LU)
write.csv(land_attr_df, "data/landcover_info.csv", row.names = F)

land_df <- left_join(state_summary, land_attr_df, 
                     by = c("ClassCode", "ClassName", "FormCode", "FormName", 
                            "MacroCode", "MacroName", "EcoSysName"))  
land_df <- land_df %>%
  relocate(Value, .before = ClassCode) %>%
  relocate(c(EcoSysCode, EcoSysName), .after = GroupName) %>%
  relocate(c(SubClassCode, SubClassName), .after = ClassName)

write.csv(land_df, "data/landcover_info.csv")


# Load boundaries ----
boundary_dir <- "../../Research/Movement Barriers/Data/Barriers/Fences/Sources/"
boundaries <- st_read(dsn = paste0(boundary_dir, "UT_SITLA_Ownership_LandOwnership_WM"), 
                      layer = "UT_SITLA_Ownership_LandOwnership_WM")
boundaries <- boundaries %>%
  st_transform(crs = 4326)
plot(st_geometry(boundaries))

boundaries <- boundaries %>%
  dplyr::select(-c(Edit_Date, Label_Fede, Label_Stat, GIS_Acres, COUNTY, STATE_LGD,
                   UT_LGD))

# dir.create("data/Exercises/UT_land_ownership")
st_write(boundaries, "data/Exercises/UT_land_ownership",
         "UT_land_ownership", driver = "ESRI Shapefile")

# dir.create("data/Examples/UT_land_ownership")
boundaries %>%
  st_transform(crs = 26912) %>%
  st_write("data/Examples/UT_land_ownership",
           "UT_land_ownership_utm", driver = "ESRI Shapefile")

# Make elevation and snow rasters ----
elev <- raster("../../Avgar Lab on WILD/UtahEnvironmentalCovariates/DEM/dem_wgs.tif")
elev

snow <- stack("../../Avgar Lab on WILD/UtahEnvironmentalCovariates/SNODAS/daily_rasters_native_resolution/SNODAS_20190223.tif")
snow

ext_wgs <- extent(c(-112, -110, 40, 42))
crop(elev, ext_wgs, filename = "Data/elev_crop", format = "GTiff")
crop(snow, ext_wgs, filename = "Data/snow_crop", format = "GTiff")

elev_crop <- raster("Data/elev_crop.tif")
elev_crop
plot(elev_crop)

