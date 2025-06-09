library(tidyverse)
library(terra)
library(sf)
library(sp)
library(ggplot2)
library(tigris)
library(tmap)
library(raster)
library(units)
library(purrr)
library(progress)

projection = "epsg:5070"


# tigris shape file of Idaho
states <- tigris::states(year = 2024)
id <- states %>%
  filter(STUSPS == "ID")

fed <- st_read("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/original/PADUS4_0Geodatabase/PADUS4_0_Geodatabase.gdb", layer = "PADUS4_0Fee") 

id_fed <- fed %>%
  filter(State_Nm == "ID") %>%
  filter(Mang_Type == "FED")

rm(fed)

sf_use_s2(FALSE)

## check for validity, remove empty geometries, and reproject 
if (!all(st_is_valid(id)))
  id <- st_make_valid(id)

id <- id %>%
  filter(!st_is_empty(.))

id_proj <- st_transform(id, projection)

id_fed_proj <- st_transform(id_fed, projection)

## Make the multisurface into multipolygons
id_fed_proj <- id_fed_proj %>% st_cast("MULTIPOLYGON")

# then check for and fix validity and empty geometries
if (!all(st_is_valid(id_fed_proj)))
  id_fed_proj <- st_make_valid(id_fed_proj)

any(st_is_empty(id_fed_proj))

## Filter for Federal agency name and type
fed_proj_name <- id_fed_proj %>%
  dplyr::select(Mang_Name)
fed_proj_type <- id_fed_proj %>%
  dplyr::select(Mang_Type)

# Create a function to make unioned simple features for the different federal agencies

union_function <- function(sf, name){
  tmp <- sf %>%
    filter(Mang_Name == name)
  tmp_union <- st_as_sf(st_union(tmp)) %>%
    mutate(fed_name = name)
}

## create unioned simple features for each agency
blm_union <- union_function(fed_proj_name, "BLM")
doe_union <- union_function(fed_proj_name, "DOE")
usbr_union <- union_function(fed_proj_name, "USBR")
usfs_union <- union_function(fed_proj_name, "USFS")
nps_union <- union_function(fed_proj_name, "NPS")
fws_union <- union_function(fed_proj_name, "FWS")

## combine into one simple feature
fed_name_union <- bind_rows(blm_union, doe_union, usbr_union, usfs_union, nps_union, fws_union)

# Create a sample grid of Idaho to complete the calculations.
# Need to make the grid a polygon and count the different agencies in each cell

id_cells <- st_make_grid(id_proj, cellsize = 3000)
id_cells_sf <- st_sf(id_cells) 
# add unique cell id
id_cells_sf <- id_cells_sf %>% 
  mutate(GRIDCELL_REFID = as.character(row_number()))

## Create a template raster for the shapefiles
XMIN <- ext(id_cells_sf)$xmin
XMAX <- ext(id_cells_sf)$xmax
YMIN <- ext(id_cells_sf)$ymin
YMAX <- ext(id_cells_sf)$ymax
aspectRatio <- (YMAX-YMIN)/(XMAX-XMIN)
cellSize <- 3000
NCOLS <- as.integer((XMAX-XMIN)/cellSize)
NROWS <- as.integer(NCOLS * aspectRatio)
id_cells_rst <- rast(ncol=NCOLS, nrow=NROWS, 
                        xmin=XMIN, xmax=XMAX, ymin=YMIN, ymax=YMAX,
                        vals=1, crs=projection)

#----richness----
# Calculate the number of federal agencies on each 3km grid cell

#id_fed_name_int <- st_intersection(id_cells_sf, fed_proj_name)
id_fed_name_int <- st_intersection(id_cells_sf, fed_name_union)
id_fed_name_rich <- id_fed_name_int %>%
  group_by(., GRIDCELL_REFID) %>%
  summarise(., numfed = n()) # create a new variable with the count of unique agencies 
  
# rasterize the grid cells
id_fed_name_rich_rst <- rasterize(id_fed_name_rich, id_cells_rst, field = "numfed")

# check the raster
plot(id_fed_name_rich_rst)


# Fill in na with 0 and save the raster
id_fed_name_rich_rst[is.na(id_fed_name_rich_rst[])] <- 0 
plot(id_fed_name_rich_rst)

# Resample and crop to the reference raster
ref_rast <- rast(here::here("data/processed/wfrc_BP_ID_3km_2024-12-03.tif"))
id_fed_name_rich_rst_resamp <- resample(id_fed_name_rich_rst, ref_rast, method = "near")
#plot(id_fed_name_rich_rst_resamp)

id_fed_name_rich_rst_crop <- crop(id_fed_name_rich_rst_resamp, ref_rast, mask = TRUE)
#plot(id_fed_name_rich_rst_crop)
#plot(id_fed_name_rich_rst)


# Save the raster
writeRaster(id_fed_name_rich_rst_crop, here::here(paste0("data/processed/fed_rich_id_3km_pred_crop_", 
                                                 Sys.Date(), ".tif")), overwrite = TRUE)


