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

union_test <- st_union(fed_proj_name, by_feature = TRUE)

fed_names <- unique(fed_proj_name$Mang_Name)

union_function <- function(sf, name){
  tmp <- sf %>%
    filter(Mang_Name == name)
  tmp_union <- st_as_sf(st_union(tmp)) %>%
    mutate(fed_name = name)
}

test_union <- union_function(fed_proj_name, "BLM")
plot(st_geometry(test_union))
plot(st_geometry(blm_union))

blm_union <- union_function(fed_proj_name, "BLM")
doe_union <- union_function(fed_proj_name, "DOE")
usbr_union <- union_function(fed_proj_name, "USBR")
usfs_union <- union_function(fed_proj_name, "USFS")
nps_union <- union_function(fed_proj_name, "NPS")
fws_union <- union_function(fed_proj_name, "FWS")

fed_name_union <- bind_rows(blm_union, doe_union, usbr_union, usfs_union, nps_union, fws_union)

blm <- union_test %>%
  filter(Mang_Name == "BLM")
plot(st_geometry(id_proj))
plot(st_geometry(blm), add = TRUE)
blm_union <- st_as_sf(st_union(blm)) %>%
  mutate(fed_name = "BLM")

doe <- fed_proj_name %>%
  filter(Mang_Name == "DOE")
plot(st_geometry(id_proj))
plot(st_geometry(doe), add = TRUE)

usbr <- fed_proj_name %>%
  filter(Mang_Name == "USBR")
plot(st_geometry(id_proj))
plot(st_geometry(usbr), add = TRUE)

usfs <- union_test %>%
  filter(Mang_Name == "USFS")
plot(st_geometry(usfs))
plot(st_geometry(id_proj), add = TRUE)

usfs_union <- st_as_sf(st_union(usfs)) %>%
  mutate(fed_name = "USFS")
st_is_valid(usfs_union)
plot(st_geometry(usfs_union), col = "blue")
plot(st_geometry(id_proj), add = TRUE)

nps <- fed_proj_name %>%
  filter(Mang_Name == "NPS")
plot(st_geometry(id_proj))
plot(st_geometry(nps), add = TRUE)

nps_union <- st_as_sf(st_union(nps)) %>%
  mutate(fed_name = "NPS")
plot(st_geometry(id_proj))
plot(st_geometry(nps_union), col = "red", add = TRUE)

fws <- fed_proj_name %>%
  filter(Mang_Name == "FWS")
plot(st_geometry(id_proj))
plot(st_geometry(fws), add = TRUE)

test_sf <- bind_rows(usfs_union, nps_union)

plot(st_geometry(test_sf))

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
  
#id_fed_name_int <- st_intersection(id_cells_sf, fed_proj_name)
id_fed_name_int <- st_intersection(id_cells_sf, fed_name_union)
id_fed_name_rich <- id_fed_name_int %>%
  group_by(., GRIDCELL_REFID) %>%
  summarise(., numfed = n())
  #summarise(., numfed = count(., as.factor(fed_name), .drop = FALSE))

id_fed_name_rich_rst <- rasterize(id_fed_name_rich, id_cells_rst, field = "numfed")

plot(id_fed_name_rich_rst)
plot(st_geometry(id_proj), add = TRUE)
plot(st_geometry(usfs_union), col = "red", add = TRUE)
plot(st_geometry(nps_union),  col = "green", add = TRUE)
plot(st_geometry(fws_union),  col = "blue", add = TRUE)
plot(st_geometry(blm_union),  col = "blue", add = TRUE)
plot(st_geometry(doe_union),  col = "yellow", add = TRUE)
plot(st_geometry(usbr_union),  col = "orange", add = TRUE)

# That all seems legit, so now I want to fill in na with 0 and save the raster
id_fed_name_rich_rst[is.na(id_fed_name_rich_rst[])] <- 0 
plot(id_fed_name_rich_rst)

# crop to ref raster
## have to resample first
ref_rast <- rast(here::here("data/processed/wfrc_BP_ID_3km_2024-12-03.tif"))
id_fed_name_rich_rst_resamp <- resample(id_fed_name_rich_rst, ref_rast, method = "near")
plot(id_fed_name_rich_rst_resamp)

id_fed_name_rich_rst_crop <- crop(id_fed_name_rich_rst_resamp, ref_rast, mask = TRUE)
plot(id_fed_name_rich_rst_crop)
plot(id_fed_name_rich_rst)


# Save the raster
writeRaster(id_fed_name_rich_rst_crop, here::here(paste0("data/processed/fed_rich_id_3km_pred_crop_", 
                                                 Sys.Date(), ".tif")), overwrite = TRUE)


#----
pb2 <- progress_bar$new(format = "[:bar] :current/:total (:percent)", total = dim(id_cells_sf)[1])

intersectFeatures_rich <- map_dfr(1:dim(id_cells_sf)[1], function(ix){
  pb2$tick()
  st_intersection(x = id_cells_sf[ix,], y = fed_proj_name[id_fed_name_int[[ix]],])
})

hd_intersectFeatures_rich <- head(intersectFeatures_rich, n = 79L)

intersectFeatures_richness <- intersectFeatures_rich %>%
  group_by(., GRIDCELL_REFID) %>%
  summarise(., numfed = n())

identical(nc_fed_name_rich, intersectFeatures_richness)
identical(nc_fed_name_rich$numfed, intersectFeatures_richness$numfed)

intersectFeatures_rich_rst <- rasterize(intersectFeatures_richness, nc_cells_rst, field = "numfed")

plot(intersectFeatures_rich_rst)
plot(st_geometry(nc_proj), add = TRUE)


identical(nc_fed_name_rich_rst, intersectFeatures_rich_rst)
