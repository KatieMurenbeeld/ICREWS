library(tidyverse)
library(sf)
library(terra)
library(tigris)
library(stringr)
library(RCurl)
library(stars)
library(spdep)
library(gstat)
# Set timeout to 10 minutes
options(timeout=6000)

# Create a new subdirectory for the data in the data/original directory
data_dir <- here::here("data/original/")
sub_dir <- "cejst"

# check if sub directory exists 
if (file.exists(sub_dir)){
  
} else {
  
  # create a new sub directory inside
  # the data directory
  dir.create(file.path(data_dir, sub_dir))
}

# download and unzip the data into the new subdirectory (for next time)
# update this to detect .zip in the file name as well
download_data <- function(url, file_name) {
  if (str_detect(url, ".zip")) {
    tmp <- tempfile()
    download.file(url, tmp)
    unzip(zipfile = tmp, exdir = here::here("data/original/"))
  } else if (str_detect(file_name, ".zip")) {
    download.file(url, here::here(paste0("data/original/", file_name)))
    unzip(zipfile = here::here(paste0("data/original/", file_name)))
  }
  else {
    download.file(url, here::here(paste0("data/original/", file_name)))
  }
}

# Climate and Economic Justice Screening Tool
url <- "https://static-data-screeningtool.geoplatform.gov/data-versions/2.0/data/score/downloadable/2.0-shapefile-codebook.zip"
file_name <- ""

# NOTE TO SELF: Need to fix the download data function
download_data(url, file_name)
unzip(here::here("data/original/usa.zip"), exdir = here::here("data/original/cejst/"))

# read in the cejst shapefile
cejst <- read_sf(here::here("data/original/cejst/usa.shp"))

# Filter for Idaho
filt_cejst <- cejst %>%
  filter(SF == "Idaho")

## Select variables: less high school, housing burden, energy burden, and PM2.5
cejst_vars <- filt_cejst %>% 
  dplyr::select(geometry, HSEF, HBF_PFS, EBF_PFS, PM25F_PFS, EALR_PFS)

## check for validity, remove empty geometries, and reproject 
if (!all(st_is_valid(cejst_vars)))
  cejst_vars <- st_make_valid(cejst_vars)

cejst_vars <- cejst_vars %>%
  filter(!st_is_empty(.))

projection <- "epsg:5070"

cejst_vars_proj <- cejst_vars %>%
  st_transform(projection)

## Load county boundaries from tigris
### Set the year to account for changes to FIPS codes
counties_2020 <- tigris::counties(year = 2020) 
### Filter for Idaho (FIPS = 16)
counties_2020 <- counties_2020 %>%
  filter(STATEFP == 16) %>%
  dplyr::select(GEOID, geometry)

counties_2020 <- st_transform(counties_2020, projection)

## Create a template raster for the shapefiles
# use the Wildfire data as the reference raster
ref_rast <- rast(here::here("data/processed/wfrc_BP_ID_3km_2024-12-03.tif"))

XMIN <- ext(ref_rast)$xmin
XMAX <- ext(ref_rast)$xmax
YMIN <- ext(ref_rast)$ymin
YMAX <- ext(ref_rast)$ymax
aspectRatio <- (YMAX-YMIN)/(XMAX-XMIN)
cellSize <- 3000
NCOLS <- as.integer((XMAX-XMIN)/cellSize)
NROWS <- as.integer(NCOLS * aspectRatio)
templateRas <- rast(ncol=NCOLS, nrow=NROWS, 
                    xmin=XMIN, xmax=XMAX, ymin=YMIN, ymax=YMAX,
                    vals=1, crs=crs(ref_rast))

grd <- st_as_stars(templateRas)

# function to rasterize variable, make points, make predictions
# raster_grid is like a reference raster
idw_preds <- function(data_proj, ref_raster, lay, empty_grid){
  var.rst <- rasterize(data_proj, ref_raster, field = lay, fun = "mean")
  var.pt <- as.points(var.rst) %>%
    st_as_sf(.)
  var.pred <- idw(var.pt[[1]]~1, var.pt, empty_grid)
  var.pred.rst <- rasterize(st_as_sf(var.pred), ref_raster, field = "var1.pred")
  names(var.pred.rst) <- paste0(lay, ".pred")
  return(c(orig.rst = var.rst, pred.rst = var.pred.rst))
}

# Less high school
lesshs.preds <- idw_preds(cejst_vars_proj, templateRas, "HSEF", grd)
plot(lesshs.preds$orig.rst)
plot(lesshs.preds$pred.rst)
lesshs_crop <- crop(lesshs.preds$pred.rst, ref_rast, mask = TRUE)
plot(lesshs_crop)
#nrow(as.data.frame(lesshs_crop))
#nrow(as.data.frame(ref_rast))
writeRaster(x = lesshs_crop, here::here(paste0("data/processed/cejst_lesshs_id_3km_pred_crop_", 
                                                          Sys.Date(), ".tif")), overwrite = TRUE)

# Housing burden 
hsburd.preds <- idw_preds(cejst_vars_proj, templateRas, "HBF_PFS", grd)
plot(hsburd.preds$orig.rst)
houseburd_crop <- crop(hsburd.preds$pred.rst, ref_rast, mask = TRUE)
plot(houseburd_crop)
nrow(as.data.frame(houseburd_crop))
writeRaster(houseburd_crop, here::here(paste0("data/processed/cejst_hsbrd_id_3km_pred_crop_", 
                                           Sys.Date(), ".tif")), overwrite = TRUE)

# Energy burden 
engburd.preds <- idw_preds(cejst_vars_proj, templateRas, "EBF_PFS", grd)
plot(engburd.preds$orig.rst)
engburd_crop <- crop(engburd.preds$pred.rst, ref_rast, mask = TRUE)
plot(engburd_crop)
nrow(as.data.frame(engburd_crop))
writeRaster(engburd_crop, here::here(paste0("data/processed/cejst_engbrd_id_3km_pred_crop_", 
                                              Sys.Date(), ".tif")), overwrite = TRUE)

# PM2.5 exposure 
pm25.preds <- idw_preds(cejst_vars_proj, templateRas, "PM25F_PFS", grd)
plot(pm25.preds$orig.rst)
pm25_crop <- crop(pm25.preds$pred.rst, ref_rast, mask = TRUE)
plot(pm25_crop)
nrow(as.data.frame(pm25_crop))
writeRaster(pm25_crop, here::here(paste0("data/processed/cejst_pm25_id_3km_pred_crop_", 
                                            Sys.Date(), ".tif")), overwrite = TRUE)

# Expected agricultural loss rate 
ealr.preds <- idw_preds(cejst_vars_proj, templateRas, "EALR_PFS", grd)
plot(ealr.preds$orig.rst)
ealr_crop <- crop(ealr.preds$pred.rst, ref_rast, mask = TRUE)
plot(ealr_crop)
nrow(as.data.frame(ealr_crop))
writeRaster(ealr_crop, here::here(paste0("data/processed/cejst_ealr_id_3km_pred_crop_", 
                                         Sys.Date(), ".tif")), overwrite = TRUE)

#test_rst <- raster(here::here("data/processed/cejst_ealr_id_3km_pred_crop_2025-01-06.tif"))
#test_rst
