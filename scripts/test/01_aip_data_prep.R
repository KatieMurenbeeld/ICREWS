library(stringr)
library(sf)
library(terra)
library(tidyverse)
library(haven)
library(tigris)
library(readxl)
library(spdep)
library(gstat)
library(stars)

# Set timeout to 10 minutes
options(timeout=6000)

# Create a new subdirectory for the data in the data/original directory
data_dir <- here::here("data/original/")
sub_dir <- "aip_files"

# check if sub directory exists 
if (file.exists(sub_dir)){
  
} else {
  
  # create a new sub directory inside
  # the data directory
  dir.create(file.path(data_dir, sub_dir))
}

# Download transmission line data from EIA
# https://atlas.eia.gov/search?q=transmission

# Load the data
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/BQKU4M
aip <- read_dta("/Users/katiemurenbeeld/Analysis/ICREWS/icrews_analysis/data/original/aip_files/aip_counties_ideology_v2022a.dta")

# Load the reference raster and reproject
ref_rast <- rast(here::here("data/processed/wfrc_BP_ID_3km_2024-12-03.tif"))
ref_rast_proj <- project(ref_rast, projection)

## Load county boundaries from tigris
### Set the year to account for changes to FIPS codes
counties_2020 <- tigris::counties(year = 2020) 
counties_2020 <- counties_2020 %>%
  filter(STATEFP == 16) %>%
  dplyr::select(GEOID, geometry)
counties_2020 <- st_transform(counties_2020, projection)

## American Ideology Data
aip_mrp <- aip %>% # needs 2020 counties
  filter(survey_period == "2017-2021") %>%
  dplyr::select(county_fips, mrp_ideology, mrp_ideology_se, demshare_pres) %>%
  rename("FIPS" = "county_fips")

## Make sure all FIPS codes are padded with 0s for a total of 5 characters
update_fips <- function(data_set) {
  data_set$FIPS <- as.character(data_set$FIPS)
  data_set$FIPS <- str_pad(data_set$FIPS, 5, side="left", pad="0")
  return(data_set)
}

aip_fips <- update_fips(aip_mrp)

## join to the 2020 counties
aip_county <- left_join(counties_2020, aip_fips,
                        by = c("GEOID" = "FIPS"))

## check for validity, remove empty geometries, and reproject 
if (!all(st_is_valid(aip_county)))
  aip_county <- st_make_valid(aip_county)

aip_county <- aip_county %>%
  filter(!st_is_empty(.))

aip_proj <- aip_county %>%
  st_transform(projection)

## Create a template raster for the shapefiles
XMIN <- ext(ref_rast_proj)$xmin
XMAX <- ext(ref_rast_proj)$xmax
YMIN <- ext(ref_rast_proj)$ymin
YMAX <- ext(ref_rast_proj)$ymax
aspectRatio <- (YMAX-YMIN)/(XMAX-XMIN)
cellSize <- 3000
NCOLS <- as.integer((XMAX-XMIN)/cellSize)
NROWS <- as.integer(NCOLS * aspectRatio)
templateRas <- rast(ncol=NCOLS, nrow=NROWS, 
                    xmin=XMIN, xmax=XMAX, ymin=YMIN, ymax=YMAX,
                    vals=1, crs=crs(ref_rast_proj))

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
  #var.pred.rst.crop <- crop(var.pred.rst, ref_raster, mask = TRUE)
  #return(var.pred.rst)
  return(c(orig.rst = var.rst, pred.rst = var.pred.rst))
}

aip.preds <- idw_preds(aip_proj, templateRas, "mrp_ideology", grd)
plot(aip.preds$orig.rst)

## Crop to reference raster
aip_crop <- crop(aip.preds$pred.rst, ref_rast_proj, mask = TRUE)
plot(aip_crop)
nrow(as.data.frame(aip_crop$mrp_ideology.pred))

# Save the raster
writeRaster(aip_crop, here::here(paste0("data/processed/aip_id_3km_pred_crop_", 
                                        Sys.Date(), ".tif")), overwrite = TRUE)
