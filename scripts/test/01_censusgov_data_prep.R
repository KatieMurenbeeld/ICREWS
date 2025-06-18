library(tidyverse)
library(tidycensus)
library(sf)
library(terra)
library(raster)
library(ncdf4)
library(tigris)
library(stringr)
library(RCurl)
library(stars)
library(spdep)
library(gstat)
library(readxl)

# set the projection
projection <- "epsg:5070"
# Load the reference raster
ref_rast <- rast(here::here("data/processed/wfrc_BP_ID_3km_2024-12-03.tif"))

#census_api_key("0c67a04e1be4c76930c858ef2c307536f889aff3", install = TRUE)

# find the keys for the variables you want to use from 2023 five year
v23 <- load_variables(2023, "acs5", cache = TRUE)

View(v23)

# Load in the appropriate sheets from the Government Units 2022 Final 

gen_purp <- as.data.frame(read_excel(here::here("data/original/census/govt_units_2022/Govt_Units_2022_Final.xlsx"),
                       sheet = "General Purpose"))
spec_dist <- as.data.frame(read_excel(here::here("data/original/census/govt_units_2022/Govt_Units_2022_Final.xlsx"),
                        sheet = "Special District"))
school_dist <- as.data.frame(read_excel(here::here("data/original/census/govt_units_2022/Govt_Units_2022_Final.xlsx"),
                          sheet = "School District"))

gen_purp_id <- gen_purp %>%
  filter(STATE == "ID") %>%
  mutate(FIPS = paste(FIPS_STATE, FIPS_COUNTY, sep = "")) %>%
  dplyr::select(FIPS, COUNTY_AREA_NAME, UNIT_NAME, UNIT_TYPE)
  
spec_dist_id <- spec_dist %>%
  filter(STATE == "ID") %>%
  mutate(FIPS = paste(FIPS_STATE, FIPS_COUNTY, sep = "")) %>%
  dplyr::select(FIPS, COUNTY_AREA_NAME, UNIT_NAME, FUNCTION_NAME)

sch_dist_id <- school_dist %>%
  filter(STATE == "ID") %>%
  mutate(FIPS = paste(FIPS_STATE, FIPS_COUNTY, sep = "")) %>%
  dplyr::select(FIPS, COUNTY_AREA_NAME, UNIT_NAME)

# Find the number of unique jurisdictions per county
gen_purp_county <- gen_purp_id %>% 
  group_by(FIPS) %>%
  summarise(num_gen = n())

spec_dist_county <- spec_dist_id %>% 
  group_by(FIPS) %>%
  summarise(num_spec = n())

sch_dist_county <- sch_dist_id %>% 
  group_by(FIPS) %>%
  summarise(num_sch = n())

# join all the results together
gov_df <- left_join(gen_purp_county, spec_dist_county)
gov_df <- left_join(gov_df, sch_dist_county)  

gov_df <- gov_df %>%
  mutate(total_gov = num_gen + num_spec + num_sch)

# Get the county population using tidycensus
county_pop <- get_acs(
  geography = "county", 
  variables = "B01003_001",
  year = 2022,
  state = "ID"
) %>%
  dplyr::select(GEOID, estimate)

gov_df_pop <- left_join(gov_df, county_pop, by = c("FIPS" = "GEOID"))  

gov_10k <- gov_df_pop %>%
  mutate(gov_10k = (total_gov / estimate) * 10000)

# Download the county data for Idaho. Set the year to 2022
counties_2022 <- tigris::counties(year = 2022) 
counties_2022 <- counties_2022 %>%
  filter(STATEFP == 16) %>%
  dplyr::select(GEOID, NAME, geometry)
counties_2022 <- st_transform(counties_2022, projection)

counties_2022 <- counties_2022 %>%
  mutate(county_area = st_area(counties_2022))

counties_2022_proj <- st_transform(counties_2022, projection)

# join to gov_10k
gov_10k_proj <- left_join(counties_2022_proj, gov_10k, by = c("GEOID" = "FIPS")) 

# Rasterize using the reference raster
gov_10k.rst <- rasterize(gov_10k_proj, ref_rast, field = "gov_10k", fun = "mean")
nrow(as.data.frame(gov_10k.rst))

# Fill NAs with focal
gov_10k_fill <- focal(gov_10k.rst, 3, mean, na.policy='only', na.rm = TRUE)
plot(gov_10k_fill)
nrow(as.data.frame(gov_10k_fill))

# Crop to reference raster
gov_10k_fill_crop <- crop(gov_10k_fill, ref_rast, mask = TRUE)
plot(gov_10k_fill_crop)
nrow(as.data.frame(ref_rast))
nrow(as.data.frame(gov_10k_fill_crop))

# Save the raster
writeRaster(gov_10k_fill_crop, here::here(paste0("data/processed/num_gov_10k_2022_id_3km_crop_", 
                                                 Sys.Date(), ".tif")))




