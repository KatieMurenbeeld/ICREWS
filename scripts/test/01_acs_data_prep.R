library(tidyverse)
library(tidycensus)
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
sub_dir <- "acs"

# check if sub directory exists 
if (file.exists(sub_dir)){
  
} else {
  
  # create a new sub directory inside
  # the data directory
  dir.create(file.path(data_dir, sub_dir))
}

# Set the projection
projection <- "epsg:5070"

# Load the data
acs_house_heat <- read_csv(here::here("data/original/acs/ACSDP5Y2023.DP04-2025-06-10T210233.csv"))
acs_electric_month <- read_csv(here::here("data/original/acs/ACSDT5Y2023.B25132-2025-06-10T210303.csv"))
acs_water_year <- read_csv(here::here("data/original/acs/ACSDT5Y2023.B25134-2025-06-10T210313.csv"))
acs_med_age <- read_csv(here::here("data/original/acs/ACSST5Y2023.S0101-2025-06-10T203350.csv"))
acs_edu <- read_csv(here::here("data/original/acs/ACSST5Y2023.S1501-2025-06-10T203245.csv"))

# Load the reference raster
ref_rast <- rast(here::here("data/processed/wfrc_BP_ID_3km_2024-12-03.tif"))




# Download the county data for Idaho. Set the year to 2023
counties_2023 <- tigris::counties(year = 2023) 
counties_2023 <- counties_2023 %>%
  filter(STATEFP == 16) %>%
  dplyr::select(GEOID, NAME, geometry)
counties_2023 <- st_transform(counties_2023, projection)

counties_2023 <- counties_2023 %>%
  mutate(county_area = st_area(counties_2023))

counties_2023_proj <- st_transform(counties_2023, projection)

## test out using tidycensus

census_api_key("0c67a04e1be4c76930c858ef2c307536f889aff3", install = TRUE)

# find the keys for the variables you want to use from 2023 five year
v23 <- load_variables(2023, "acs5", cache = TRUE)

View(v23)

# test with median age: B01002_001

id_medage <- get_acs(geography = "county", 
              variables = c(medage = "B01002_001"), 
              state = "ID", 
              year = 2023)

id_medage

# test for monthly electricity costs: B25132_001

id_electric_month <- get_acs(geography = "county", 
                             variables = c(total = "B25132_001", 
                                           not_charged = "B25132_002", 
                                           charged = "B25132_003", 
                                           charged_less50 = "B25132_004",
                                           charged_50_100 = "B25132_005",
                                           charged_100_150 = "B25132_006",
                                           charged_150_200 = "B25132_007",
                                           charged_200_250 = "B25132_008",
                                           charged_more250 = "B25132_009"), 
                             state = "ID", 
                             year = 2023)
id_electric_month

# test for annual water and sewer costs: B25134_001

id_water_year <- get_acs(geography = "county", 
                         variables = c(total = "B25134_001", 
                                       not_charged = "B25134_002", 
                                       charged = "B25134_003", 
                                       charged_less125 = "B25134_004",
                                       charged_125_250 = "B25134_005",
                                       charged_250_500 = "B25134_006",
                                       charged_500_750 = "B25134_007",
                                       charged_750_1000 = "B25134_008",
                                       charged_more1000 = "B25134_009"), 
                         state = "ID", 
                         year = 2023)
id_water_year

# test for educational attainment ofr age >25 and older: B15003_001 

id_25yo_edu <- get_acs(geography = "county", 
                         variables = c(total = "B15003_001", 
                                       no_school = "B15003_002", 
                                       nursery = "B15003_003", 
                                       kinder = "B15003_004",
                                       grade_01 = "B15003_005",
                                       grade_02 = "B15003_006",
                                       grade_03 = "B15003_007",
                                       grade_04 = "B15003_008",
                                       grade_05 = "B15003_009",
                                       grade_06 = "B15003_010",
                                       grade_07 = "B15003_011",
                                       grade_08 = "B15003_012",
                                       grade_09 = "B15003_013",
                                       grade_10 = "B15003_014",
                                       grade_11 = "B15003_015",
                                       grade_12_no_dip = "B15003_016",
                                       hs_dip = "B15003_017",
                                       ged = "B15003_018",
                                       coll_less1yr = "B15003_019",
                                       coll_more1yr_no_deg = "B15003_020",
                                       as_deg = "B15003_021",
                                       bs_deg = "B15003_022",
                                       ms_deg = "B15003_023",
                                       profess_deg = "B15003_024",
                                       phd = "B15003_025"), 
                         state = "ID", 
                         year = 2023)
id_25yo_edu

# test for % industry not in ag, forestry, hunting/fishing, and mining:
# C24070

id_industry <- get_acs(geography = "county", 
                             variables = c(total = "C24070_001", 
                                           ag = "C24070_002", 
                                           man = "C24070_004",
                                           ae = "C24070_012"), 
                             state = "ID", 
                             year = 2023)
id_industry


# Need to pivot all the tables wider so that each row is only one county?
# Or do i want to calculate % education level (less highschool) and then pivot wider
# Would need to do a loop and calculate for each GEOID
# Median age is good to go

# Median Age is good to go
medage_proj <- left_join(counties_2023_proj, id_medage,
                           by = "GEOID")
# update the projection
#medage_proj <- st_transform(medage_county, projection)

# Rasterize using the reference raster
medage.rst <- rasterize(medage_proj, ref_rast, field = "estimate", fun = "mean")
nrow(as.data.frame(medage.rst))

# Fill NAs with focal
medage_fill <- focal(medage.rst, 3, mean, na.policy='only', na.rm = TRUE)
plot(medage_fill)
nrow(as.data.frame(medage_fill))

# Crop to reference raster
medage_fill_crop <- crop(medage_fill, ref_rast, mask = TRUE)
plot(medage_fill_crop)
nrow(as.data.frame(ref_rast))
nrow(as.data.frame(medage_fill_crop))

# Save the raster
writeRaster(medage_fill_crop, here::here(paste0("data/processed/median_age_2023_id_3km_crop_", 
                                                Sys.Date(), ".tif")))


## pivot wider, remove margin of error and county name, sum to remove the NAs
# monthly electric costs
id_ele_wide <- id_electric_month %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  select(-moe) %>%
  select(-NAME) %>%
  group_by(GEOID) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE)

id_ele_wide <- id_ele_wide %>%
  mutate(pct_charged_more_250 = (charged_more250 / total) * 100)

# annual water and sewer costs
id_water_wide <- id_water_year %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  select(-moe) %>%
  select(-NAME) %>%
  group_by(GEOID) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE)

id_water_wide <- id_water_wide %>%
  mutate(pct_charged_more_1000 = (charged_more1000 / total) * 100)

# highest level of educational attainment
id_edu_wide <- id_25yo_edu %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  select(-moe) %>%
  select(-NAME) %>%
  group_by(GEOID) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE)

id_edu_wide <- id_edu_wide %>%
  mutate(sum_less_coll = select(., no_school:coll_more1yr_no_deg) %>% rowSums()) %>%
  mutate(pct_less_coll = ((select(., no_school:coll_more1yr_no_deg) %>% rowSums()) / total) * 100)

# industry
id_industry_wide <- id_industry %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  select(-moe) %>%
  select(-NAME) %>%
  group_by(GEOID) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE)

id_industry_wide <- id_industry_wide %>%
  mutate(pct_ag = (ag / total) * 100) %>%
  mutate(pct_ae = (ae / total) * 100) %>%
  mutate(pct_man = (man / total) * 100) %>%
  mutate(pct_all_other = 100 - pct_ag - pct_man - pct_ae) %>%
  mutate(pct_non_ag = 100 - pct_ag) %>%
  mutate(pct_non_man = 100 - pct_man) %>%
  mutate(pct_ag_man = pct_man + pct_ag)


#id_edu_wide <- id_edu_wide %>%
#  mutate(pct_less_coll_test = ((no_school + nursery + kinder + grade_01 + grade_02 + grade_03 + grade_04 + grade_05 + grade_06 + grade_07 + grade_08 + grade_09 + grade_10 + grade_11 + grade_12_no_dip + hs_dip + ged + coll_less1yr + coll_more1yr_no_deg) / total) * 100) %>%
#  mutate(sum_less_coll_test = no_school + nursery + kinder + grade_01 + grade_02 + grade_03 + grade_04 + grade_05 + grade_06 + grade_07 + grade_08 + grade_09 + grade_10 + grade_11 + grade_12_no_dip + hs_dip + ged + coll_less1yr + coll_more1yr_no_deg)

# Join to the county data
id_elec_county <- left_join(counties_2023_proj, id_ele_wide,
                            by = "GEOID")
id_water_county <- left_join(counties_2023_proj, id_water_wide,
                            by = "GEOID")
id_edu_county <- left_join(counties_2023_proj, id_edu_wide,
                           by = "GEOID")
id_industry_county <- left_join(counties_2023_proj, id_industry_wide,
                                by = "GEOID")

# Rasterize, fill NA, crop and save rasters

# Rasterize using the reference raster
id_elec.rst <- rasterize(id_elec_county, ref_rast, field = "pct_charged_more_250", fun = "mean")
nrow(as.data.frame(id_elec.rst))

id_water.rst <- rasterize(id_water_county, ref_rast, field = "pct_charged_more_1000", fun = "mean")
nrow(as.data.frame(id_water.rst))

id_edu.rst <- rasterize(id_edu_county, ref_rast, field = "pct_less_coll", fun = "mean")
nrow(as.data.frame(id_edu.rst))

id_pct_ag.rst <- rasterize(id_industry_county, ref_rast, field = "pct_ag", fun = "mean")
nrow(as.data.frame(id_pct_ag.rst))

id_pct_man.rst <- rasterize(id_industry_county, ref_rast, field = "pct_man", fun = "mean")
nrow(as.data.frame(id_pct_man.rst))

id_pct_ag_man.rst <- rasterize(id_industry_county, ref_rast, field = "pct_ag_man", fun = "mean")
nrow(as.data.frame(id_pct_ag_man.rst))

# Fill NAs with focal
id_elec_fill <- focal(id_elec.rst, 3, mean, na.policy='only', na.rm = TRUE)
plot(id_elec_fill)
nrow(as.data.frame(id_elec_fill))

id_water_fill <- focal(id_water.rst, 3, mean, na.policy='only', na.rm = TRUE)
plot(id_water_fill)
nrow(as.data.frame(id_water_fill))

id_edu_fill <- focal(id_edu.rst, 3, mean, na.policy='only', na.rm = TRUE)
plot(id_edu_fill)
nrow(as.data.frame(id_edu_fill))

id_pct_ag_fill <- focal(id_pct_ag.rst, 3, mean, na.policy='only', na.rm = TRUE)
id_pct_man_fill <- focal(id_pct_man.rst, 3, mean, na.policy='only', na.rm = TRUE)
id_pct_ag_man_fill <- focal(id_pct_ag_man.rst, 3, mean, na.policy='only', na.rm = TRUE)


# Crop to reference raster
id_elec_fill_crop <- crop(id_elec_fill, ref_rast, mask = TRUE)
plot(id_elec_fill_crop)
nrow(as.data.frame(ref_rast))
nrow(as.data.frame(id_elec_fill_crop))

id_water_fill_crop <- crop(id_water_fill, ref_rast, mask = TRUE)
plot(id_water_fill_crop)
nrow(as.data.frame(ref_rast))
nrow(as.data.frame(id_water_fill_crop))

id_edu_fill_crop <- crop(id_edu_fill, ref_rast, mask = TRUE)
plot(id_edu_fill_crop)
nrow(as.data.frame(ref_rast))
nrow(as.data.frame(id_edu_fill_crop))

id_pct_ag_fill_crop <- crop(id_pct_ag_fill, ref_rast, mask = TRUE)
plot(id_pct_ag_fill_crop)
nrow(as.data.frame(ref_rast))
nrow(as.data.frame(id_pct_ag_fill_crop))

id_pct_man_fill_crop <- crop(id_pct_man_fill, ref_rast, mask = TRUE)
plot(id_pct_man_fill_crop)
nrow(as.data.frame(ref_rast))
nrow(as.data.frame(id_pct_man_fill_crop))

id_pct_ag_man_fill_crop <- crop(id_pct_ag_man_fill, ref_rast, mask = TRUE)
plot(id_pct_ag_man_fill_crop)
nrow(as.data.frame(ref_rast))
nrow(as.data.frame(id_pct_ag_man_fill_crop))

# Save the raster
writeRaster(id_elec_fill_crop, here::here(paste0("data/processed/pct_charged_more_250_month_electricity_2023_id_3km_crop_", 
                                                Sys.Date(), ".tif")))

writeRaster(id_water_fill_crop, here::here(paste0("data/processed/pct_charged_more_1000_year_water_sewer_2023_id_3km_crop_", 
                                                 Sys.Date(), ".tif")))

writeRaster(id_edu_fill_crop, here::here(paste0("data/processed/pct_over25yo_less_college_2023_id_3km_crop_", 
                                                  Sys.Date(), ".tif")))

writeRaster(id_pct_ag_fill_crop, here::here(paste0("data/processed/pct_ag_industry_2023_id_3km_crop_", 
                                                Sys.Date(), ".tif")))

writeRaster(id_pct_man_fill_crop, here::here(paste0("data/processed/pct_man_industry_2023_id_3km_crop_", 
                                                   Sys.Date(), ".tif")))

writeRaster(id_pct_ag_man_fill_crop, here::here(paste0("data/processed/pct_ag_man_industry_2023_id_3km_crop_", 
                                                    Sys.Date(), ".tif")))

















