library(tidyverse)
library(terra)
library(sf)
library(ggplot2)
library(exactextractr)
library(tigris)

# Load the cluster results
sgfcm_k8_result <- rast(here::here("outputs/models/sgfcm_result_k8_2025-01-08.tif"))

# Need a shapefile of Idaho
## get the state boundaries
states <- tigris::states()

## reproject
states_proj <- st_transform(states, crs = crs(sgfcm_k8_result))

## get the idaho boundary
id_bdry <- states_proj %>%
  filter(STUSPS == "ID")

# Use exact extractr to get the proportion of archetypes in the state
v <- id_bdry %>% st_cast("MULTIPOLYGON")
z_k8 <- crop(sgfcm_k8_result, v, mask = TRUE)

x_k8 <- exact_extract(z_k8, v, coverage_area = TRUE)

areas_k8 <- bind_rows(x_k8) %>%
  group_by(value) %>%
  summarize(total_arch_area = sum(coverage_area)) %>%
  mutate(proportion_pct = round((total_arch_area/sum(total_arch_area))*100, 2)) 


