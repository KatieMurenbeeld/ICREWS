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
sub_dir <- "infrastructure"

# check if sub directory exists 
if (file.exists(sub_dir)){
  
} else {
  
  # create a new sub directory inside
  # the data directory
  dir.create(file.path(data_dir, sub_dir))
}

# Road density (from TIGRIS)
# Download the road data for Idaho. Set the year to 2023
# could do all roads - would have to loop through counties 
# or could do distance to primary and secondary roads? 
roads_2023 <- tigris::primary_secondary_roads("Idaho", year = "2023") 
roads_proj <- st_transform(roads_2023, projection)

gg <- ggplot()
gg <- gg + geom_sf(data = roads_proj,
                   color="black", fill="white", size=0.25)
gg

rails_2023 <- tigris::rails(year = "2023") 
rails_proj <- st_transform(rails_2023, projection)

gg <- ggplot()
gg <- gg + geom_sf(data = rails_2023,
                   color="black", fill="white", size=0.25)
gg <- gg + theme_map()
gg

# Download the county data for Idaho. Set the year to 2023
counties_2023 <- tigris::counties(year = 2023) 
counties_2023 <- counties_2023 %>%
  filter(STATEFP == 16) %>%
  dplyr::select(GEOID, geometry)
counties_2023 <- st_transform(counties_2023, projection)

# I need where the rail and Idaho counties intersect
id_rails <- st_intersection(rails_proj, counties_2023) 

gg <- ggplot()
gg <- gg + geom_sf(data = id_rails,
                   color="black", fill="white", size=0.25)
gg
