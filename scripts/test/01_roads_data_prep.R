library(tidyverse)
library(raster)
library(sf)
library(terra)
library(tigris)
library(stringr)
library(RCurl)
library(stars)
library(spdep)
library(gstat)
library(spatstat)

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

# Set the projection
projection <- "epsg:5070"

# Road density (from TIGRIS)
# Download the road data for Idaho. Set the year to 2023
# could do all roads - would have to loop through counties 
# or could do distance to primary and secondary roads? 
roads_2023 <- tigris::primary_secondary_roads("Idaho", year = "2023") 
roads_proj <- st_transform(roads_2023, projection)

gg <- ggplot()
gg <- gg + geom_sf(data = nw_roads,
                   color="black", fill="white", size=0.25)
gg

rails_2023 <- tigris::rails(year = "2023") 
rails_proj <- st_transform(rails_2023, projection)

gg <- ggplot()
gg <- gg + geom_sf(data = rails_2023,
                   color="black", fill="white", size=0.25)

gg

# Download the county data for Idaho. Set the year to 2023
counties_2023 <- tigris::counties(year = 2023) 
counties_2023 <- counties_2023 %>%
  filter(STATEFP == 16) %>%
  dplyr::select(GEOID, geometry)
counties_2023 <- st_transform(counties_2023, projection)

## get the state boundaries
states <- tigris::states()

## reproject
states_proj <- st_transform(states, crs = projection)

## get the idaho boundary
id_bdry <- states_proj %>%
  filter(STUSPS == "ID")

## get the boundary for states neighboring Idaho 
## we will use the NW states to calculate the distance to energy sources
nw_states <- states_proj %>%
  filter(STUSPS %in% c("ID", "WA", "MT", "OR", "NV", "UT", "WY", "CA"))

# I need where the roads and NW states intersect
nw_roads <- tigris::primary_secondary_roads(state = , year = "2023")
#nw_roads <- st_intersection(roads_proj, nw_states) 

gg <- ggplot()
gg <- gg + geom_sf(data = nw_roads,
                   color="black", fill="white", size=0.25)
gg

# Create a template raster for the shapefiles
XMIN <- ext(nw_states)$xmin
XMAX <- ext(nw_states)$xmax
YMIN <- ext(nw_states)$ymin
YMAX <- ext(nw_states)$ymax
aspectRatio <- (YMAX-YMIN)/(XMAX-XMIN)
cellSize <- 3000
NCOLS <- as.integer((XMAX-XMIN)/cellSize)
NROWS <- as.integer(NCOLS * aspectRatio)
templateRas <- rast(ncol=NCOLS, nrow=NROWS, 
                    xmin=XMIN, xmax=XMAX, ymin=YMIN, ymax=YMAX,
                    vals=1, crs=crs(nw_states))

# Rasterize the roads shapefile
nw_road_rast <- rasterize(vect(nw_roads$geometry), templateRas)

# Calculate the distance from power plants
nwrd_dist_rast <- terra::distance(nw_road_rast)


plot(nwrd_dist_rast)

# I need where the rail and Idaho counties intersect
nw_rails <- st_intersection(rails_proj, nw_states) 

gg <- ggplot()
gg <- gg + geom_sf(data = nw_rails,
                   color="black", fill="white", size=0.25)
gg


# Rasterize the roads shapefile
nw_rail_rast <- rasterize(vect(nw_rails$geometry), templateRas)

# Calculate the distance from rails
nwrl_dist_rast <- terra::distance(nw_rail_rast)

plot(nwrl_dist_rast)

psp_rail <- as.psp(nw_roads)
