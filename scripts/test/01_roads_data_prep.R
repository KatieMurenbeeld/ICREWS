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
library(raster)

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
  filter(STUSPS %in% c("ID", "WA", "MT", "OR", "NV", "UT", "WY"))

# I need where the roads and NW states intersect
id_roads <- tigris::primary_secondary_roads(state = "ID", year = "2023")
wa_roads <- tigris::primary_secondary_roads(state = "WA", year = "2023")
mt_roads <- tigris::primary_secondary_roads(state = "MT", year = "2023")
or_roads <- tigris::primary_secondary_roads(state = "OR", year = "2023")
nv_roads <- tigris::primary_secondary_roads(state = "NV", year = "2023")
ut_roads <- tigris::primary_secondary_roads(state = "UT", year = "2023")
wy_roads <- tigris::primary_secondary_roads(state = "WY", year = "2023")
nw_roads <- rbind(id_roads, wa_roads, mt_roads, or_roads, nv_roads, ut_roads, wy_roads) 

id_all_roads <- tigris::roads(state = "ID", county = "Ada County")
id_all_roads_proj <- st_transform(id_all_roads, projection)

gg <- ggplot()
gg <- gg + geom_sf(data = id_roads,
                   color="black", fill="white", size=0.25)
gg

test <- rbind(id_roads, wa_roads, mt_roads)

gg2 <- ggplot()
gg2 <- gg2 + geom_sf(data = nw_roads, 
                     color = "black", fill = "white", size = 0.25)
gg2

# Create a template raster for the shapefiles
XMIN <- ext(id_all_roads_proj)$xmin
XMAX <- ext(id_all_roads_proj)$xmax
YMIN <- ext(id_all_roads_proj)$ymin
YMAX <- ext(id_all_roads_proj)$ymax
aspectRatio <- (YMAX-YMIN)/(XMAX-XMIN)
cellSize <- 3000
NCOLS <- as.integer((XMAX-XMIN)/cellSize)
NROWS <- as.integer(NCOLS * aspectRatio)
templateRas <- rast(ncol=NCOLS, nrow=NROWS, 
                    xmin=XMIN, xmax=XMAX, ymin=YMIN, ymax=YMAX,
                    vals=1, crs=crs(id_all_roads_proj))


# reproject the roads
id_roads_proj <- st_transform(id_roads, projection)
nw_roads_proj <- st_transform(nw_roads, projection)



v <- vect(id_all_roads_proj)
#v <- vect(nw_roads_proj)
v
plot(v)
roads <- as.lines(v)
roads
plot(roads)
road_rast <- rasterizeGeom(roads, templateRas, fun = "crosses", unit = "km")
plot(road_rast)

test_rast <- rasterize(vect(id_all_roads_proj), templateRas)
#test_rast <- rasterize(vect(nw_roads_proj), templateRas)
test_rast_dist <- terra::distance(test_rast)
plot(test_rast_dist)

# Calculate the distance from primary roads in the northwest
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



library(terra)
v <- vect(system.file("ex/lux.shp", package="terra"))
v
roads <- as.lines(v)
roads
plot(roads)
rs <- rast(v)
plot(rs)
rs
