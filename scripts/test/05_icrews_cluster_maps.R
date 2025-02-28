library(tidyverse)
library(terra)
library(sf)
library(sp)
library(nngeo)
library(ggplot2)
library(patchwork)
library(geocmeans)
library(RColorBrewer)
library(viridis)
library(ggpattern)
library(distributional)
library(ggdist)
library(ggsci)
library(tigris)
library(MetBrewer)
library(cowplot)


#---set the projection----
projection = "epsg:5070"

#----Load the data----
#gfcm_k3_result <- rast(here::here("outputs/models/gfcm_result_k3_2025-01-08.tif"))
sgfcm_k8_result <- rast(here::here("outputs/models/sgfcm_result_k8_2025-01-08.tif"))
# think about other data to add, cities? rivers? "test bed" sites?

#plot(gfcm_k3_result)
plot(sgfcm_k8_result)

#gfcm_result_k3_mod <- readRDS(here::here("outputs/models/gfcm_mod_k3_2025-01-08.rds"))
sgfcm_result_k8_mod <- readRDS(here::here("outputs/models/sgfcm_mod_k8_2025-01-08.rds"))

## load additional shapefiles
## Idaho
## get the state boundaries
states <- tigris::states()
states_proj <- st_transform(states, crs = projection)
id_bdry <- states_proj %>%
  filter(STUSPS == "ID")

### tribal reservations
res <- tigris::native_areas()

### urban areas
urban <- tigris::urban_areas(year = 2020)

### cities (Driggs, McCall, CDA, Twin Falls, Boise)
cities_df <- data.frame(name = c("Driggs", "McCall", "CDA", "Twin Falls", "Boise"),
                        latitude = c(43.7231, 44.9062, 47.6735, 42.5558, 43.6150),
                        longitude = c(-111.1111, -116.1171, -116.7812, -114.4701, -116.2023)) 
cities <- st_as_sf(cities_df,
                   coords = c("longitude", "latitude"), 
                   crs = 4326)

plot(cities$geometry)

### metro areas (for Treasure Valley i.e. Boise Metropolitan Area (MSA))
metro <- tigris::core_based_statistical_areas(year = 2020)

### treasure valley 
# https://data-idwr.hub.arcgis.com/datasets/2015-irrigated-lands-for-the-treasure-valley-hand-digitized-generated/explore?location=43.621887%2C-116.473674%2C8.44
tva <- read_sf(here::here("data/original/2015__Irrigated_Lands_for_the_Treasure_Valley/2015__Irrigated_Lands_for_the_Treasure_Valley.shp"))

### Idaho water data
#url <- "https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHD/State/Shape/NHD_H_Idaho_State_Shape.zip"
watershed <- read_sf(here::here("data/original/water/NHD_H_Idaho_State_Shape (1)/Shape/WBDHU4.shp"))
rivers <- read_sf(here::here("data/original/water/NHD_H_Idaho_State_Shape (1)/Shape/NHDFlowline_0.shp"))

## Reproject the shapes to NAD83 and filter for Idaho

res_proj <- st_transform(res, projection)
res_proj <- res_proj %>%
  filter(GEOID == "0705R" | GEOID == "1185R" | GEOID == "1185T")

urban_proj <- st_transform(urban, projection)
urban_proj <- urban_proj %>%
  filter(GEOID10 == "89245" | GEOID10 == "08785")

cities_proj <- st_transform(cities, projection)

metro_proj <- st_transform(metro, projection)
boise_msa <- metro_proj %>%
  filter(GEOID == "14260")

tva_proj <- st_transform(tva, projection)
tva_valid <- st_make_valid(tva_proj)
tva_combine <- st_combine(tva_valid)
tva_comb_union <- st_union(tva_combine, by_feature = TRUE)
tva_noholes <- st_as_sf(st_remove_holes(tva_comb_union)) 

watershed_proj <- st_transform(watershed, projection)
watershed_proj <- watershed_proj %>% 
  filter(huc4 == "1701" | huc4 == "1704" | huc4 == "1705" | huc4 == "1706")

rivers_proj <- st_transform(rivers, projection)
rivers_proj <- rivers_proj %>%
  filter(gnis_id == "01533479")

# and crop to Idaho
res_crop <- st_crop(res_proj, id_bdry, mask = TRUE)

watershed_crop <- st_intersection(watershed_proj, id_bdry, mask = TRUE)

rivers_proj <- st_zm(rivers_proj) 
rivers_crop <- st_intersection(rivers_proj, id_bdry, mask = TRUE)

urban_crop <- st_intersection(urban_proj, id_bdry, mask = TRUE)

tva_crop <- st_intersection(tva_noholes, id_bdry, mask = TRUE)
## Create a map of the clusters with watersheds, rivers, and locations
# k = 8

sgfcm.k8.df <- sgfcm_k8_result$Groups %>% as.data.frame(xy = TRUE)

# create a wrapper function to wrap the plot title text
wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}

k8_map <- ggplot() +
  geom_raster(aes(x = sgfcm.k8.df$x, y = sgfcm.k8.df$y, fill = as.factor(sgfcm.k8.df$Groups))) +
  geom_sf(data = rivers_crop, fill = NA, color = "skyblue", linewidth = 1.5) +
  geom_sf(data = watershed_crop, fill = NA, color = "maroon", linewidth = 1.5) +
  geom_sf(data = id_bdry, fill = NA, color = "black", linewidth = 2) +
  geom_sf(data = res_crop, fill = NA, color = "darkgrey", linewidth = 1.5) +
  geom_sf(data = cities_proj, color = "darkgrey", size = 7) +
  geom_sf(data = tva_noholes, fill = NA, color = "darkgrey", linewidth = 1.5) +
  #geom_sf(data = urban_crop, fill = "red") + 
  #scale_fill_brewer(palette = "Set2") +
  #scale_fill_met_d("Cross") +
  scale_fill_met_d("Ingres") +
  labs(#title = "SGFMeans Clusters for Idaho:",
       #subtitle = "k=8, m=1.9, alpha = 0.5, beta = 0.4, window = 3x3", 
       fill = "Archetypes") +
  ggtitle(wrapper("SGFMeans Clusters for Idaho", width = 18)) +
  theme_bw() + 
  theme(text = element_text(size = 16),
        legend.position = "bottom",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))

k8_map
#ggsave(here::here(paste0("outputs/figures/sgfcm_k8_map_", Sys.Date(), ".png")), 
#       plot = k8_map, width = 12, height = 12, dpi = 300)

## Testing out inset maps
# from https://upgo.lab.mcgill.ca/2019/12/13/making-beautiful-maps/

sho_ban <- res_crop %>%
  filter(AIANNHCE == "1185")
cda <- res_crop %>%
  filter(AIANNHCE == "0705")


k8_map + 
  coord_sf(xlim = sf::st_bbox(cda)[c(1,3)],
           ylim = sf::st_bbox(cda)[c(2,4)],
           expand = FALSE
  )

k8_map2 <- ggplot() +
  geom_raster(aes(x = sgfcm.k8.df$x, y = sgfcm.k8.df$y, fill = as.factor(sgfcm.k8.df$Groups)), alpha = 0.85) +
  #geom_sf(data = rivers_crop, fill = NA, color = "skyblue", linewidth = 1.5) +
  #geom_sf(data = watershed_crop, fill = NA, color = "maroon", linewidth = 1.5) +
  #geom_sf(data = id_bdry, fill = NA, color = "black", linewidth = 2) +
  geom_sf(data = res_crop, fill = NA, color = "darkgrey", linewidth = 1.5) +
  #geom_sf(data = cities_proj, color = "darkgrey", size = 7) +
  geom_sf(data = tva_noholes, fill = NA, color = "darkgrey", linewidth = 1.5) +
  #geom_sf(data = urban_crop, fill = "red") +
  geom_rect(aes(xmin = st_bbox(cda)[[1]], ymin = st_bbox(cda)[[2]], xmax = st_bbox(cda)[[3]], ymax = st_bbox(cda)[[4]]),
            fill = NA, color = "black", linewidth = 0.6) +
  geom_rect(aes(xmin = st_bbox(sho_ban)[[1]], ymin = st_bbox(sho_ban)[[2]], xmax = st_bbox(sho_ban)[[3]], ymax = st_bbox(sho_ban)[[4]]),
            fill = NA, color = "black", linewidth = 0.6) +
  geom_rect(aes(xmin = st_bbox(tva_noholes)[[1]], ymin = st_bbox(tva_noholes)[[2]], xmax = st_bbox(tva_noholes)[[3]], ymax = st_bbox(tva_noholes)[[4]]),
            fill = NA, color = "black", linewidth = 0.6) +
  #scale_fill_brewer(palette = "Set2") +
  #scale_fill_met_d("Cross") +
  scale_fill_met_d("Ingres") +
  labs(title = "SGFMeans Clusters for Idaho:",
    #subtitle = "k=8, m=1.9, alpha = 0.5, beta = 0.4, window = 3x3", 
    fill = "Archetypes") +
  ggtitle(wrapper("SGFMeans Clusters for Idaho", width = 18)) +
  theme_bw() + 
  theme(text = element_text(size = 16),
        legend.position = "bottom",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text = element_blank(),
        plot.margin=unit(c(0.05, 0.05, 0.05, 0.05),"mm"),
        plot.background=element_rect(fill="white"))

k8_map2

k8_map_insets <- k8_map2 %>%
  ggdraw() +
  draw_plot(
    {k8_map2 + 
        coord_sf(
          xlim = sf::st_bbox(cda)[c(1,3)],
          ylim = sf::st_bbox(cda)[c(2,4)],
          expand = FALSE) +
        theme(legend.position = "none",
              title = element_blank(), 
              axis.ticks = element_blank())}, 
    x = 0.00, y = 0.6, width = 0.3, height = 0.3) +
  draw_plot(
    {k8_map2 + 
        coord_sf(
          xlim = sf::st_bbox(sho_ban)[c(1,3)],
          ylim = sf::st_bbox(sho_ban)[c(2,4)],
          expand = FALSE) +
        theme(legend.position = "none",
              title = element_blank(), 
              axis.ticks = element_blank())},
    x = 0.7, y = 0.3, width = 0.3, height = 0.3) +
  draw_plot(
    {k8_map2 + 
        coord_sf(
          xlim = sf::st_bbox(tva_noholes)[c(1,3)],
          ylim = sf::st_bbox(tva_noholes)[c(2,4)],
          expand = FALSE) +
        theme(legend.position = "none",
              title = element_blank(), 
              axis.ticks = element_blank())},
    x = 0.00, y = 0.25, width = 0.3, height = 0.3) 
  

k8_map_insets
ggsave(here::here(paste0("outputs/figures/draft_sgfcm_k8_map_w_insets_", Sys.Date(), ".png")), 
       plot = k8_map_insets, width = 12, height = 12, dpi = 300, bg = "white")
