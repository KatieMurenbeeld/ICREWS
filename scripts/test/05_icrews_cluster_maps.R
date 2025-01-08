library(tidyverse)
library(terra)
library(sf)
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

# Load the data
gfcm_k3_result <- rast(here::here("outputs/models/gfcm_result_k3_2025-01-08.tif"))
sgfcm_k8_result <- rast(here::here("outputs/models/sgfcm_result_k8_2025-01-08.tif"))
# think about other data to add, cities? rivers? "test bed" sites?

plot(sgfcm_all_k6_result)
plot(sgfcm_all_k8_result)

gfcm_result_k3_mod <- readRDS(here::here("outputs/models/gfcm_mod_k3_2025-01-08.rds"))
sgfcm_result_k8_mod <- readRDS(here::here("outputs/models/sgfcm_mod_k8_2025-01-08.rds"))

## Reproject the [?SHAPES?] to NAD83
projection <- "epsg: 5070"

shp_proj <- st_transform(shape, projection)

# and crop to Idaho
shp_crop <- st_crop(shp_proj, ext(gfcm_result_k3_mod))

## Create a map of the clusters with the Region and National Forest boundaries
# for k = 3
gfcm.k3.df <- gfcm_k3_result$Groups %>% as.data.frame(xy = TRUE)

k3_map <- ggplot() +
  geom_raster(aes(x = gfcm.k3.df$x, y = gfcm.k3.df$y, fill = as.factor(gfcm.k3.df$Groups))) +
  geom_sf(data = fs_nf.crop, fill = NA, color = "black") +
  geom_sf(data = fs_reg.crop, fill = NA, color = "black", linewidth = 1.1) +
  scale_fill_brewer(palette = "Set2") +
  #scale_fill_met_d("Hokusai3") +
  labs(title = "Generalized Fuzzy C-Means:",
       subtitle = "k=3, m=1.9, alpha = 0.6, beta = 0.4, window = 7x7", 
       fill = "Archetypes") +
  theme_bw() + 
  theme(text = element_text(size = 20),
        legend.position = "bottom",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))

#k3_map
ggsave(here::here(paste0("outputs/figures/gfcm_k3_map_", Sys.Date(), ".png")), 
       plot = k3_map, width = 12, height = 12, dpi = 300) 

# repeat for k = 8

sgfcm.k8.df <- sgfcm_k8_result$Groups %>% as.data.frame(xy = TRUE)

k8_map <- ggplot() +
  geom_raster(aes(x = sgfcm.k8.df$x, y = sgfcm.k8.df$y, fill = as.factor(sgfcm.k8.df$Groups))) +
  geom_sf(data = fs_nf.crop, fill = NA, color = "black") +
  geom_sf(data = fs_reg.crop, fill = NA, color = "black", linewidth = 1.1) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Spatial Generalized Fuzzy C-Means:",
       subtitle = "k=8, m=1.9, alpha = 0.5, beta = 0.4, window = 3x3", 
       fill = "Archetypes") +
  theme_bw() + 
  theme(text = element_text(size = 20),
        legend.position = "bottom",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))

k8_map
ggsave(here::here(paste0("outputs/figures/sgfcm_k8_map_", Sys.Date(), ".png")), 
       plot = k3_map, width = 12, height = 12, dpi = 300)