library(tidyverse)
library(terra)


# Load the rasters
commcap_id <- rast(here::here("data/processed/bric_commcap_id_3km_pred_crop_2025-01-07.tif"))
ealr_id <- rast(here::here("data/processed/cejst_ealr_id_3km_pred_crop_2025-01-07.tif"))
engburd_id <- rast(here::here("data/processed/cejst_engbrd_id_3km_pred_crop_2025-01-07.tif"))
netmig_id <- rast(here::here("data/processed/net_mig_2023_id_3km_crop_2025-01-07.tif"))
bp_id <- rast(here::here("data/processed/wfrc_BP_ID_3km_2024-12-03.tif"))
dist_pp_id <- rast(here::here("data/processed/dist_to_powerplant_id_3km_pred_crop_2025-01-07.tif"))
dist_wt_id <- rast(here::here("data/processed/dist_to_windturbine_id_3km_pred_crop_2025-01-07.tif"))
dist_pv_id <- rast(here::here("data/processed/dist_to_photovoltaic_id_3km_pred_crop_2025-01-07.tif"))
dist_crit_id <- rast(here::here("data/processed/crithab_dist_id_3km_pred_crop_2025-01-07.tif"))
dist_wild_id <- rast(here::here("data/processed/wildarea_dist_id_3km_pred_crop_2025-01-07.tif"))
swe_id <- rast(here::here("data/processed/swe_ann_max_id_3km_crop_2025-01-07.tif"))

#----check for NAs or unequal lengths of data----

nrow(as.data.frame(commcap_id)) #23484
nrow(as.data.frame(ealr_id)) #23484
nrow(as.data.frame(engburd_id)) #23484
nrow(as.data.frame(netmig_id)) #23484
nrow(as.data.frame(bp_id)) #23484
nrow(as.data.frame(dist_pp_id)) #23484
nrow(as.data.frame(dist_wt_id)) #23484
nrow(as.data.frame(dist_pv_id)) #23484
nrow(as.data.frame(dist_crit_id)) #23484
nrow(as.data.frame(dist_wild_id)) #23484
nrow(as.data.frame(swe_id)) #23484


# Check alignment by stacking the rasters
rast_stack <- c(commcap_id, ealr_id, engburd_id, netmig_id,
                bp_id, dist_pp_id, dist_wt_id, dist_pv_id, 
                dist_crit_id, dist_wild_id, swe_id)

# Rename variables
names(rast_stack) <- c("comm_cap", "ag_loss", "eng_burd", "net_mig",
                       "burn_prob", "dist_powerplant", "dist_windturb", "dist_solar",
                       "dist_crithab", "dist_wilderness", "ann_max_swe")

## Save the raster
writeRaster(x = rast_stack, filename = here::here(paste0("data/processed/rast_stack_all_attributes_", 
                                              Sys.Date(), ".tif")), overwrite = TRUE)
## Scale the data from 0-1 and save the raster
rast_stack_sc <- (rast_stack - global(rast_stack, "min", na.rm=TRUE)[,1])/(global(rast_stack, "max", na.rm=TRUE)[,1] - global(rast_stack, "min", na.rm=TRUE)[,1])
writeRaster(x = rast_stack_sc, filename = here::here(paste0("data/processed/rast_stack_all_attributes_scaled_", 
                                                            Sys.Date(), ".tif")), overwrite = TRUE)
