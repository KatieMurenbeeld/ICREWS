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
# additional rasters (June 20 2025)
pct_ag_man_id <- rast(here::here("data/processed/pct_ag_man_industry_2023_id_3km_crop_2025-06-20.tif"))
pct_irr_id <- rast(here::here("data/processed/irrigated_lands_pct_id_3km_crop_2025-06-20.tif"))
ret_id <- rast(here::here("data/processed/ret_mean_2014_2024_id_3km_crop_2025-06-17.tif"))
geo_well_dist_id <- rast(here::here("data/processed/idwr_geo_wells_dist_id_3km_crop_2025-06-20.tif"))
num_gov_10k_id <- rast(here::here("data/processed/num_gov_10k_2022_id_3km_crop_2025-06-18.tif"))
pct_exp_water_id <- rast(here::here("data/processed/pct_charged_more_1000_year_water_sewer_2023_id_3km_crop_2025-06-11.tif"))
pct_exp_elect_id <- rast(here::here("data/processed/pct_charged_more_250_month_electricity_2023_id_3km_crop_2025-06-11.tif"))
med_age_id <- rast(here::here("data/processed/median_age_2023_id_3km_crop_2025-06-11.tif"))
fed_area_id <- rast(here::here("data/processed/fed_area_id_3km_pred_crop_2025-06-10.tif"))
well_prod_id <- rast(here::here("data/processed/idwr_wells_production_sum_id_3km_crop_2025-06-10.tif"))
inject_act_id <- rast(here::here("data/processed/idwr_inject_wells_active_count_id_3km_crop_2025-06-10.tif"))
fed_rich_id <- rast(here::here("data/processed/fed_rich_id_3km_pred_crop_2025-06-09.tif"))
wildscenic_dist_id <- rast(here::here("data/processed/wildscenic_dist_id_3km_pred_crop_2025-06-06.tif"))
rough_id <- rast(here::here("data/processed/roughness_id_3km_pred_crop_2025-06-06.tif"))
cdd_id <- rast(here::here("data/processed/cdd_10yr_ave_2014_2024_id_3km_crop_2025-06-06.tif"))
hdd_id <- rast(here::here("data/processed/hdd_10yr_ave_2014_2024_id_3km_crop_2025-06-06.tif"))
mod_drought_id <- rast(here::here("data/processed/mod_drought_count_2014_2024_id_3km_crop_2025-06-05.tif"))
dist_rail_id <- rast(here::here("data/processed/rail_dist_id_3km_pred_crop_2025-06-04.tif"))
crosses_rail_id <- rast(here::here("data/processed/rail_crosses_id_3km_pred_crop_2025-06-04.tif"))
crosses_road_id <- rast(here::here("data/processed/all_road_type_crosses_id_3km_pred_crop_2025-06-04.tif"))
gw_pod_id <- rast(here::here("data/processed/waterright_gw_pod_count_id_3km_pred_crop_2025-06-04.tif"))
sw_pod_id <- rast(here::here("data/processed/waterright_sw_pod_count_id_3km_pred_crop_2025-06-04.tif"))
pop_dens_id <- rast(here::here("data/processed/ave_pop_densitykm2_2020_2024_id_3km_crop_2025-05-19.tif"))
aip_id <- rast(here::here("data/processed/aip_id_3km_pred_crop_2025-05-12.tif"))
frac_imp_id <- rast(here::here("data/processed/fraction_impervious_id_3km_pred_crop_2025-05-12.tif"))
dist_transline_id <- rast(here::here("data/processed/dist_to_transline_id_3km_pred_crop_2025-05-12.tif"))
pop_change_id <- rast(here::here("data/processed/ave_pop_change_2020_2024_id_3km_crop_2025-05-12.tif"))
dist_river_id <- rast(here::here("data/processed/dist_river_500k_vis_2023_id_3km_crop_2025-06-20.tif"))

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

rast_stack_02 <- c(commcap_id, ealr_id, engburd_id, netmig_id,
                bp_id, dist_pp_id, dist_wt_id, dist_pv_id, 
                dist_crit_id, dist_wild_id, swe_id, pct_ag_man_id,
                pct_irr_id, ret_id, geo_well_dist_id, num_gov_10k_id,
                pct_exp_water_id, pct_exp_elect_id, med_age_id, 
                fed_area_id, well_prod_id, inject_act_id, fed_rich_id,
                wildscenic_dist_id, rough_id, cdd_id, hdd_id, 
                mod_drought_id, dist_rail_id, crosses_road_id, 
                gw_pod_id, sw_pod_id, pop_dens_id, aip_id, 
                frac_imp_id, dist_transline_id, pop_change_id, 
                dist_river_id$river_dist_km)

# Rename variables
names(rast_stack) <- c("comm_cap", "ag_loss", "eng_burd", "net_mig",
                       "burn_prob", "dist_powerplant", "dist_windturb", "dist_solar",
                       "dist_crithab", "dist_wilderness", "ann_max_swe")

names(rast_stack_02) <- c("comm_cap", "ag_loss", "eng_burd", "net_mig",
                       "burn_prob", "dist_powerplant", "dist_windturb", "dist_solar",
                       "dist_crithab", "dist_wilderness", "ann_max_swe", "pct_ag_manu_industry",
                       "pct_irri_land", "10yr_ave_ret", "dist_geothermal_well", "num_gov_per_10k",
                       "pct_paid_>1000_yr_water_sewer", "pct_paid_>250_month_electricity", "median_age", 
                       "pct_fed_area", "well_production_total", "active_inject_wells_count", "fed_rich", 
                       "dist_wildscenic_river", "roughness", "cooling_degree_days", "heating_degree_days", 
                       "sum_days_mod_drought", "dist_to_rail", "road_density", "gw_rights_sum", "sw_rights_sum", 
                       "pop_density", "conservatism", "pct_impervious", "dist_transline",
                       "pop_change_2020_2024", "dist_river_stream")

## Save the raster
writeRaster(x = rast_stack, filename = here::here(paste0("data/processed/rast_stack_all_attributes_", 
                                              Sys.Date(), ".tif")), overwrite = TRUE)
writeRaster(x = rast_stack_02, filename = here::here(paste0("data/processed/rast_stack_02_all_attributes_", 
                                                         Sys.Date(), ".tif")), overwrite = TRUE)
## Scale the data from 0-1 and save the raster
rast_stack_sc <- (rast_stack - global(rast_stack, "min", na.rm=TRUE)[,1])/(global(rast_stack, "max", na.rm=TRUE)[,1] - global(rast_stack, "min", na.rm=TRUE)[,1])
writeRaster(x = rast_stack_sc, filename = here::here(paste0("data/processed/rast_stack_all_attributes_scaled_", 
                                                            Sys.Date(), ".tif")), overwrite = TRUE)
rast_stack_02_sc <- (rast_stack_02 - global(rast_stack_02, "min", na.rm=TRUE)[,1])/(global(rast_stack_02, "max", na.rm=TRUE)[,1] - global(rast_stack_02, "min", na.rm=TRUE)[,1])
writeRaster(x = rast_stack_02_sc, filename = here::here(paste0("data/processed/rast_stack_02_all_attributes_scaled_", 
                                                            Sys.Date(), ".tif")), overwrite = TRUE)
