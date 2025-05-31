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
library(forcats)
library(MetBrewer)
#----Load the data----
rst_sc <- rast(here::here("data/processed/rast_stack_all_attributes_scaled_2025-01-07.tif"))
rst <- rast(here::here("data/processed/rast_stack_all_attributes_2025-01-07.tif"))

#gfcm_k3_result <- rast(here::here("outputs/models/gfcm_result_k3_2025-01-08.tif"))
sgfcm_k8_result <- rast(here::here("outputs/models/sgfcm_result_k8_2025-01-08.tif"))

#plot(gfcm_k3_result)
plot(sgfcm_k8_result)

#gfcm_result_k3_mod <- readRDS(here::here("outputs/models/gfcm_mod_k3_2025-01-08.rds"))
sgfcm_result_k8_mod <- readRDS(here::here("outputs/models/sgfcm_mod_k8_2025-01-08.rds"))


# Variable interp plot
#data <- as.data.frame(rst_sc)
data <- as.data.frame(scale(rst))
#data$groups_k3 <- gfcm_result_k3_mod$Groups
data$groups_k8 <- sgfcm_result_k8_mod$Groups

#data$groups_k3 <- gsub('V', 'A', data$groups_k3)
data$groups_k8 <- gsub('V', 'A', data$groups_k8)

k8_long_df <- data %>%
  pivot_longer(!groups_k8, names_to = "var_name")

k8_means_long <- data %>%
  group_by(groups_k8) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  pivot_longer(!groups_k8, names_to = "var_name", values_to = "mean")

k8_med_long <- data %>%
  group_by(groups_k8) %>%
  summarise(across(where(is.numeric), ~ median(.x, na.rm = TRUE))) %>% 
  pivot_longer(!groups_k8, names_to = "var_name", values_to = "median")

k8_sd_long <- data %>%
  group_by(groups_k8) %>%
  summarise(across(where(is.numeric), ~ sd(.x, na.rm = TRUE))) %>% 
  pivot_longer(!groups_k8, names_to = "var_name", values_to = "sd")

k8_long <- left_join(k8_means_long, k8_sd_long)
k8_long <- left_join(k8_long, k8_med_long)

# reorder the variables
k8_long_reorder <- k8_long %>% 
  mutate(var_name = fct_relevel(var_name, 
                                "comm_cap", "eng_burd", "ag_loss", "net_mig", 
                                "dist_crithab", "dist_wilderness",
                                "burn_prob", "ann_max_swe",
                                "dist_powerplant", "dist_windturb", "dist_solar"))

k8_long_reorder <- k8_long_reorder %>%
  mutate(sets = case_when(var_name == "comm_cap" | var_name == "eng_burd" | var_name == "ag_loss" | var_name == "net_mig"  | var_name == "dist_crithab" | var_name == "dist_wilderness" ~ "social",
                            var_name == "burn_prob" | var_name == "ann_max_swe"  ~ "ecological",
                            var_name == "dist_powerplant" | var_name == "dist_windturb" | var_name == "dist_solar"  ~ "technological"))


k8_var_interp <- ggplot(k8_long_reorder, aes(x = var_name, y = mean, fill = sets)) +
  geom_col() +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(.9)) +
  scale_fill_met_d("Kandinsky") +
  coord_flip() +
  facet_wrap(~groups_k8, ncol = 4) +
  theme(text = element_text(size = 16),
        legend.position = "right", 
        axis.title.y = element_blank()) 

k8_var_interp
#ggsave(paste0("~/Analysis/Archetype_Analysis/figures/sgfcm_all_k6_var_interp_", Sys.Date(), ".png"), 
#       plot = k6_var_interp, width = 12, height = 8, dpi = 300) 

k8_var_interp_med <- ggplot(k8_long_reorder, aes(x = var_name, y = median, fill = sets)) +
  geom_col() +
  geom_errorbar(aes(ymin=median-sd, ymax=median+sd), width=.2,
                position=position_dodge(.9)) +
  scale_fill_met_d("Kandinsky") +
  coord_flip() +
  facet_wrap(~groups_k8, ncol = 4) +
  theme(text = element_text(size = 20),
        legend.position = "right", 
        axis.title.y = element_blank()) 

k8_var_interp_med
#ggsave(paste0("~/Analysis/Archetype_Analysis/figures/sgfcm_all_k6_var_interp_med_", Sys.Date(), ".png"), 
#       plot = k8_var_interp_med, width = 12, height = 8, dpi = 300)

# Create bar plots of the variables where IQR != 0
check_iqr_overlap <- function(x) {
  # Calculate the 25th and 75th percentiles
  lower <- quantile(x, 0.25, na.rm = TRUE)
  upper <- quantile(x, 0.75, na.rm = TRUE)
  
  # Check if the IQR range overlaps 0
  return(lower <= 0 & upper >= 0)
}

overlap <- k8_long_df %>% 
  group_by(groups_k8, var_name) %>% 
  summarise(overlap = check_iqr_overlap(value), .groups="drop")


k8_long_join <- k8_long_df %>% 
  left_join(overlap) %>% 
  filter(overlap == FALSE) # use when looking at all archetypes
  #mutate(value = case_when(overlap == TRUE ~ 0,
  #                         overlap == FALSE ~ value)) # use mutate for individual arches

# reorder the variables
 k8_long_overlap_reorder <- k8_long_join %>% 
  mutate(var_name = fct_relevel(var_name, 
                                "comm_cap", "eng_burd", "ag_loss", "net_mig", 
                                "dist_crithab", "dist_wilderness",
                                "burn_prob", "ann_max_swe",
                                "dist_powerplant", "dist_windturb", "dist_solar"
  ))

k8_long_overlap_reorder <- k8_long_overlap_reorder %>% # need to remove dist to critical habitat and less hs
  mutate(sets = case_when(var_name == "comm_cap" | var_name == "eng_burd" | var_name == "ag_loss" | var_name == "net_mig"  | var_name == "dist_crithab" | var_name == "dist_wilderness" ~ "social",
                          var_name == "burn_prob" | var_name == "ann_max_swe"  ~ "ecological",
                          var_name == "dist_powerplant" | var_name == "dist_windturb" | var_name == "dist_solar"  ~ "technological"))

# replace var_name with more easily interpreted names 
k8_long_overlap_reorder_newnames <- k8_long_overlap_reorder %>%
  mutate(new_var_name = case_when(var_name == "burn_prob" ~ "burn probability",
                                  var_name == "dist_wilderness" ~ "dist. to wilderness area",
                                  var_name == "dist_crithab" ~ "dist. to critical habitat",
                                  var_name == "net_mig" ~ "net migration 2022", 
                                  var_name == "comm_cap" ~ "community capital",
                                  var_name == "ag_loss" ~ "expected ag. loss rate",
                                  var_name == "eng_burd" ~ "energy burden",
                                  var_name == "ann_max_swe" ~ "annual max swe",
                                  var_name == "dist_powerplant" ~ "dist. to power plant",
                                  var_name == "dist_windturb" ~ "dist. to wind turbines",
                                  var_name == "dist_solar" ~ "dist. to solar fields"))

# reorder the variables
k8_long_overlap_reorder_newnames <- k8_long_overlap_reorder_newnames %>% 
  mutate(new_var_name = fct_relevel(new_var_name, 
                                    "community capital", 
                                    "energy burden", 
                                    "expected ag. loss rate", 
                                    "net migration 2022", 
                                    "dist. to critical habitat",
                                    "dist. to wilderness area",
                                    "burn probability", 
                                    "annual max swe", 
                                    "dist. to power plant", 
                                    "dist. to wind turbines", 
                                    "dist. to solar fields"
  ))

idv_arch <- k8_long_overlap_reorder_newnames %>%
  filter(groups_k8 == "A8")

k8_iqr_no_overlap <- ggplot(data=k8_long_overlap_reorder_newnames, mapping = aes(x=new_var_name, y=value, fill=sets)) +
  geom_boxplot(outliers = FALSE, coef=0) +
  geom_hline(yintercept = 0, linetype=2) +
  scale_fill_met_d("Kandinsky")+
  coord_flip()+
  theme_bw()+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
        text = element_text(size = 20),
        legend.position = "right") +
  facet_wrap(vars(groups_k8), ncol = 4)

k8_iqr_no_overlap
ggsave(here::here(paste0("outputs/figures/sgfcm_k8_var_interp_iqr_no_overlap_", Sys.Date(), ".png")), 
       plot = k8_iqr_no_overlap, width = 12, height = 8, dpi = 300)

# Create bar plots of the standard deviation for the values that do overlap with 0
k8_long_join_overlap_true <- k8_long_df %>% 
  left_join(overlap) %>% 
  filter(overlap == TRUE)

k8_sd_overlap <- k8_long_join_overlap_true %>% 
  group_by(groups_k8, var_name) %>% 
  summarise(sd = sd(value), .groups="drop")

# reorder the variables
k8_sd_long_overlap_reorder <- k8_sd_overlap %>% 
  mutate(var_name = fct_relevel(var_name, 
                                "comm_cap", "eng_burd", "ag_loss", "net_mig", 
                                "dist_crithab", "dist_wilderness",
                                "burn_prob", "ann_max_swe",
                                "dist_powerplant", "dist_windturb", "dist_solar"
  ))

k8_sd_long_overlap_reorder <- k8_sd_long_overlap_reorder %>% # need to remove dist to critical habitat and less hs
  mutate(sets = case_when(var_name == "comm_cap" | var_name == "eng_burd" | var_name == "ag_loss" | var_name == "net_mig"  | var_name == "dist_crithab" | var_name == "dist_wilderness" ~ "social",
                          var_name == "burn_prob" | var_name == "ann_max_swe"  ~ "ecological",
                          var_name == "dist_powerplant" | var_name == "dist_windturb" | var_name == "dist_solar"  ~ "technological"))

# replace var_name with more easily interpreted names 
k8_sd_long_overlap_reorder_newnames <- k8_sd_long_overlap_reorder %>%
  mutate(new_var_name = case_when(var_name == "burn_prob" ~ "burn probability",
                                  var_name == "dist_wilderness" ~ "dist. to wilderness area",
                                  var_name == "dist_crithab" ~ "dist. to critical habitat",
                                  var_name == "net_mig" ~ "net migration 2022", 
                                  var_name == "comm_cap" ~ "community capital",
                                  var_name == "ag_loss" ~ "expected ag. loss rate",
                                  var_name == "eng_burd" ~ "energy burden",
                                  var_name == "ann_max_swe" ~ "annual max swe",
                                  var_name == "dist_powerplant" ~ "dist. to power plant",
                                  var_name == "dist_windturb" ~ "dist. to wind turbines",
                                  var_name == "dist_solar" ~ "dist. to solar fields"))

# reorder the variables
k8_sd_long_overlap_reorder_newnames <- k8_sd_long_overlap_reorder_newnames %>% 
  mutate(new_var_name = fct_relevel(new_var_name, 
                                    "community capital", 
                                    "energy burden", 
                                    "expected ag. loss rate", 
                                    "net migration 2022", 
                                    "dist. to critical habitat",
                                    "dist. to wilderness area",
                                    "burn probability", 
                                    "annual max swe", 
                                    "dist. to power plant", 
                                    "dist. to wind turbines", 
                                    "dist. to solar fields"
  ))

k8_sd_overlap <- ggplot(k8_sd_long_overlap_reorder_newnames, aes(x = new_var_name, y = sd, fill = sets)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype=2) +
  scale_fill_met_d("Kandinsky")+
  coord_flip()+
  theme_bw()+
  facet_wrap(vars(groups_k8), ncol = 4)

k8_sd_overlap
ggsave(here::here(paste0("outputs/figures/sgfcm_k8_var_interp_sd_overlap_", Sys.Date(), ".png")), 
       plot = k8_sd_overlap, width = 12, height = 8, dpi = 300)



