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

#----Load the data----
rst_sc <- rast(here::here("data/processed/rast_stack_all_attributes_scaled_2025-01-07.tif"))
rst <- rast(here::here("data/processed/rast_stack_all_attributes_2025-01-07.tif"))

gfcm_k3_result <- rast(here::here("outputs/models/gfcm_result_k3_2025-01-08.tif"))
sgfcm_k8_result <- rast(here::here("outputs/models/sgfcm_result_k8_2025-01-08.tif"))

plot(gfcm_k3_result)
plot(sgfcm_k8_result)

gfcm_result_k3_mod <- readRDS(here::here("outputs/models/gfcm_mod_k3_2025-01-08.rds"))
sgfcm_result_k8_mod <- readRDS(here::here("outputs/models/sgfcm_mod_k8_2025-01-08.rds"))


# Variable interp plot
#data <- as.data.frame(rst_sc)
data <- as.data.frame(scale(rst))
data$groups_k3 <- gfcm_k3_result$Groups
data$groups_k8 <- sgfcm_k8_result$Groups

data$groups_k3 <- gsub('V', 'A', data$groups_k3)
data$groups_k8 <- gsub('V', 'A', data$groups_k8)

k3_long_df <- data %>%
  dplyr::select(-groups_k8) %>%
  pivot_longer(!groups_k6, names_to = "var_name")

k3_means_long <- data %>%
  group_by(groups_k3) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  pivot_longer(!groups_k3, names_to = "var_name", values_to = "mean")

k3_sd_long <- data %>%
  group_by(groups_k3) %>%
  summarise(across(where(is.numeric), ~ sd(.x, na.rm = TRUE))) %>% 
  pivot_longer(!groups_k3, names_to = "var_name", values_to = "sd")

k3_med_long <- data %>%
  group_by(groups_k3) %>%
  summarise(across(where(is.numeric), ~ median(.x, na.rm = TRUE))) %>% 
  pivot_longer(!groups_k3, names_to = "var_name", values_to = "median")

k3_long <- left_join(k3_means_long, k3_sd_long)
k3_long <- left_join(k3_long, k3_med_long)

k8_means_long <- data %>%
  group_by(groups_k8) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  pivot_longer(!groups_k8, names_to = "var_name", values_to = "mean")

k8_sd_long <- data %>%
  group_by(groups_k8) %>%
  summarise(across(where(is.numeric), ~ sd(.x, na.rm = TRUE))) %>% 
  pivot_longer(!groups_k8, names_to = "var_name", values_to = "sd")

k8_long <- left_join(k8_means_long, k8_sd_long)

# reorder the variables
k3_long_reorder <- k3_long %>% 
  mutate(var_name = fct_relevel(var_name, 
                                "treecov", "forprod", "tempseas", 
                                "precseas", "rough", "whp", 
                                "forgain", "distcrit", "distwild",
                                "pm25", "fedrich", 
                                "treeage", "pct_forpay", "pct_delmill",
                                "netmig", "comm_cap", "aip", 
                                "travtime", "hsbrd", "engbrd", "lesshs"))

k3_long_reorder <- k3_long_reorder %>%
  mutate(sets = case_when(var_name == "treecov" | var_name == "forprod"  ~ "social",
                            var_name == "distwild" | var_name == "distcrit"  ~ "ecological",
                            var_name == "fedrich" | var_name == "pm25"  ~ "technological"))


k3_var_interp <- ggplot(k3_long_reorder, aes(x = var_name, y = mean, fill = sets)) +
  geom_col() +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(.9)) +
  #scale_fill_brewer(palette = "Set2") +
  coord_flip() +
  facet_wrap(~groups_k3) +
  theme(text = element_text(size = 20),
        legend.position = "right", 
        axis.title.y = element_blank()) 

k3_var_interp
#ggsave(paste0("~/Analysis/Archetype_Analysis/figures/sgfcm_all_k6_var_interp_", Sys.Date(), ".png"), 
#       plot = k6_var_interp, width = 12, height = 8, dpi = 300) 

k6_var_interp_sd <- ggplot(k6_long_reorder, aes(x = var_name, y = sd, fill = ostrom)) +
  geom_col() +
  #scale_fill_brewer(palette = "Set2") +
  coord_flip() +
  facet_wrap(~groups_k6) +
  theme(text = element_text(size = 20),
        legend.position = "right", 
        axis.title.y = element_blank()) 

k6_var_interp_sd
ggsave(paste0("~/Analysis/Archetype_Analysis/figures/sgfcm_all_k6_var_interp_sd_", Sys.Date(), ".png"), 
       plot = k6_var_interp_sd, width = 12, height = 8, dpi = 300) 

k6_var_interp_med <- ggplot(k6_long_reorder, aes(x = var_name, y = median, fill = ostrom)) +
  geom_col() +
  geom_errorbar(aes(ymin=median-sd, ymax=median+sd), width=.2,
                position=position_dodge(.9)) +
  #scale_fill_brewer(palette = "Set2") +
  coord_flip() +
  facet_wrap(~groups_k6) +
  theme(text = element_text(size = 20),
        legend.position = "right", 
        axis.title.y = element_blank()) 

k6_var_interp_med
ggsave(paste0("~/Analysis/Archetype_Analysis/figures/sgfcm_all_k6_var_interp_med_", Sys.Date(), ".png"), 
       plot = k6_var_interp_med, width = 12, height = 8, dpi = 300)

ggplot(k8_long, aes(x = var_name, y = mean, fill = groups_k8)) +
  geom_col() +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(.9)) +
  coord_flip() +
  facet_wrap(~groups_k8, ncol = 4) +
  theme(legend.position = "none", 
        axis.title.y = element_blank()) 

# Create bar plots of the variables where IQR != 0
check_iqr_overlap <- function(x) {
  # Calculate the 25th and 75th percentiles
  lower <- quantile(x, 0.25, na.rm = TRUE)
  upper <- quantile(x, 0.75, na.rm = TRUE)
  
  # Check if the IQR range overlaps 0
  return(lower <= 0 & upper >= 0)
}

overlap <- k6_long_df %>% 
  group_by(groups_k6, var_name) %>% 
  summarise(overlap = check_iqr_overlap(value), .groups="drop")


k6_long_join <- k6_long_df %>% 
  left_join(overlap) %>% 
  filter(overlap == FALSE)

# reorder the variables
k6_long_overlap_reorder <- k6_long_join %>% 
  mutate(var_name = fct_relevel(var_name, 
                                "treecov", "forprod", "tempseas", 
                                "precseas", "rough", "whp", 
                                "forgain", 
                                #"distcrit", 
                                "distwild",
                                "pm25", "fedrich", 
                                "treeage", "pct_forpay", "pct_delmill",
                                "netmig", "comm_cap", "aip",
                                "travtime", "hsbrd", "engbrd"
                                #"lesshs"
  ))

k6_long_overlap_reorder <- k6_long_overlap_reorder %>% # need to remove dist to critical habitat and less hs
  mutate(ostrom = case_when(var_name == "treecov" | var_name == "forprod" | var_name == "tempseas" | var_name == "precseas" | var_name == "rough" | var_name == "whp" ~ "resource system",
                            var_name == "distwild" | var_name == "forgain" ~ "resource unit",
                            var_name == "fedrich" | var_name == "pm25" | var_name == "treeage" | var_name == "pct_forpay" | var_name == "pct_delmill" | var_name == "netmig" | var_name == "comm_cap" | var_name == "aip" | var_name == "travtime" | var_name == "hsbrd" | var_name == "engbrd" | var_name == "lesshs" ~ "users"))


k6_iqr_no_overlap <- ggplot(data=k6_long_overlap_reorder, mapping = aes(x=var_name, y=value, fill=ostrom)) +
  geom_boxplot(outliers = FALSE, coef=0) +
  geom_hline(yintercept = 0, linetype=2) +
  #scale_fill_manual(values=c15)+
  coord_flip()+
  theme_bw()+
  facet_wrap(vars(groups_k6))

k6_iqr_no_overlap
ggsave(paste0("~/Analysis/Archetype_Analysis/figures/sgfcm_all_k6_var_interp_iqr_no_overlap_", Sys.Date(), ".png"), 
       plot = k6_iqr_no_overlap, width = 12, height = 8, dpi = 300)

# Create bar plots of the standard deviation for the values that do overlap with 0
k6_long_join_overlap_true <- k6_long_df %>% 
  left_join(overlap) %>% 
  filter(overlap == TRUE)

k6_sd_overlap <- k6_long_join_overlap_true %>% 
  group_by(groups_k6, var_name) %>% 
  summarise(sd = sd(value), .groups="drop")

# reorder the variables
k6_sd_overlap_reorder <- k6_sd_overlap %>% 
  mutate(var_name = fct_relevel(var_name, 
                                #"treecov", 
                                "forprod", "tempseas", 
                                "precseas", "rough", "whp", 
                                "forgain", 
                                "distcrit", 
                                "distwild",
                                #"pm25", 
                                "fedrich", 
                                "treeage", "pct_forpay", 
                                #"pct_delmill",
                                "netmig", "comm_cap", "aip", 
                                "travtime", "hsbrd", "engbrd", "lesshs"))

k6_sd_long_overlap_reorder <- k6_sd_overlap_reorder %>% # need to remove treecover, pm25, and pct_delmill
  mutate(ostrom = case_when(var_name == "forprod" | var_name == "tempseas" | var_name == "precseas" | var_name == "rough" | var_name == "whp" ~ "resource system",
                            var_name == "distwild" | var_name == "distcrit" |  var_name == "forgain" ~ "resource unit",
                            var_name == "fedrich" | var_name == "treeage" | var_name == "pct_forpay" | var_name == "netmig" | var_name == "comm_cap" | var_name == "aip" | var_name == "travtime" | var_name == "hsbrd" | var_name == "engbrd" | var_name == "lesshs" ~ "users"))


k6_sd_overlap <- ggplot(k6_sd_long_overlap_reorder, aes(x = var_name, y = sd, fill = ostrom)) +
  geom_col() +
  #scale_fill_brewer(palette = "Set2") +
  coord_flip() +
  facet_wrap(~groups_k6) +
  theme(text = element_text(size = 20),
        legend.position = "right", 
        axis.title.y = element_blank()) 

k6_sd_overlap
ggsave(paste0("~/Analysis/Archetype_Analysis/figures/sgfcm_all_k6_var_interp_sd_overlap_", Sys.Date(), ".png"), 
       plot = k6_sd_overlap, width = 12, height = 8, dpi = 300)


ggplot(data=k6_long_overlap_reorder, mapping = aes(x=var_name, y=sd, fill=ostrom)) +
  geom_boxplot(outliers = FALSE, coef=0) +
  geom_hline(yintercept = 0, linetype=2) +
  #scale_fill_manual(values=c15)+
  coord_flip()+
  theme_bw()+
  facet_wrap(vars(groups_k6))


