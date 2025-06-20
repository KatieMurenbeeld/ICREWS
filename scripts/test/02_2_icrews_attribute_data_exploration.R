library(tidyverse)
library(terra)
library(raster)
library(sf)
library(ggplot2)
library(reshape2)
library(ggcorrplot)
library(GGally)
library(PerformanceAnalytics)

options(scipen=999)

#----Load the data----
#df <- as.data.frame(rast(here::here("data/processed/rast_stack_all_attributes_2025-01-07.tif")))
df <- as.data.frame(rast(here::here("data/processed/rast_stack_02_all_attributes_2025-06-20.tif")))

#---Get summary statistics for each variable----
summ_df <- as.data.frame(summary(df))

df_sd <- as.data.frame(apply(df, 2, sd), col.names = "standard_deviation")
df_mean <- as.data.frame(apply(df, 2, mean), col.names = "mean")
df_median <- as.data.frame(apply(df, 2, median), col.names = "median")

df_summ <- cbind(df_mean, df_median, df_sd)
df_summ <- df_summ %>%
  rename("mean" = `apply(df, 2, mean)`,
         "median" = `apply(df, 2, median)`,
         "stan_dev" = `apply(df, 2, sd)`)

#---Get correlations between each variable----
df_corr <- cor(df, method = "pearson")

df_corrplot <- ggcorrplot(df_corr, hc.order = TRUE, type = "lower", lab = TRUE) +
  theme_bw() + 
  theme(axis.text.x=element_text(size=12, angle=45, vjust=0.9, hjust=1, 
                                 margin=margin(-3,0,0,0)),
        axis.text.y=element_text(size=12),
        axis.title.x = element_blank(), axis.title.y = element_blank())

df_corrplot

ggsave(here::here(paste0("outputs/figures/icrews_attri_corrplot_rast_stack_02_", Sys.Date(), ".png")), 
       plot = df_corrplot, width = 20, height = 20, dpi = 300)

#---Plot histograms of variables----
df_melt <- melt(df)

df_hist <- ggplot(df_melt ,aes(x = value)) + 
  facet_wrap(~variable,scales = "free_x") + 
  geom_histogram(bins = 20)
df_hist

ggsave(here::here(paste0("outputs/figures/icrews_attri_hists_rast_stack_02_", Sys.Date(), ".png")), 
       plot = df_hist, width = 20, height = 20, dpi = 300)

#---Scatter plots?---- 
## maybe I could make a figure like I did for my dissertation...

### Using GGally library
test_ggpairs <- ggpairs(df)
test_ggpairs

### Using PerformanceAnalytics library
#test_performan <- chart.Correlation(df, method = "pearson")
#test_performan





