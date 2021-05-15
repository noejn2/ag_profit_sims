rm(list = ls())

# Sets-up datasets for maps
options(scipen = 999)

library(tidyverse)
library(readxl)
library(ggpubr)

USmap_state_df <- readRDS(file = 'data/state_sims_map_df.rds')
IL_sims_comp   <- read_excel(path = 'output/ag_profit_sims_IL.xlsx',
                             sheet = "competitors")
IL_sims_impo   <- read_excel(path = 'output/ag_profit_sims_IL.xlsx',
                             sheet = "importers")

USmap_state_df <- left_join(USmap_state_df,
                            IL_sims_comp,
                            by = c("st_name" = "state"))
USmap_state_df <- USmap_state_df %>%
  select(!SE) %>%
  rename(IL_sims_comp = estimate) %>%
  mutate(IL_sims_comp = IL_sims_comp*acres/1000000)

USmap_state_df <- left_join(USmap_state_df,
                            IL_sims_impo,
                            by = c("st_name" = "state"))
USmap_state_df <- USmap_state_df %>%
  select(!SE) %>%
  rename(IL_sims_impo = estimate) %>%  
  mutate(IL_sims_impo = IL_sims_impo*acres/1000000)

# Checking data summary to create the breaks ----
summary(USmap_state_df$IL_sims_comp)
summary(USmap_state_df$IL_sims_impo)
# The palette:
colorspace::diverge_hsv(6)

breaks = c(-Inf, 0, 10, 50, 150, 500, Inf)
labels = c("Loss",
           "$  0 M and $10 M",
           "$ 10 M and $50 M",
           "$ 50 M and $150 M",
           "$150 M and $500 M",
           "Above $500 M")

# Building left map (no trade) -----
left_map <- ggplot() +
  geom_polygon(data = USmap_state_df,
               aes(x = long,
                   y = lat,
                   group = group,
                   fill = cut(IL_sims_comp,
                              breaks = breaks,
                              labels = labels)),
               color = NA) +
  geom_polygon(data = USmap_state_df,
               aes(x = long,
                   y = lat,
                   group = group),
               fill = NA,
               color = "black",
               size = .1) +
  labs(
    title = "Drought in Iowa and Kansas"
  ) +
  scale_fill_manual(values = c(
    "Loss"                 = "#bd0026",
    "$  0 M and $10 M"     = "#eff3ff",
    "$ 10 M and $50 M"     = "#bdd7e7",
    "$ 50 M and $150 M"    = "#6baed6",
    "$150 M and $500 M"    = "#3182bd",
    "Above $500 M" = "#08519c")) +
  theme(panel.background = element_rect(fill = NA, 
                                        color = NA)) +
  coord_equal() +
  theme(legend.direction = "horizontal",
        legend.position = "bottom") +
  guides(fill = guide_legend(title = "Millions of 2012 dollars:",
                             title.position = "top"))

# Building right map (trade) -----
right_map <- ggplot() +
  geom_polygon(data = USmap_state_df,
               aes(x = long,
                   y = lat,
                   group = group,
                   fill = cut(IL_sims_impo,
                              breaks = breaks,
                              labels = labels)),
               color = NA) +
  geom_polygon(data = USmap_state_df,
               aes(x = long,
                   y = lat,
                   group = group),
               fill = NA,
               color = "black",
               size = .1) +
  labs(
    title = "Drought in Louisiana and Missouri"
  ) +
  scale_fill_manual(values = c(
    "Loss"                 = "#bd0026",
    "$  0 M and $10 M"     = "#eff3ff",
    "$ 10 M and $50 M"     = "#bdd7e7",
    "$ 50 M and $150 M"    = "#6baed6",
    "$150 M and $500 M"    = "#3182bd",
    "Above $500 M" = "#08519c")) +
  theme(panel.background = element_rect(fill = NA, 
                                        color = NA)) +
  coord_equal() +
  theme(legend.direction = "horizontal",
        legend.position = "bottom") +
  guides(fill = guide_legend(title = "Millions of 2012 dollars:",
                             title.position = "top"))

#Saving maps ----
### Note: Check the code right below for high resolution maps

### Low resolution maps
png(file = "output/low_resolution_maps/US_states_IL_comp_impo.png",
    width = 768,
    height = 458.88,
    units = "px")
ggarrange(left_map, 
          right_map, 
          ncol = 2,
          hjust = 0,
          vjust = 0,
          labels = "U.S. Crop Growers' Forecasted Profit under Different Middle of \n the Century Drought Conditions",
          legend = "none") %>%
  gridExtra::grid.arrange(get_legend(left_map),
                          heights = unit(c(80, 5), "mm"))
dev.off()


### High resolution maps
pdf(file = "output/figs/US_states_IL_comp_impo.pdf",
    width = 8,
    height = 4.78)
ggarrange(left_map, 
          right_map, 
          ncol = 2,
          hjust = 0,
          vjust = 0,
          labels = "U.S. Crop Growers' Forecasted Profit under Different Middle of \n the Century Drought Conditions",
          legend = "none") %>%
  gridExtra::grid.arrange(get_legend(left_map),
                          heights = unit(c(80, 5), "mm"))
dev.off()
#END