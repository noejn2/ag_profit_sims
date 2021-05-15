rm(list = ls())

# Sets-up datasets for maps
options(scipen = 999)

library(tidyverse)
library(ggpubr)

USmap_state_df   <- readRDS(file = 'data/state_sims_map_df.rds')
USmap_county_df  <- readRDS(file = 'data/county_sims_map_df.rds')

# Checking data summary to create the breaks ----
summary(USmap_state_df$forecast_notrade)
summary(USmap_state_df$forecast_trade)
last_row <- USmap_state_df[length(USmap_state_df$long),]
last_row$forecast_trade <- 1501
USmap_state_df <- rbind(USmap_state_df, last_row)
last_row$forecast_trade <- -751
USmap_state_df <- rbind(USmap_state_df, last_row)

breaks = c(-Inf, -1500, -750, 0, 750, 1500, Inf)
labels = c("Below -$1,500 M",
           "-$1,500 M and  -$750 M",
           "-$  750 M and      0 M",
           " $    0 M and   $750 M",
           " $  750 M and $1,500 M",
           "Above $1,500 M")

# Building left map (no trade) -----
left_map <- ggplot() +
  geom_polygon(data = USmap_state_df,
               aes(x = long,
                   y = lat,
                   group = group,
                   fill = cut(forecast_notrade,
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
    title = "Gain/Loss without trade"
  ) +
  scale_fill_manual(values   = c(
    "Below -$1,500 M"        = "#b2182b",
    "-$1,500 M and  -$750 M" = "#ef8a62",
    "-$  750 M and      0 M" = "#fddbc7",
    " $    0 M and   $750 M" = "#d1e5f0",
    " $  750 M and $1,500 M" = "#67a9cf",
    "Above $1,500 M"         = "#2166ac")) +
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
                   fill = cut(forecast_trade,
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
    title = "Gain/Loss with trade"
  ) +
  scale_fill_manual(values   = c(
    "Below -$1,500 M"        = "#b2182b",
    "-$1,500 M and  -$750 M" = "#ef8a62",
    "-$  750 M and      0 M" = "#fddbc7",
    " $    0 M and   $750 M" = "#d1e5f0",
    " $  750 M and $1,500 M" = "#67a9cf",
    "Above $1,500 M"         = "#2166ac")) +
  theme(panel.background = element_rect(fill = NA, 
                                        color = NA)) +
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
png(file = "output/low_resolution_maps/US_states.png",
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
  gridExtra::grid.arrange(get_legend(right_map),
                          heights = unit(c(80, 5), "mm"))
dev.off()


### High resolution maps
pdf(file = "output/figs/US_states.pdf",
    width = 8,
    height = 4.78)
ggarrange(left_map, 
          right_map, 
          ncol = 2,
          hjust = 0,
          vjust = 0,
          labels = "U.S. Crop Growers' Forecasted Profit to the Middle of the Century",
          legend = "none") %>%
  gridExtra::grid.arrange(get_legend(right_map),
                          heights = unit(c(80, 5), "mm"))
dev.off()
#END