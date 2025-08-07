# install packages
#install.packages(c("easypackages", "here", "ggplot2", "dplyr", "scales", "stringr", "tidyverse"))

# load libraries
library(easypackages)
libraries("here", "ggplot2", "dplyr", "scales", "stringr", "tidyverse")

#### DIRECTORIES ####
# working directory
setwd("C:/Users/court/Documents/Bertarelli_Data_Manager_Forms/Indo_Pacific/Bertarelli_Nutrientscape/")
library(here)
#set_here() # run if setting up for the first time
here::i_am(".here")
here::here() # check where we are according to the here package,is it correct?

#### load data ####
df = read.csv(here("Source_Data", "Seabirds", "Seabirds_Foraging_Energy.csv")) %>%
  dplyr::rename(foraging_range = Mean_max_foraging_dist_km) %>%
  rename(energy_requirements = Total_SeabirdEnergyRequirements_MJ) %>%
  rename(region = Region)

# summarise by region and foraging range:
df_summary = df %>%
  group_by(region, foraging_range) %>%
  summarize(total_energy_requirements = sum(energy_requirements)) %>%
  mutate(total_energy_requirements = total_energy_requirements/10^6)

# create complete grid of all regions & foraging range combos:
regions = unique(df$region)
foraging_ranges = seq(min(df$foraging_range), max(df$foraging_range), by = 1)
complete_grid = expand.grid(region = regions, foraging_range = foraging_ranges)

# merge with summarised data:
df_merged = complete_grid %>%
  left_join(df_summary, by = c("region", "foraging_range")) %>%
  replace_na(list(total_energy_requirements = 0))

# calculate the cumulative sum of energy requirements:
df_cumu = df_merged %>%
  group_by(region) %>%
  arrange(-foraging_range) %>%
  mutate(cumulative_energy_requirements = cumsum(total_energy_requirements)) %>%
  mutate(cumulative_energy_requirements = ifelse(cumulative_energy_requirements == 0,
                                             NA, cumulative_energy_requirements))

#### create heatmaps ####
##### Seychelles #####
Seychelles = ggplot(filter(df_cumu, region == "Seychelles"), 
       aes(y = foraging_range, x = region, fill = cumulative_energy_requirements)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("#FF9F00", "#FF8200", "#FE6100", "#F34D42", "#E63768"),
                       na.value = NA, guide = guide_legend(reverse = T)) +
  coord_flip() +
  labs(y = "Seabird foraging range (km)",
       x = "",
       fill = expression("Total seabird energy requirements (MJ x 10"^6*")")) +
  theme_bw() +
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(), 
        axis.line = element_line(color = "black"),  
        legend.position = "bottom", 
        axis.ticks.y = element_blank(), 
        axis.text.y = element_blank(),  
        axis.title.y = element_blank(),
        plot.title = element_blank())


# save the legend object for use in later multi-panel plot
legend = ggplotGrob(Seychelles)$grobs[[which(sapply(ggplotGrob(Seychelles)$grobs,
                                                       function(x) x$name) == "guide-box")]]
plot(legend)

# remove the legend 
Seychelles_NL = ggplot(filter(df_cumu, region == "Seychelles"), 
                    aes(y = foraging_range, x = region, fill = cumulative_energy_requirements)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("#FF9F00", "#FF8200", "#FE6100", "#F34D42", "#E63768"),
                                  na.value = NA, guide = guide_legend(reverse = T)) +
  coord_flip() +
  labs(y = "Seabird foraging range (km)",
       x = "",
       fill = expression("Total seabird energy requirements (MJ x 10"^6*")")) +
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(), 
        axis.line = element_line(color = "black"), 
        plot.title = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(size = 20, color = "black"),
        axis.title.x = element_text(size = 22, color = "black"),
        axis.ticks.y = element_blank(), 
        axis.text.y = element_blank(),  
        axis.title.y = element_blank())


##### Chagos ######
Chagos_NL = ggplot(filter(df_cumu, region == "Chagos Archipelago"), 
       aes(y = foraging_range, x = region, fill = cumulative_energy_requirements)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("#FF9F00", "#FF8200", "#FE6100", "#F34D42", "#E63768"),
                       na.value = NA, guide = guide_legend(reverse = T)) +
  coord_flip() +
  labs(y = "Seabird foraging range (km)",
       x = "",
       fill = expression("Total seabird energy requirements (MJ x 10"^6*")")) +
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(), 
        axis.line = element_line(color = "black"), 
        plot.title = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(size = 20, color = "black"),
        axis.title.x = element_text(size = 22, color = "black"),
        axis.ticks.y = element_blank(), 
        axis.text.y = element_blank(),  
        axis.title.y = element_blank())

##### Tetiaroa ######
Tetiaroa_NL = ggplot(filter(df_cumu, region == "Tetiaroa"), 
       aes(y = foraging_range, x = region, fill = cumulative_energy_requirements)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("#FF9F00", "#FF8200", "#FE6100", "#F34D42", "#E63768"),
                       na.value = NA, guide = guide_legend(reverse = T)) +
  coord_flip() +
  labs(y = "Seabird foraging range (km)",
       x = "",
       fill = expression("Total seabird energy requirements (MJ x 10"^6*")")) +
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(), 
        axis.line = element_line(color = "black"), 
        plot.title = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(size = 20, color = "black"),
        axis.title.x = element_text(size = 22, color = "black"),
        axis.ticks.y = element_blank(), 
        axis.text.y = element_blank(),  
        axis.title.y = element_blank())

plot(Seychelles_NL)
plot(Chagos_NL)
plot(Tetiaroa_NL)

# remove y-axis lines
Seychelles_NLNY = Seychelles_NL +
  theme(axis.line.y = element_blank())
plot(Seychelles_NLNY)

Chagos_NLNY = Chagos_NL +
  theme(axis.line.y = element_blank())
plot(Chagos_NLNY)

Tetiaroa_NLNY = Tetiaroa_NL +
  theme(axis.line.y = element_blank())
plot(Tetiaroa_NLNY)

#### save outputs ####
ggsave(here("Figures", "Seychelles_Foraging_Barplot.png"),
       plot = Seychelles_NLNY, width = 6, height = 1.5, dpi = 600)
ggsave(here("Figures", "Chagos_Foraging_Barplot.png"),
       plot = Chagos_NLNY, width = 6, height = 1.5, dpi = 600)
ggsave(here("Figures", "Tetiaroa_Foraging_Barplot.png"),
       plot = Tetiaroa_NLNY, width = 6, height = 1.5, dpi = 600)

save.image(here("Seabird_Energy_Foraging_range.RData"))
