# install packages
#install.packages(c("easypackages", "here", "ggplot2", "dplyr", "scales", "stringr"))

# load libraries
library(easypackages)
libraries("here", "ggplot2", "dplyr", "scales", "stringr")

#### DIRECTORIES ####
# working directory
setwd("C:/Users/court/Documents/Bertarelli_Data_Manager_Forms/Indo_Pacific/Bertarelli_Nutrientscape/")
library(here)
#set_here() # run if setting up for the first time
here::i_am(".here")
here::here() # check where we are according to the here package,is it correct?

######## OCTOBER UPDATES #######
# read in the seabird energy data (at the island level)
bird = read.csv(here("Source_Data", "Seabirds", "Fig1_seabirds_by_island.csv"))
# assign rat treatment values
bird = bird %>%
  mutate(Treatment = case_when(
    Island %in% c("Aie", "Aride", "Cousine", "Fregate", "Grande Coquillage",
                  "Honuea", "Ile de la Passe", "Ile Longue", "Middle Brother",
                  "Nelson", "Reiono", "South Brother", "Tahuna Iti") ~ "Rats Absent",
    Island %in% c("Auroa", "Eagle", "Felicite", "Grande Mapou", "Hiranae",
                  "Ile Anglaise (PB)", "Ile Anglaise (SAL)", "Ile Fouquet",
                  "Ile Poule", "Onetahi", "Oroatera", "Rimatuu", "Tauini",
                  "Tiaraunu", "Yeye") ~ "Rats Present",
    TRUE ~ NA_character_))

# arrange treatment factors
bird = bird %>%
  mutate(Treatment = factor(Treatment, levels = c("Rats Absent", "Rats Present"))) %>%
  arrange(Treatment == "Rats Present")

# add the habitat cover data for each island
habitat = read.csv(here("Source_Data", "Islands", "HabitatCover_Chagos_Tetiaroa_Seychelles.csv")) %>%
  mutate(Island = str_replace_all(Island, "_", " "))

# update names to match those in the bird dataframe
habitat = habitat %>%
  # exclude islands that we aren't using for this paper 
  # (they don't have matching marine and terrestrial survey sites)
  filter(!Island %in% c("East Island", "Mapou", "Rahi", "Petite Bois Mangue")) %>%
  mutate(Island = case_when(
    # map specific islands to new names
    Island == "Eagle Island" ~ "Eagle",
    Island == "Nelson's Island" ~ "Nelson",
    # distinguish Anglaise based on Atoll context
    (Atoll == "Peros Banhos" & Island == "Anglaise") ~ "Ile Anglaise (PB)",
    Island == "Grand Coquillage" ~ "Grande Coquillage",
    Island == "Longue" ~ "Ile Longue",
    Island == "Grande Ile Mapou" ~ "Grande Mapou",
    Island == "Poule" ~ "Ile Poule",
    # distinguish Anglaise based on Atoll context
    (Atoll == "Salomon Islands" & Island == "Anglaise") ~ "Ile Anglaise (SAL)",
    Island == "Fouquet" ~ "Ile Fouquet",
    Island == "Passe" ~ "Ile de la Passe",
    Island == "Ahuroa" ~ "Auroa",
    Island == "Hiraanae" ~ "Hiranae",
    Island == "Horotera" ~ "Oroatera",
    Island == "Iti" ~ "Tahuna Iti",
    TRUE ~ Island)) # retain original entry if it doesn’t match any condition

# combine the data for use in biplots
plot_data = left_join(bird, habitat)

# set plotting margins - par(mar = c(bottom, left, top, right))
par(mar = c(1, 1, 1, 2))

# isolate plot data for each atoll/region
tetiaroa = plot_data %>%
  filter(Island %in% c("Aie", "Honuea", "Reiono", "Tahuna Iti", "Auroa", "Hiranae", 
                      "Onetahi", "Oroatera", "Rimatuu", "Tauini", "Tiaraunu"))
seychelles = plot_data %>%
  filter(Island %in% c("Aride", "Cousine", "Fregate", "Felicite"))

chagos = plot_data %>%
  filter(Island %in% c("Grande Coquillage", "Ile de la Passe","Ile Longue",
                       "Middle Brother","Nelson", "South Brother", "Eagle", 
                       "Grande Mapou", "Ile Anglaise (PB)", "Ile Anglaise (SAL)",
                       "Ile Fouquet", "Ile Poule", "Yeye"))

# read in the region-specific model fit for tetiaroa
tetiaroa_estimates = read.csv(here("Source_Data", "Seabirds", "Fig1_tetiaroa_estimated_lines.csv"))
tetiaroa_estimates = tetiaroa_estimates %>%
  mutate(seabird_energy_unscale = seabird_energy_unscale * 1000)

# create the tetiaroa biplot
tetiaroa_plot = 
  ggplot() +
  # adjust the size of triangles for "Rats Present" to keep them smaller than circles/squares
  geom_jitter(data = subset(tetiaroa, Treatment == "Rats Present"),
              aes(x = SeabirdEnergyRequirements_MJha, 
                  y = Ninput_kgha, 
                  shape = Treatment, 
                  fill = Native_percent), 
              size = 7, color = "black", stroke = 0.7,
              height = 10, width = 1) + 
  geom_jitter(data = subset(tetiaroa, Treatment != "Rats Present"),
              aes(x = SeabirdEnergyRequirements_MJha, 
                  y = Ninput_kgha, 
                  shape = Treatment, 
                  fill = Native_percent), 
              size = 8, color = "black", stroke = 0.7,
              height = 10, width = 1) + 
  # add line and uncertainty shading from tetiaroa_estimates
  geom_line(data = tetiaroa_estimates, 
            aes(x = seabird_energy_unscale, y = estimate__), lwd = 1, colour = "gray1") +
  geom_ribbon(data = tetiaroa_estimates, 
              aes(x = seabird_energy_unscale, y = estimate__, ymin = lower__, ymax = upper__), 
              alpha = 0.2, fill = "gray1") +
  # set shape and fill scales
  scale_shape_manual(values = c("Rats Present" = 24,  # triangle
                                "Rats Absent" = 21 )) +  # circle
  scale_fill_gradientn(
    colors = c("#b7efc5", "#6ede8a", "#25a244", "#1a7431", "#10451d"),
    values = c(0, 0.2, 0.4, 0.6, 0.8, 1),
    limits = c(0, 100)) + 
  # labels and theme adjustments
  labs(x = bquote("Seabird energy flow (MJ ha"^-1~"yr"^-1*")"),
       y = bquote("Seabird N input (kg ha"^-1~"yr"^-1*")")) +
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(), 
        axis.line = element_line(color = "black"), 
        plot.title = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(size = 20, color = "black"),
        axis.title.x = element_text(size = 22, color = "black"),
        axis.text.y = element_text(size = 20, color = "black", angle = 90),
        axis.title.y = element_text(size = 22, color = "black"))

tetiaroa_plot

# read in the region-specific model fit for chagos
chagos_estimates = read.csv(here("Source_Data", "Seabirds", "Fig1_chagos_estimated_lines.csv"))
chagos_estimates = chagos_estimates %>%
  mutate(seabird_energy_unscale = seabird_energy_unscale * 1000)

# create the chagos biplot
chagos_plot = 
  ggplot() +
  # adjust the size of triangles for "Rats Present" to keep them smaller than circles/squares
  geom_jitter(data = subset(chagos, Treatment == "Rats Present"),
              aes(x = SeabirdEnergyRequirements_MJha, 
                  y = Ninput_kgha, 
                  shape = Treatment, 
                  fill = Native_percent), 
              size = 7, color = "black", stroke = 0.7,
              height = 10, width = 1) + 
  geom_jitter(data = subset(chagos, Treatment != "Rats Present"),
              aes(x = SeabirdEnergyRequirements_MJha, 
                  y = Ninput_kgha, 
                  shape = Treatment, 
                  fill = Native_percent), 
              size = 8, color = "black", stroke = 0.7,
              height = 10, width = 1) + 
  # add line and uncertainty shading from chagos_estimates
  geom_line(data = chagos_estimates, 
            aes(x = seabird_energy_unscale, y = estimate__), lwd = 1, colour = "gray1") +
  geom_ribbon(data = chagos_estimates, 
              aes(x = seabird_energy_unscale, y = estimate__, ymin = lower__, ymax = upper__), 
              alpha = 0.2, fill = "gray1") +
  # set shape and fill scales
  scale_shape_manual(values = c("Rats Present" = 24,  # triangle
                                "Rats Absent" = 21 )) +  # circle
  scale_fill_gradientn(
    colors = c("#b7efc5", "#6ede8a", "#25a244", "#1a7431", "#10451d"),
    values = c(0, 0.2, 0.4, 0.6, 0.8, 1),
    limits = c(0, 100)) + 
  # labels and theme adjustments
  labs(x = bquote("Seabird energy flow (MJ ha"^-1~"yr"^-1*")"),
       y = bquote("Seabird N input (kg ha"^-1~"yr"^-1*")")) +
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(), 
        axis.line = element_line(color = "black"), 
        plot.title = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(size = 20, color = "black"),
        axis.title.x = element_text(size = 22, color = "black"),
        axis.text.y = element_text(size = 20, color = "black", angle = 90),
        axis.title.y = element_text(size = 22, color = "black"))

chagos_plot

# read in the region-specific model fit for seychelles
seychelles_estimates = read.csv(here("Source_Data", "Seabirds", "Fig1_seychelles_estimated_lines.csv"))
seychelles_estimates = seychelles_estimates %>%
  mutate(seabird_energy_unscale = seabird_energy_unscale * 1000)

# create the seychelles biplot
seychelles_plot = 
  ggplot() +
  # adjust the size of triangles for "Rats Present" to keep them smaller than circles/squares
  geom_point(data = subset(seychelles, Treatment == "Rats Present"),
              aes(x = SeabirdEnergyRequirements_MJha, 
                  y = Ninput_kgha, 
                  shape = Treatment, 
                  fill = Native_percent), 
              size = 7, color = "black", stroke = 0.7) + 
  geom_point(data = subset(seychelles, Treatment != "Rats Present"),
              aes(x = SeabirdEnergyRequirements_MJha, 
                  y = Ninput_kgha, 
                  shape = Treatment, 
                  fill = Native_percent), 
              size = 8, color = "black", stroke = 0.7) + 
  # add line and uncertainty shading from seychelles_estimates
  geom_line(data = seychelles_estimates, 
            aes(x = seabird_energy_unscale, y = estimate__), lwd = 1, colour = "gray1") +
  geom_ribbon(data = seychelles_estimates, 
              aes(x = seabird_energy_unscale, y = estimate__, ymin = lower__, ymax = upper__), 
              alpha = 0.2, fill = "gray1") +
  # set shape and fill scales
  scale_shape_manual(values = c("Rats Present" = 24,  # triangle
                                "Rats Absent" = 21 )) +  # circle
  scale_fill_gradientn(
    colors = c("#b7efc5", "#6ede8a", "#25a244", "#1a7431", "#10451d"),
    values = c(0, 0.2, 0.4, 0.6, 0.8, 1),
    limits = c(0, 100)) + 
  # labels and theme adjustments
  labs(x = bquote("Seabird energy flow (MJ ha"^-1~"yr"^-1*")"),
       y = bquote("Seabird N input (kg ha"^-1~"yr"^-1*")")) +
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(), 
        axis.line = element_line(color = "black"), 
        plot.title = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(size = 20, color = "black"),
        axis.title.x = element_text(size = 22, color = "black"),
        axis.text.y = element_text(size = 20, color = "black", angle = 90),
        axis.title.y = element_text(size = 22, color = "black"))

seychelles_plot

# apply standard scale to all biplots
tetiaroa_plot_scaled = tetiaroa_plot + ylim(-20, 400) + 
  scale_x_continuous(limits = c(-10000, 210000 ),
                     breaks = seq(0, (210000), by = 50000),
                     labels = scales::comma_format())
tetiaroa_plot_scaled

chagos_plot_scaled = chagos_plot + ylim(-20, 400) + 
  scale_x_continuous(limits = c(-10000, 210000 ),
                     breaks = seq(0, (210000), by = 50000),
                     labels = scales::comma_format())
chagos_plot_scaled

seychelles_plot_scaled = seychelles_plot + ylim(-20, 400) + 
  scale_x_continuous(limits = c(-10000, 210000 ),
                     breaks = seq(0, (210000), by = 50000),
                     labels = scales::comma_format())
seychelles_plot_scaled

# save biplots for inclusion in the bottom row of Figure 1
ggsave(here("Figures", "Tetiaroa_Figure1_Biplot_v3.png"),
       plot = tetiaroa_plot_scaled, width = 6, height = 6, dpi = 600)
ggsave(here("Figures", "Chagos_Figure1_Biplot_v3.png"),
       plot = chagos_plot_scaled, width = 6, height = 6, dpi = 600)
ggsave(here("Figures", "Seychelles_Figure1_Biplot_v3.png"),
       plot = seychelles_plot_scaled, width = 6, height = 6, dpi = 600)

# saving again, but with different size
ggsave(here("Figures", "Tetiaroa_Figure1_Biplot_v3_Wide.png"),
       plot = tetiaroa_plot_scaled, width = 7.5, height = 6, dpi = 600)
ggsave(here("Figures", "Chagos_Figure1_Biplot_v3_Wide.png"),
       plot = chagos_plot_scaled, width = 7.5, height = 6, dpi = 600)
ggsave(here("Figures", "Seychelles_Figure1_Biplot_v3_Wide.png"),
       plot = seychelles_plot_scaled, width = 7.5, height = 6, dpi = 600)


# add in the coordinates for maping
gps = read.csv(here("Source_Data", "GPS_Points", "Study_Islands.csv"))
tetiaroa_gps = left_join(tetiaroa, gps, by = "Island") %>%
  filter(Island %in% tetiaroa$Island) %>%
  mutate(Atoll = Atoll.y,
         Region = Region.y,
         Treatment = Treatment.x) %>%
  select(-Atoll.x, -Atoll.y, -Region.x, -Region.y, -Treatment.x, -Treatment.y) %>%
  relocate(Atoll, .before = Island) %>%
  relocate(Region, .before = Atoll) %>%
  relocate(Treatment, .after = Island)

chagos_gps = left_join(chagos, gps, by = "Island") %>%
  filter(Island %in% chagos$Island) %>%
  mutate(Atoll = Atoll.y,
         Region = Region.y,
         Treatment = Treatment.x) %>%
  select(-Atoll.x, -Atoll.y, -Region.x, -Region.y, -Treatment.x, -Treatment.y) %>%
  relocate(Atoll, .before = Island) %>%
  relocate(Region, .before = Atoll) %>%
  relocate(Treatment, .after = Island)

seychelles_gps = left_join(seychelles, gps, by = "Island") %>%
  filter(Island %in% seychelles$Island) %>%
  mutate(Atoll = Atoll.y,
         Region = Region.y,
         Treatment = Treatment.x) %>%
  select(-Atoll.x, -Atoll.y, -Region.x, -Region.y, -Treatment.x, -Treatment.y) %>%
  relocate(Atoll, .before = Island) %>%
  relocate(Region, .before = Atoll) %>%
  relocate(Treatment, .after = Island)

# save the map data for each region
write.csv(seychelles_gps, 
          here("Source_Data", "GPS_Points", "Seychelles_Figure1_Map_Data.csv"),
          row.names = FALSE)
write.csv(chagos_gps,
          here("Source_Data", "GPS_Points", "Chagos_Figure1_Map_Data.csv"),
          row.names = FALSE)
write.csv(tetiaroa_gps,
          here("Source_Data", "GPS_Points", "Tetiaroa_Figure1_Map_Data.csv"),
          row.names = FALSE)

# save R data
save.image(here("Seabird_Energy_Biplots.RData"))


########### ARCHIVED ############
# #### data prep ####
# # read in the seabird energy data (at the species level, shared originally by Ruth Dunn as Seabirds_Energy_Nitrogen.csv)
# bird = read.csv(here("Source_Data", "Seabirds", "Seabirds_Energy_Nitrogen.csv"))
# 
# # some quick fixes to names
# bird = bird %>%
#   mutate(Atoll = str_replace_all(Atoll, "_", " ")) %>%
#   mutate(Island = str_replace_all(Island, "_", " ")) %>%
#   mutate(Island = str_to_title(Island)) # 
# 
# # summarize the data to be at an island level, rather than species level
# summary_bird = bird %>%
#   group_by(Region, Atoll, Island) %>%
#   mutate(TotalDensity_birdsha = sum(Density_birdsha)) %>%
#   mutate(TotalSeabirdEnergyReq_MJha = sum(SeabirdEnergyRequirements_MJha)) %>%
#   mutate(TotalNinput_kgha = sum(Ninput_kgha)) %>%
#   select(Region, Atoll, Island, TotalDensity_birdsha, TotalSeabirdEnergyReq_MJha, TotalNinput_kgha) %>%
#   distinct()
# 
# # isolate tetiaroa islands where we have marine and/or forest data
# tetiaroa = summary_bird %>%
#   filter(Atoll == "Tetiaroa")
# 
# # distinguish between rats present, absent, and eradicated
# tetiaroa = tetiaroa %>% 
#   filter(Island != "Rahi") %>%
#   mutate(Treatment = case_when(
#     Island %in% c("Aie", "Iti") ~ "Rats Absent",
#     Island %in% c("Honuea", "Tauini", "Tiaraunu", "Ahuroa", "Hiraanae",
#                   "Horotera", "Rimatuu") ~ "Rats Present",
#     Island %in% c("Reiono", "Onetahi") ~ "Rats Eradicated",
#     TRUE ~ NA_character_  # If none of the conditions are met, return NA
#   ))
# 
# # isolate chagos islands where we have marine and/or forest data
# chagos = summary_bird %>%
#   filter(Region == "Chagos Archipelago") %>%
#   mutate(Atoll = case_when(
#     Atoll == "Great Chagos bank" ~ "Great Chagos Bank",
#     TRUE ~ Atoll))
# 
# # Diego Garcia
# dg = chagos %>%
#   filter(Atoll == "Diego Garcia") %>%
#   filter(Island %in% 
#            c("East Island", "Diego Garcia")) %>%
#   mutate(Treatment = case_when(
#     Island == "East Island" ~ "Rats Absent",
#     Island == 'Diego Garcia' ~ "Rats Present",
#     TRUE ~ NA_character_))
# 
# # Peros Banhos
# pb = chagos %>%
#   filter(Atoll == "Peros Banhos") %>%
#   filter(Island %in%
#            c("Anglaise", "Grande Mapou", "Grand Coquillage", "Longue",
#              "Petite Bois Mangue", "Poule", "Yeye")) %>%
#   mutate(Treatment = case_when(
#     Island %in% c("Grand Coquillage", "Longue", "Petite Bois Mangue") ~ "Rats Absent",
#     Island %in% c("Anglaise", "Grande Mapou", "Poule", "Yeye") ~ "Rats Present",
#     TRUE ~ NA_character_))
# 
# # Great Chagos Bank
# gcb = chagos %>%
#   filter(Atoll == "Great Chagos Bank") %>%
#   filter(Island %in% 
#            c("Eagle Island", "Middle Brother", "Nelson's Island", "South Brother")) %>%
#   mutate(Treatment = case_when(
#     Island %in% c("Middle Brother", "Nelson's Island", "South Brother") ~ "Rats Absent",
#     Island == "Eagle Island" ~ "Rats Present",
#     TRUE ~ NA_character_))
# 
# # Salomon
# sal = chagos %>%
#   filter(Atoll == "Salomon Islands") %>%
#   filter(Island %in%
#            c("Anglaise", "Fouquet", "Mapou", "Passe")) %>%
#   mutate(Treatment = case_when(
#     Island %in% c("Mapou", "Passe") ~ "Rats Absent",
#     Island %in% c("Anglaise", "Fouquet") ~ "Rats Present",
#     TRUE ~ NA_character_))
# 
# # recombine chagos data into a single data frame
# chagos = rbind(dg, pb, gcb, sal)
# 
# # isolate seychelles islands where we have marine and/or forest data
# seychelles = summary_bird %>%
#   filter(Region == "Seychelles")
# 
# # add felicite data (no birds)
# seychelles = rbind(seychelles, 
#                    data.frame(Region = "Seychelles", 
#                               Atoll = NA_character_,
#                               Island = "Felicite",
#                               TotalDensity_birdsha = 0,
#                               TotalSeabirdEnergyReq_MJha = 0,
#                               TotalNinput_kgha = 0))
# 
# # distinguish between rats present, absent, and eradicated
# seychelles = seychelles %>% 
#   mutate(Treatment = case_when(
#     Island %in% c("Cousine", "Aride") ~ "Rats Absent",
#     Island == "Felicite" ~ "Rats Present",
#     Island == "Fregate" ~ "Rats Eradicated",
#     TRUE ~ NA_character_))
# 
# # add the habitat cover data
# habitat = read.csv(here("Source_Data", "Islands", "HabitatCover_Chagos_Tetiaroa_Seychelles.csv")) %>%
#   mutate(Island = str_replace_all(Island, "_", " "))
# 
# # adding the habitat data for each region
# tetiaroa = left_join(tetiaroa,
#                      filter(habitat, Region == "Tetiaroa"),
#                      by = c("Region", "Island")) %>%
#   rename(Atoll = Atoll.x) %>%
#   select(-Atoll.y)
# 
# chagos = left_join(chagos,
#                    (habitat %>%
#                       mutate(Island = if_else(
#                         Region == "Chagos" & Atoll == "Peros Banhos" & Island == "Grande Ile Mapou",
#                         "Grande Mapou",  # Change Island to "Grande Mapou"
#                         Island  # Otherwise, keep the original value
#                       )) %>%
#                       mutate(Region = "Chagos Archipelago")),
#                    by = c("Region", "Atoll", "Island"))
# 
# seychelles = left_join(seychelles, habitat,
#                        by = c("Region", "Atoll", "Island"))
# 
# #### biplots ####
# # ensure the Treatment factor levels are ordered so "Rats Present" is plotted last (on top)
# tetiaroa = tetiaroa %>%
#   mutate(Treatment = factor(Treatment, levels = c("Rats Absent", "Rats Eradicated", "Rats Present"))) %>%
#   arrange(Treatment == "Rats Present")
# chagos = chagos %>%
#   mutate(Treatment = factor(Treatment, levels = c("Rats Absent", "Rats Present"))) %>%
#   arrange(Treatment == "Rats Present")
# seychelles = seychelles %>%
#   mutate(Treatment = factor(Treatment, levels = c("Rats Absent", "Rats Eradicated", "Rats Present"))) %>%
#   arrange(Treatment == "Rats Present")
# 
# # set plotting margins - par(mar = c(bottom, left, top, right))
# par(mar = c(1, 1, 1, 2))
# 
# # create biplots
# tet_plot = 
#   ggplot(tetiaroa,
#          aes(x = TotalSeabirdEnergyReq_MJha, 
#              y = TotalNinput_kgha, 
#              shape = Treatment,  # shape based on Treatment
#              fill = Native_percent)) +  # fill based on Native_percent
#   geom_smooth(method = "lm", se = TRUE, linewidth = 0.7,
#               aes(group = 1), color = "black") +
#   # make the triangles smaller because they appear larger relative to circle/square
#   geom_point(data = subset(tetiaroa, Treatment == "Rats Present"),
#              aes(shape = Treatment), size = 7, color = "black", stroke = 0.7) + 
#   geom_point(data = subset(tetiaroa, Treatment != "Rats Present"),
#              aes(shape = Treatment), size = 8, color = "black", stroke = 0.7) +  
#   scale_shape_manual(values = c("Rats Present" = 24,  # triangle
#                                 "Rats Absent" = 21,   # circle
#                                 "Rats Eradicated" = 22)) +  # square
#   scale_fill_gradientn(
#     colors = c("#b7efc5", "#6ede8a", "#25a244", "#1a7431", "#10451d"),
#     values = c(0, 0.2, 0.4, 0.6, 0.8, 1),
#     limits = c(0, 100)) + 
#   labs(x = "Total seabird energy requirements (MJ/ha)",
#        y = "Total N input (kg/ha)") +
#   theme(panel.background = element_blank(), 
#         panel.grid = element_blank(), 
#         axis.line = element_line(color = "black"), 
#         plot.title = element_blank(),
#         legend.position = "none",
#         axis.text.x = element_text(size = 20, color = "black"),
#         axis.title.x = element_text(size = 22, color = "black"),
#         axis.text.y = element_text(size = 20, color = "black", angle = 90),
#         axis.title.y = element_text(size = 22, color = "black"))
# 
# tet_plot
# 
# chagos_plot = 
#   ggplot(chagos,
#          aes(x = TotalSeabirdEnergyReq_MJha, 
#              y = TotalNinput_kgha, 
#              shape = Treatment,  # shape based on Treatment
#              fill = Native_percent)) +  # fill based on Native_percent
#   geom_smooth(method = "lm", se = TRUE, linewidth = 0.7,
#               aes(group = 1), color = "black") +
#   # make the triangles smaller because they appear larger relative to circle/square
#   geom_point(data = subset(chagos, Treatment == "Rats Present"),
#              aes(shape = Treatment), size = 7, color = "black", stroke = 0.7) + 
#   geom_point(data = subset(chagos, Treatment != "Rats Present"),
#              aes(shape = Treatment), size = 8, color = "black", stroke = 0.7) +  
#   scale_shape_manual(values = c("Rats Present" = 24,  # triangle
#                                 "Rats Absent" = 21,   # circle
#                                 "Rats Eradicated" = 22)) +  # square
#   scale_fill_gradientn(
#     colors = c("#b7efc5", "#6ede8a", "#25a244", "#1a7431", "#10451d"),
#     values = c(0, 0.2, 0.4, 0.6, 0.8, 1),
#     limits = c(0, 100)) + 
#   labs(x = "Total seabird energy requirements (MJ/ha)",
#        y = "Total N input (kg/ha)") +
#   theme(panel.background = element_blank(), 
#         panel.grid = element_blank(), 
#         axis.line = element_line(color = "black"), 
#         plot.title = element_blank(),
#         legend.position = "none",
#         axis.text.x = element_text(size = 20, color = "black"),
#         axis.title.x = element_text(size = 22, color = "black"),
#         axis.text.y = element_text(size = 20, color = "black", angle = 90),
#         axis.title.y = element_text(size = 22, color = "black"))
# chagos_plot
# 
# sey_plot = 
#   ggplot(seychelles,
#          aes(x = TotalSeabirdEnergyReq_MJha, 
#              y = TotalNinput_kgha, 
#              shape = Treatment,  # shape based on Treatment
#              fill = Native_percent)) +  # fill based on Native_percent
#   geom_smooth(method = "lm", se = TRUE, linewidth = 0.7,
#               aes(group = 1), color = "black") +
#   # make the triangles smaller because they appear larger relative to circle/square
#   geom_point(data = subset(seychelles, Treatment == "Rats Present"),
#              aes(shape = Treatment), size = 7, color = "black", stroke = 0.7) + 
#   geom_point(data = subset(seychelles, Treatment != "Rats Present"),
#              aes(shape = Treatment), size = 8, color = "black", stroke = 0.7) +  
#   scale_shape_manual(values = c("Rats Present" = 24,  # triangle
#                                 "Rats Absent" = 21,   # circle
#                                 "Rats Eradicated" = 22)) +  # square
#   scale_fill_gradientn(
#     colors = c("#b7efc5", "#6ede8a", "#25a244", "#1a7431", "#10451d"),
#     values = c(0, 0.2, 0.4, 0.6, 0.8, 1),
#     limits = c(0, 100)) +  
#   labs(x = "Total seabird energy requirements (MJ/ha)",
#        y = "Total N input (kg/ha)") +
#   theme(panel.background = element_blank(), 
#         panel.grid = element_blank(), 
#         axis.line = element_line(color = "black"), 
#         plot.title = element_blank(),
#         legend.position = "none",
#         axis.text.x = element_text(size = 20, color = "black"),
#         axis.title.x = element_text(size = 22, color = "black"),
#         axis.text.y = element_text(size = 20, color = "black", angle = 90),
#         axis.title.y = element_text(size = 22, color = "black"))
# sey_plot
# 
# # define max x and y axis values for standardized plotting
# xmax = max(c(tetiaroa$TotalSeabirdEnergyReq_MJha, 
#              chagos$TotalSeabirdEnergyReq_MJha, 
#              seychelles$TotalSeabirdEnergyReq_MJha),
#            na.rm = TRUE)
# 
# ymax = max(c(tetiaroa$TotalNinput_kgha, 
#              chagos$TotalNinput_kgha, 
#              seychelles$TotalNinput_kgha),
#            na.rm = TRUE) + 50 # add a small buffer to the max y-value for plotting
# 
# # apply standard scale to all biplots
# tet_plot_scaled = tet_plot + ylim(-5, ymax) + 
#   scale_x_continuous(limits = c(0, xmax),
#                      breaks = seq(0, (xmax), by = 50000),
#                      labels = scales::comma_format())
# tet_plot_scaled
# 
# chagos_plot_scaled = chagos_plot + ylim(-5, ymax) + 
#   scale_x_continuous(limits = c(0, xmax),
#                      breaks = seq(0, (xmax), by = 50000),
#                      labels = scales::comma_format())
# chagos_plot_scaled
# 
# sey_plot_scaled = sey_plot + ylim(-5, ymax) + 
#   scale_x_continuous(limits = c(0, xmax),
#                      breaks = seq(0, (xmax), by = 50000),
#                      labels = scales::comma_format())
# sey_plot_scaled
# 
# # save biplots for inclusion in the bottom row of Figure 1
# ggsave(here("Figures", "Tetiaroa_Figure1_Biplot_v3.png"),
#        plot = tet_plot_scaled, width = 6, height = 6, dpi = 600)
# ggsave(here("Figures", "Chagos_Figure1_Biplot_v3.png"),
#        plot = chagos_plot_scaled, width = 6, height = 6, dpi = 600)
# ggsave(here("Figures", "Seychelles_Figure1_Biplot_v3.png"),
#        plot = sey_plot_scaled, width = 6, height = 6, dpi = 600)
# 
# 
# ggsave(here("Figures", "Tetiaroa_Figure1_Biplot_v3_Wide.png"),
#        plot = tet_plot_scaled, width = 7.5, height = 6, dpi = 600)
# ggsave(here("Figures", "Chagos_Figure1_Biplot_v3_Wide.png"),
#        plot = chagos_plot_scaled, width = 7.5, height = 6, dpi = 600)
# ggsave(here("Figures", "Seychelles_Figure1_Biplot_v3_Wide.png"),
#        plot = sey_plot_scaled, width = 7.5, height = 6, dpi = 600)
# 
# ### map data ####
# gps = read.csv(here("Source_Data", "GPS_Points", "Study_Islands.csv"))
# 
# seychelles_gps = left_join(seychelles, gps)
# chagos_gps = left_join(chagos, gps)
# tetiaroa_gps = left_join(tetiaroa, gps, by = c("Atoll", "Island")) %>%
#   mutate(Region = Region.y) %>%
#   relocate(Region, .before = Atoll) %>%
#   select(-Region.x, -Region.y)
# write.csv(seychelles_gps, 
#           here("Source_Data", "GPS_Points", "Seychelles_Figure1_Map_Data.csv"),
#           row.names = FALSE)
# write.csv(chagos_gps,
#           here("Source_Data", "GPS_Points", "Chagos_Figure1_Map_Data.csv"),
#           row.names = FALSE)
# write.csv(tetiaroa_gps,
#           here("Source_Data", "GPS_Points", "Tetiaroa_Figure1_Map_Data.csv"),
#           row.names = FALSE)