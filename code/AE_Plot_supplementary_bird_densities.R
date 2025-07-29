library(tidyverse)
library(patchwork)

rm(list = ls(all = TRUE))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load data ####

df <- read_csv("../data//Seabirds_Energy_Nitrogen.csv") %>%
  dplyr::select(-c(SeabirdEnergyRequirements_MJha, Ninput_kgha)) %>%
  add_row(Region = "Chagos Archipelago", Island = "South Brother", Species = "Anous minutus") %>%
  add_row(Region = "Chagos Archipelago", Island = "South Brother", Species = "Onychoprion lunatus") %>%
  add_row(Region = "Seychelles", Island = "Felicite", Species = "Sula sula") %>%
  add_row(Region = "Seychelles", Island = "Felicite", Species = "Thalasseus bergii") %>%
  add_row(Region = "Seychelles", Island = "Felicite", Species = "Sula leucogaster") %>%
  add_row(Region = "Seychelles", Island = "Felicite", Species = "Sterna sumatrana") %>%
  add_row(Region = "Seychelles", Island = "Felicite", Species = "Fregata minor") %>%
  add_row(Region = "Seychelles", Island = "Felicite", Species = "Fregata ariel") %>%
  add_row(Region = "Seychelles", Island = "Felicite", Species = "Anous minutus") %>%
  add_row(Region = "Seychelles", Island = "Felicite", Species = "Onychoprion lunatus") %>%
  add_row(Region = "Tetiaroa", Island = "Iti", Species = "Sterna sumatrana") %>%
  add_row(Region = "Tetiaroa", Island = "Iti", Species = "Puffinus bailloni") %>%
  add_row(Region = "Tetiaroa", Island = "Iti", Species = "Phaethon lepturus") %>%
  add_row(Region = "Tetiaroa", Island = "Iti", Species = "Ardenna pacifica") %>%
  add_row(Region = "Tetiaroa", Island = "Iti", Species = "Anous tenuirostris") %>%
  filter(!Species %in% c("Sternula albifrons", "Sula dactylatra", "Sterna dougallii",
         "Phaethon rubricauda", "Onychoprion anaethetus")) %>%
  mutate(Island = dplyr::case_when(
    Island == "Poule" ~ "Ile Poule",
    Island == "Grand Mapou" ~ "Grande Mapou",
    Island == "Passe" ~ "Ile de la Passe",
    Island == "Fouquet" ~ "Ile Fouquet",
    Island == "Iti" ~ "Tahuna iti",
    Island == "Ahuroa" ~ "Auroa",
    Island == "Longue" ~ "Ile Longue",
    Island == "Horotera" ~ "Oroatera",
    Island == "Nelson's Island" ~ "Nelsons Island",
    Atoll == "Peros Banhos" & Island == "Anglaise" ~ "Ile Anglaise (PB)",
    Atoll == "Salomon Islands" & Island == "Anglaise" ~ "Ile Anglaise (SAL)",
    TRUE ~ Island)) %>%
  mutate(Island = dplyr::case_when(
    Island == "Aie" ~ "Ā'ie",
    Island == "Hiraanae" ~ "Hira'a'anae",
    Island == "Auroa" ~ "Ahuroa",
    Island == "Tiaraunu" ~ "Tiara'aunu",
    Island == "Honuea" ~ "Honu'ea",
    Island == "Rimatuu" ~ "Rimatu'u",
    Island == "Oroatera" ~ "Horoatera",
    Island == "Yeye" ~ "Ile Yéyé",
    TRUE ~ Island))

levels(as.factor(df$Species))
summary(df$Density_birdsha)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create the heatmap ####

g1 <- ggplot(df %>%
         filter(Region == "Seychelles"),
       aes(y = Species, x = Island, fill = Density_birdsha)) +
  geom_tile() +
  scale_fill_gradient2(high = "#DC267F", mid = "#FE6100", low = "#FFB000",
                       midpoint = 3000, limits = c(0.02,6000),
                       na.value = NA,
                       guide = guide_legend(reverse = F, nrow =  1)) +
  labs(y = "Seabird species",
       x = "Island",
       fill = "Seabird density (birds/ha)") +
  ggtitle(expression(bold("A")~"Seychelles"))


g2 <- ggplot(df %>%
         filter(Region == "Chagos Archipelago"),
       aes(y = Species, x = Island, fill = Density_birdsha)) +
  geom_tile() +
  scale_fill_gradient2(high = "#DC267F", mid = "#FE6100", low = "#FFB000",
                       midpoint = 3000, limits = c(0.02,6000),
                       na.value = NA,
                       guide = guide_legend(reverse = F, nrow =  1)) +
  labs(y = "Seabird species",
       x = "Island",
       fill = "Seabird density (birds/ha)") +
  ggtitle(expression(bold("B")~"Chagos Archipelago"))

g3 <- ggplot(df %>%
         filter(Region == "Tetiaroa"),
       aes(y = Species, x = Island, fill = Density_birdsha)) +
  geom_tile() +
  scale_fill_gradient2(high = "#DC267F", mid = "#FE6100", low = "#FFB000",
                       midpoint = 3000, limits = c(0.02,6000),
                       na.value = NA,
                       guide = guide_legend(reverse = F, nrow =  1)) +
  labs(y = "Seabird species",
       x = "Island",
       fill = "Seabird density (birds/ha)") +
  ggtitle(expression(bold("C")~"Tetiaroa"))


g1 + g2 + g3 +
  plot_layout(ncol = 3, widths = c(0.8, 2, 2),
              guides = "collect", axes = "collect") &
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.y = element_text(face = "italic"),
        axis.text.x = element_text(angle = 90),
        title = element_text(size = 8))

ggsave("../figures/Supp_Fig1_Seabird_density_islands.png", width = 8, height = 6)
