# packages
# install.packages("tidyverse")
# install.packages("patchwork")
# install.packages("ggtext")
library(tidyverse)
library(patchwork)
library(ggtext)

# clear space
rm(list = ls(all = TRUE))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# load data ####
df <- read_csv("Seabirds_Energy_Nitrogen.csv") %>%
  # dplyr::select(-c(SeabirdEnergyRequirements_MJha, Ninput_kgha)) %>%
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
    Island == "Tauini" ~ "Tauvini",
    Island == "Tiaraunu" ~ "Tiara'aunu",
    Island == "Honuea" ~ "Honu'ea",
    Island == "Rimatuu" ~ "Rimatu'u",
    Island == "Oroatera" ~ "Horoatera",
    Island == "Yeye" ~ "Ile Yéyé",
    TRUE ~ Island)) %>%
  mutate(Family = case_when(
    Species %in% c("Anous minutus", "Anous stolidus", "Anous tenuirostris",
                   "Gygis alba", "Onychoprion fuscatus", "Onychoprion lunatus",
                   "Sterna sumatrana", "Thalasseus bergii") ~ "Laridae",
    Species %in% c("Ardenna pacifica", "Puffinus bailloni") ~ "Procellariidae",
    Species %in% c("Fregata ariel", "Fregata minor") ~ "Fregatidae",
    Species == "Phaethon lepturus" ~ "Phaethontidae",
    Species %in% c("Sula leucogaster", "Sula sula") ~ "Sulidae",
    TRUE ~ NA_character_)) %>%
  mutate(Rats = case_when(
    Island %in% c("Ahuroa", "Eagle Island", "Felicite",
                  "Grande Mapou", "Hira'a'anae", "Honu'ea",
                  "Horoatera", "Ile Anglaise (PB)", "Ile Anglaise (SAL)",
                  "Ile Fouquet", "Ile Poule", "Ile Yéyé", "Onetahi",
                  "Rimatu'u", "Tauvini", "Tiara'aunu") ~ "Rats",
    Island %in% c("Ā'ie", "Aride", "Cousine", "Fregate",
                  "Grand Coquillage", "Ilde de la Passe", "Ile Longue",
                  "Middle Brother", "Nelsons Island",
                  "Reiono", "South Brother", "Tahuna iti",
                  "Ile de la Passe") ~ "No rats",
    TRUE ~ NA_character_))

# sum data by seabird family
df_family <- df %>%
  group_by(Region, Atoll, Island, Rats, Family) %>%
  summarise(Energy_sum = sum(SeabirdEnergyRequirements_MJha, na.rm = TRUE),
            N_sum = sum(Ninput_kgha, na.rm = TRUE),
            .groups = "drop")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# create the seabird energy heatmaps ####

summary(df_family$Energy_sum)
hist(df_family$Energy_sum)

g_all <- df_family %>%
  mutate(Region = factor(Region, levels = c("Seychelles", "Chagos Archipelago", "Tetiaroa")),
         Island = reorder(Island, -N_sum),
         Energy_sum = ifelse(Energy_sum == 0, NA, Energy_sum)) %>%
  ggplot(aes(y = Family, x = Island, fill = Energy_sum)) +
  geom_tile() +
  scale_fill_gradientn(
    colours = c("#FFB000", "#FE6100", "#DC267F"),
    trans = "log1p",
    labels = scales::label_comma(),
    breaks = c(0, 10, 100, 1000, 10000, 100000),
    guide = guide_colorbar(reverse = FALSE,
                           barwidth = unit(7, "cm"), ticks = FALSE),
    na.value = NA) +
  scale_x_discrete(labels = c(
    "Aride"            = expression(bold("Aride")),
    "Cousine"          = expression(bold("Cousine")),
    "Fregate"          = expression(bold("Fregate")),
    "Grand Coquillage" = expression(bold("Grand Coquillage")),
    "Ile de la Passe"  = expression(bold("Ile de la Passe")),
    "Ile Longue"       = expression(bold("Ile Longue")),
    "Middle Brother"   = expression(bold("Middle Brother")),
    "Nelsons Island"   = expression(bold("Nelsons Island")),
    "South Brother"    = expression(bold("South Brother")),
    "Ā'ie"             = expression(bold("Ā'ie")),
    "Reiono"           = expression(bold("Reiono")),
    "Tahuna iti"       = expression(bold("Tahuna iti")))) +
  scale_y_discrete(labels = c(
    "Sulidae"        = "<i>Sulidae</i><br><span style='font-size:11pt'>(30–110 km)</span>",
    "Procellariidae" = "<i>Procellariidae</i><br><span style='font-size:11pt'>(460–590 km)</span>",
    "Phaethontidae"  = "<i>Phaethontidae</i><br><span style='font-size:11pt'>(c. 170 km)</span>",
    "Laridae"        = "<i>Laridae</i><br><span style='font-size:11pt'>(2–580 km)</span>",
    "Fregatidae"     = "<i>Fregatidae</i><br><span style='font-size:11pt'>(100–940 km)</span>")) +
  labs(y = expression("Seabird energy flow (MJ yr"^-1*")"),
       x = "Island",
       fill = expression("Seabird energy flow (MJ yr"^-1*")")) +
  theme_bw() +
  theme(axis.text.y = element_markdown(hjust = 1, size = 12),
        legend.position = "bottom") +
  facet_wrap(~Region, scales = "free_x")   # facet by Region

g_all


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# create the bar charts of N input ####

n_all <- df %>%
  group_by(Region, Atoll, Island, Rats) %>%
  summarise(N_sum = sum(Ninput_kgha, na.rm = TRUE), .groups = "drop") %>%
  mutate(Region = factor(Region, levels = c("Seychelles", "Chagos Archipelago", "Tetiaroa")),
         Island = reorder(Island, -N_sum)) %>%
  ggplot(aes(x = Island, y = N_sum)) +
  geom_col() +
  scale_x_discrete(labels = c(
    "Aride"            = expression(bold("Aride")),
    "Cousine"          = expression(bold("Cousine")),
    "Fregate"          = expression(bold("Fregate")),
    "Grand Coquillage" = expression(bold("Grand Coquillage")),
    "Ile de la Passe"  = expression(bold("Ile de la Passe")),
    "Ile Longue"       = expression(bold("Ile Longue")),
    "Middle Brother"   = expression(bold("Middle Brother")),
    "Nelsons Island"   = expression(bold("Nelsons Island")),
    "South Brother"    = expression(bold("South Brother")),
    "Ā'ie"             = expression(bold("Ā'ie")),
    "Reiono"           = expression(bold("Reiono")),
    "Tahuna iti"       = expression(bold("Tahuna iti")))) +
  labs(y = "Nitrogen input (kg ha⁻¹ year⁻¹)\n[log scale]",
       x = "Island") +
  scale_y_continuous(trans = "log1p",
                     limits = c(0, 370),
                     breaks = c(0, 1, 5, 25, 50, 100, 200, 400)) +
  facet_wrap(~Region, scales = "free_x") +
  theme_bw()

n_all

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Seabird foraging range vals ####

df_foraging <- read_csv("Seabirds_Foraging_Energy.csv") %>%
  mutate(Family = case_when(
    Species %in% c("Anous minutus", "Anous stolidus", "Anous tenuirostris",
                   "Gygis alba", "Onychoprion fuscatus", "Onychoprion lunatus",
                   "Sterna sumatrana", "Thalasseus bergii") ~ "Laridae",
    Species %in% c("Ardenna pacifica", "Puffinus bailloni") ~ "Procellariidae",
    Species %in% c("Fregata ariel", "Fregata minor") ~ "Fregatidae",
    Species == "Phaethon lepturus" ~ "Phaethontidae",
    Species %in% c("Sula leucogaster", "Sula sula") ~ "Sulidae",
    TRUE ~ NA_character_))  %>%
  filter(!Species %in% c("Sternula albifrons", "Sula dactylatra", "Sterna dougallii",
                         "Phaethon rubricauda", "Onychoprion anaethetus")) %>%
  group_by(Family) %>%
  summarise(Mean_foraging = mean(Mean_max_foraging_dist_km, na.rm = TRUE),
            Min_foraging = min(Mean_max_foraging_dist_km, na.rm = TRUE),
            Max_foraging = max(Mean_max_foraging_dist_km, na.rm = TRUE),
            .groups = "drop")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# combine the plots! ####

g_all / n_all +
  plot_layout(heights = c(1.5, 1), 
              widths = c(1, 0.20, 1, 0.20, 1), 
              axes = "collect") &
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.text = element_blank(),
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))

ggsave("Seabird_density_islands_family_N_foraging6_nolegend.png", width = 11, height = 8)

# panel B will have no x-axis labels (the x-axis ticks will match up with the ticks 
# and island labels in panel C)
g_all_no_x = g_all +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none",
        panel.spacing.x = unit(1.2, "lines"),
        strip.text = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
g_all_no_x
ggsave("Panel_B_No_X_Axis.png", width = 12.4, height = 3)


n_all = n_all +
  theme(axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "none",
        panel.spacing.x = unit(1.2, "lines"),
        strip.text = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
n_all
ggsave("Panel_C.png", width = 11, height = 4.5)
