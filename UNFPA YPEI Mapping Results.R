# Creating UNFPA Young People Empowerment Index Maps
# November 2021
# Code written by Kristin Bietsch, PhD; Avenir Health; kbietsch@avenirhealth.org


# The packages required for this code:
library(ggplot2)
library(tidyr)
library(tidyverse)
require(xlsx)
library(viridis)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)

options(scipen = 999)

# Set the working directory where input files are stored and where you want results to be saved
setwd("C:/Users/KristinBietsch/files/UNFPA/Code for GitHub/Input Data")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

data_w <- read.csv("YEI_IndexCountryResults PopWeight 120721.csv") %>%
  rename(iso_n3=ISONum)

# If you want to display all countries
#index <- data_w %>%  select(iso_n3, Index, Economics, Education, Gender, Politics, Safety, SRH)

#If you want only countries with 20 or more indicators:
index <- data_w %>% filter(Indicator_N>=20) %>% select(iso_n3, Index, Economics, Education, Gender, Politics, Safety, SRH)

# If you want to display UNFPA countries with 20 or more indicators
index_unfpa <- data_w %>% filter(Indicator_N>=20) %>% filter(UNFPAGroup!="Non-UNFPA Country") %>% select(iso_n3, Index, Economics, Education, Gender, Politics, Safety, SRH)

# creating world dataframes
world_full <- world %>% mutate(iso_n3=as.numeric(as.character(iso_n3))) %>% left_join(index, by="iso_n3")
world_unfpa_full <- world %>% mutate(iso_n3=as.numeric(as.character(iso_n3))) %>% left_join(index_unfpa, by="iso_n3")


###############################################################
# Creating world maps for the index and six domains

# Index
ggplot(data = world_full) +
  geom_sf(aes(fill = Index)) +
  coord_sf( ylim = c(-60, 84), expand = FALSE) +
  scale_fill_gradient(
    low = "#FDF3F8",
    high = "#6B113E",
    space = "Lab",
    na.value = "#CCCCCC",
    guide = "colourbar",
    aesthetics = "fill"
  ) +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

# If you would like to export these results, uncomment the line below
ggsave("YEI_Map_Index_120721.jpg",  height=8, width=15, units = "in")

# Economic Domain
ggplot(data = world_full) +
  geom_sf(aes(fill = Economics)) +
  coord_sf( ylim = c(-60, 84), expand = FALSE) +
  scale_fill_gradient(
    low = "#EBF5FB",
    high = "#123F5A",
    space = "Lab",
    na.value = "#CCCCCC",
    guide = "colourbar",
    aesthetics = "fill"
  ) +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

# If you would like to export these results, uncomment the line below
ggsave("YEI_Map_Economics_120721.jpg",  height=8, width=15, units = "in")

# Education Domain
ggplot(data = world_full) +
  geom_sf(aes(fill = Education)) +
  coord_sf( ylim = c(-60, 84), expand = FALSE) +
  scale_fill_gradient(
    low = "#F4FAFC",
    high = "#256981",
    space = "Lab",
    na.value = "#CCCCCC",
    guide = "colourbar",
    aesthetics = "fill"
  ) +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

# If you would like to export these results, uncomment the line below
ggsave("YEI_Map_Education_120721.jpg",  height=8, width=15, units = "in")

# Gender Domain
ggplot(data = world_full) +
  geom_sf(aes(fill = Gender)) +
  coord_sf( ylim = c(-60, 84), expand = FALSE) +
  scale_fill_gradient(
    low = "#FDF3EF",
    high = "#9B3B15",
    space = "Lab",
    na.value = "#CCCCCC",
    guide = "colourbar",
    aesthetics = "fill"
  ) +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

# If you would like to export these results, uncomment the line below
ggsave("YEI_Map_Gender_120721.jpg",  height=8, width=15, units = "in")

# Politics Domain
ggplot(data = world_full) +
  geom_sf(aes(fill = Politics)) +
  coord_sf( ylim = c(-60, 84), expand = FALSE) +
  scale_fill_gradient(
    low = "#FFF4E1",
    high = "#AE6C00",
    space = "Lab",
    na.value = "#CCCCCC",
    guide = "colourbar",
    aesthetics = "fill"
  ) +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

# If you would like to export these results, uncomment the line below
ggsave("YEI_Map_Politics_120721.jpg",  height=8, width=15, units = "in")

# Safety Domain
ggplot(data = world_full) +
  geom_sf(aes(fill = Safety)) +
  coord_sf( ylim = c(-60, 84), expand = FALSE) +
  scale_fill_gradient(
    low = "#ECF8EF",
    high = "#1E5229",
    space = "Lab",
    na.value = "#CCCCCC",
    guide = "colourbar",
    aesthetics = "fill"
  ) +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

# If you would like to export these results, uncomment the line below
ggsave("YEI_Map_Safety_120721.jpg",  height=8, width=15, units = "in")

# SRH Domain
ggplot(data = world_full) +
  geom_sf(aes(fill = SRH)) +
  coord_sf( ylim = c(-60, 84), expand = FALSE) +
  scale_fill_gradient(
    low = "#FFF2D9",
    high = "#B87B00",
    space = "Lab",
    na.value = "#CCCCCC",
    guide = "colourbar",
    aesthetics = "fill"
  ) +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())
# If you would like to export these results, uncomment the line below
 ggsave("YEI_Map_SRH_120721.jpg",  height=8, width=15, units = "in")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Creating world maps for UNFPA Countries the index and six domains

# Index
ggplot(data = world_unfpa_full) +
  geom_sf(aes(fill = Index)) +
  coord_sf( ylim = c(-60, 84), expand = FALSE) +
  scale_fill_gradient(
    low = "#FDF3F8",
    high = "#6B113E",
    space = "Lab",
    na.value = "#CCCCCC",
    guide = "colourbar",
    aesthetics = "fill"
  ) +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

# If you would like to export these results, uncomment the line below
 ggsave("YEI_UNFPAMap_Index_120721.jpg",  height=8, width=15, units = "in")

# Economic Domain
ggplot(data = world_unfpa_full) +
  geom_sf(aes(fill = Economics)) +
  coord_sf( ylim = c(-60, 84), expand = FALSE) +
  scale_fill_gradient(
    low = "#EBF5FB",
    high = "#123F5A",
    space = "Lab",
    na.value = "#CCCCCC",
    guide = "colourbar",
    aesthetics = "fill"
  ) +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

# If you would like to export these results, uncomment the line below
 ggsave("YEI_UNFPAMap_Economics_120721.jpg",  height=8, width=15, units = "in")

# Education Domain
ggplot(data = world_unfpa_full) +
  geom_sf(aes(fill = Education)) +
  coord_sf( ylim = c(-60, 84), expand = FALSE) +
  scale_fill_gradient(
    low = "#F4FAFC",
    high = "#256981",
    space = "Lab",
    na.value = "#CCCCCC",
    guide = "colourbar",
    aesthetics = "fill"
  ) +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

# If you would like to export these results, uncomment the line below
ggsave("YEI_UNFPAMap_Education_120721.jpg",  height=8, width=15, units = "in")

# Gender Domain
ggplot(data = world_unfpa_full) +
  geom_sf(aes(fill = Gender)) +
  coord_sf( ylim = c(-60, 84), expand = FALSE) +
  scale_fill_gradient(
    low = "#FDF3EF",
    high = "#9B3B15",
    space = "Lab",
    na.value = "#CCCCCC",
    guide = "colourbar",
    aesthetics = "fill"
  ) +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

# If you would like to export these results, uncomment the line below
ggsave("YEI_UNFPAMap_Gender_120721.jpg",  height=8, width=15, units = "in")

# Politics Domain
ggplot(data = world_unfpa_full) +
  geom_sf(aes(fill = Politics)) +
  coord_sf( ylim = c(-60, 84), expand = FALSE) +
  scale_fill_gradient(
    low = "#FFF4E1",
    high = "#AE6C00",
    space = "Lab",
    na.value = "#CCCCCC",
    guide = "colourbar",
    aesthetics = "fill"
  ) +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

# If you would like to export these results, uncomment the line below
ggsave("YEI_UNFPAMap_Politics_120721.jpg",  height=8, width=15, units = "in")

# Safety Domain
ggplot(data = world_unfpa_full) +
  geom_sf(aes(fill = Safety)) +
  coord_sf( ylim = c(-60, 84), expand = FALSE) +
  scale_fill_gradient(
    low = "#ECF8EF",
    high = "#1E5229",
    space = "Lab",
    na.value = "#CCCCCC",
    guide = "colourbar",
    aesthetics = "fill"
  ) +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

# If you would like to export these results, uncomment the line below
ggsave("YEI_UNFPAMap_Safety_120721.jpg",  height=8, width=15, units = "in")

# SRH Domain
ggplot(data = world_unfpa_full) +
  geom_sf(aes(fill = SRH)) +
  coord_sf( ylim = c(-60, 84), expand = FALSE) +
  scale_fill_gradient(
    low = "#FFF2D9",
    high = "#B87B00",
    space = "Lab",
    na.value = "#CCCCCC",
    guide = "colourbar",
    aesthetics = "fill"
  ) +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

# If you would like to export these results, uncomment the line below
ggsave("YEI_UNFPAMap_SRH_120721.jpg",  height=8, width=15, units = "in")



