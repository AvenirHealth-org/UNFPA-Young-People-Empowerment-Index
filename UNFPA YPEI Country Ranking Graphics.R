# Creating UNFPA Young People Empowerment Index Graphics 3
# November 2021
# Code written by Kristin Bietsch, PhD; Avenir Health; kbietsch@avenirhealth.org

# The packages required for this code:
library(ggplot2)
library(tidyr)
library(tidyverse)
library(viridis)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)

# Set the working directory where input files are stored and where you want results to be saved
setwd("C:/Users/KristinBietsch/files/UNFPA/Code for GitHub/Input Data")

countries <- read.csv("YEI_IndexCountryResults PopWeight 111221.csv")
regions <- read.csv("Country Regions Large.csv") %>% select(Country, UNFPAGroup)


data_graph <- countries  %>% filter(Indicator_N>=20) %>%
  select(Country, SDG, Index, Economics, Education, Gender, Politics, Safety, SRH) %>%
  gather(Variable, Value, Economics:SRH) %>%
  mutate(Variable=case_when(
    Variable=="Economics"   ~ "Economic Empowerment"  ,                
    Variable== "Education"  ~ "Educational Empowerment" ,                           
    Variable== "Gender" ~     "Gender and Autonomy"     ,             
    Variable=="Safety"     ~     "Safety and Security" ,             
    Variable==  "SRH"     ~ "SRH Empowerment" ,                         
    Variable=="Politics"  ~ "Youth Policy and Political Participation")) %>%
  left_join(regions, by="Country")

levels(as.factor(data_graph$UNFPAGroup))

ggplot(subset(data_graph, UNFPAGroup== "Arab States" ), aes(y= reorder(Country, Index))) +
  geom_bar( stat = "summary", fun.y = "mean", aes( x = Index), fill="#E377FD") +
  geom_point(aes(x=Value, shape=Variable), size=6) +
  xlim(0,1) +
  theme_bw() +
  labs(title="", y="",  shape="Domain") +
  theme(text = element_text(size = 25))      
#ggsave("UNFPA Country Rankings Arab States 111921.jpg", dpi=300, height=15, width=18, units = "in")
#ggsave("UNFPA Country Rankings Arab States 111921.eps", dpi=300, height=15, width=18, units = "in")


ggplot(subset(data_graph, UNFPAGroup=="Asia & the Pacific"  ), aes(y= reorder(Country, Index))) +
  geom_bar( stat = "summary", fun.y = "mean", aes( x = Index), fill="#B6A8DC") +
  geom_point(aes(x=Value, shape=Variable), size=6) +
  xlim(0,1) +
  theme_bw() +
  labs(title="", y="",  shape="Domain") +
  theme(text = element_text(size = 25))      
#ggsave("UNFPA Country Rankings Asia Pacific 111921.jpg", dpi=300, height=15, width=18, units = "in")
#ggsave("UNFPA Country Rankings Asia Pacific 111921.eps", dpi=300, height=15, width=18, units = "in")


ggplot(subset(data_graph, UNFPAGroup==  "East & Southern Africa"  ), aes(y= reorder(Country, Index))) +
  geom_bar( stat = "summary", fun.y = "mean", aes( x = Index), fill="#92AFD6") +
  geom_point(aes(x=Value, shape=Variable), size=6) +
  xlim(0,1) +
  theme_bw() +
  labs(title="", y="",  shape="Domain") +
  theme(text = element_text(size = 25))      
#ggsave("UNFPA Country Rankings East Southern Africa 111921.jpg", dpi=300, height=15, width=18, units = "in")
#ggsave("UNFPA Country Rankings East Southern Africa 111921.eps", dpi=300, height=15, width=18, units = "in")


ggplot(subset(data_graph, UNFPAGroup== "Eastern Europe & Central Asia" ), aes(y= reorder(Country, Index))) +
  geom_bar( stat = "summary", fun.y = "mean", aes( x = Index), fill="#7BCDDB") +
  geom_point(aes(x=Value, shape=Variable), size=6) +
  xlim(0,1) +
  theme_bw() +
  labs(title="", y="",  shape="Domain") +
  theme(text = element_text(size = 25))      
#ggsave("UNFPA Country Rankings Eastern Europe Central Asia 111921.jpg", dpi=300, height=15, width=18, units = "in")
#ggsave("UNFPA Country Rankings Eastern Europe Central Asia 111921.eps", dpi=300, height=15, width=18, units = "in")


ggplot(subset(data_graph, UNFPAGroup== "Latin America & the Caribbean"), aes(y= reorder(Country, Index))) +
  geom_bar( stat = "summary", fun.y = "mean", aes( x = Index), fill="#5BDFC6") +
  geom_point(aes(x=Value, shape=Variable), size=6) +
  xlim(0,1) +
  theme_bw() +
  labs(title="", y="",  shape="Domain") +
  theme(text = element_text(size = 25))      
#ggsave("UNFPA Country Rankings LAC 111921.jpg",  dpi=300, height=15, width=18, units = "in")
#ggsave("UNFPA Country Rankings LAC 111921.eps",  dpi=300, height=15, width=18, units = "in")


ggplot(subset(data_graph, UNFPAGroup== "West & Central Africa"  ), aes(y= reorder(Country, Index))) +
  geom_bar( stat = "summary", fun.y = "mean", aes( x = Index), fill="#8CD8A2") +
  geom_point(aes(x=Value, shape=Variable), size=6) +
  xlim(0,1) +
  theme_bw() +
  labs(title="", y="",  shape="Domain") +
  theme(text = element_text(size = 25))      
#ggsave("UNFPA Country Rankings West Central Africa 111921.jpg", dpi=300, height=15, width=18, units = "in")
#ggsave("UNFPA Country Rankings West Central Africa 111921.eps", dpi=300, height=15, width=18, units = "in")


ggplot(subset(data_graph, UNFPAGroup== "Non-UNFPA Country"  ), aes(y= reorder(Country, Index))) +
  geom_bar( stat = "summary", fun.y = "mean", aes( x = Index), fill="#BDE575") +
  geom_point(aes(x=Value, shape=Variable), size=6) +
  xlim(0,1) +
  theme_bw() +
  labs(title="", y="",  shape="Domain") +
  theme(text = element_text(size = 25))      
#ggsave("UNFPA Country Rankings Non UNFPA Countries 111921.jpg", dpi=300, height=15, width=18, units = "in")
#ggsave("UNFPA Country Rankings Non UNFPA Countries 111921.eps", dpi=300, height=15, width=18, units = "in")


