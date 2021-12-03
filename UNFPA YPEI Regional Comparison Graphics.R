# Creating UNFPA Young People Empowerment Index Graphics
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
library("ggradar")

devtools::install_github("ricardo-bion/ggradar")
options(scipen = 999)


# Set the working directory where input files are stored and where you want results to be saved
setwd("C:/Users/KristinBietsch/files/UNFPA/Code for GitHub/Input Data")

regional <- read.csv("YEI Regional Results Weighted 111221.csv")

# For these graphics we only want the global and UNFPA groupings, feel free to adjust
unfpa <- regional %>% filter(Region=="Global" |
                               Region=="UNFPA Total"     |                                   
                               Region=="UNFPA: Arab States"    |                             
                               Region=="UNFPA: Asia & the Pacific"    |                     
                               Region=="UNFPA: East & Southern Africa"  |                    
                               Region=="UNFPA: Eastern Europe & Central Asia" |              
                               Region=="UNFPA: Latin America & the Caribbean"   |           
                               Region=="UNFPA: West & Central Africa"  |   
                               Region=="UNFPA: Non-UNFPA Country"   ) %>%
  mutate(Region=case_when(Region=="Global"  ~ "Global", # renaming
                          Region=="UNFPA Total" ~ "UNFPA Total",                                    
                          Region=="UNFPA: Arab States" ~ "Arab States",                                
                          Region=="UNFPA: Asia & the Pacific"   ~ "Asia & the Pacific",                     
                          Region=="UNFPA: East & Southern Africa"    ~ "East & Southern Africa",                  
                          Region=="UNFPA: Eastern Europe & Central Asia"   ~ "Eastern Europe & Central Asia",          
                          Region=="UNFPA: Latin America & the Caribbean"  ~ "Latin America & the Caribbean",             
                          Region=="UNFPA: West & Central Africa"   ~ "West & Central Africa",   
                          Region=="UNFPA: Non-UNFPA Country"  ~ "Non-UNFPA Country" )) %>%
  select(Region, Index, Economics, Education, Gender, Politics, Safety, SRH) %>%
  gather(Variable, Value, Index:SRH) %>%
  mutate(Region = fct_relevel(Region,  # Organizing display level
                              "Global", "UNFPA Total", "Arab States", 
                              "Asia & the Pacific", "East & Southern Africa", "Eastern Europe & Central Asia", 
                              "Latin America & the Caribbean", "West & Central Africa", "Non-UNFPA Country")) %>%
  mutate(Variable_full = case_when(Variable==  "Economics" ~ "My Life: Economic Empowerment", # renaming
                                   Variable=="Education"  ~ "My Life: Educational Empowerment",
                                   Variable=="Gender"  ~ "My Life: Gender and Autonomy",   
                                   Variable== "Index"  ~ "Index",   
                                   Variable== "Politics" ~ "My World: Youth Policy and Political Participation",  
                                   Variable== "Safety"   ~ "My World: Safety and Security", 
                                   Variable== "SRH"  ~ "My Body: SRH Empowerment" ))  %>%
  mutate(Variable_full = fct_relevel(Variable_full,  # Organizing display level
                                     "Index",  "My Body: SRH Empowerment",    "My Life: Gender and Autonomy",
                                     "My Life: Educational Empowerment",  "My Life: Economic Empowerment",
                                     "My World: Youth Policy and Political Participation",  "My World: Safety and Security"))





ggplot(unfpa, aes(x=Variable_full, y=Value, fill=Variable_full)) + 
  geom_bar(stat="identity") +
  facet_wrap( ~ Region, nrow=1, strip.position="bottom") +
  scale_fill_manual( values=
                       c(   "Index" = "#3BA351",   "My Body: SRH Empowerment" = "#E4519C",    "My Life: Gender and Autonomy" = "#FFBF3B",
                            "My Life: Educational Empowerment" = "#E15E27",  "My Life: Economic Empowerment"= "#EA9200", 
                            "My World: Youth Policy and Political Participation" ="#389AD6" , "My World: Safety and Security" = "#80C3DA")) +
  labs(fill="", x="", y="", title="Global Average Young People Empowerment Index Scores") + 
  theme_bw() +
  guides(fill = guide_legend(nrow = 2)) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.background = element_blank(),
        legend.position = "top")

# If you would like to export these results, uncomment the line below
ggsave("Global Averages YPEI Scores 111921.jpg",  dpi=300, width=17, height=6, units = "in")
ggsave("Global Averages YPEI Scores 111921.eps",  dpi=300, width=17, height=6, units = "in")


# Graphics comparing an individual region to the global results
ggplot(subset(unfpa, Region== "Global" | Region=="Arab States"), aes(x=Variable_full, y=Value, fill=Variable_full)) + 
  geom_bar(stat="identity") +
  facet_wrap( ~ Region, nrow=1, strip.position="bottom") +
  scale_fill_manual( values=
                       c(   "Index" = "#3BA351",   "My Body: SRH Empowerment" = "#E4519C",    "My Life: Gender and Autonomy" = "#FFBF3B",
                            "My Life: Educational Empowerment" = "#E15E27",  "My Life: Economic Empowerment"= "#EA9200", 
                            "My World: Youth Policy and Political Participation" ="#389AD6" , "My World: Safety and Security" = "#80C3DA")) +
  labs(fill="", x="", y="", title="Global Average Young People Empowerment Index Scores") + 
  theme_bw() +
  guides(fill = guide_legend(nrow = 2)) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.background = element_blank(),
        legend.position = "top",
        axis.text = element_text(size=13),
        strip.text = element_text(size=13))

# If you would like to export these results, uncomment the line below
 ggsave("Arab States Averages YPEI Scores 111921.jpg",  dpi=300, width=10, height=6, units = "in")
 ggsave("Arab States Averages YPEI Scores 111921.eps",  dpi=300, width=10, height=6, units = "in")
 


ggplot(subset(unfpa, Region== "Global" | Region=="Asia & the Pacific"), aes(x=Variable_full, y=Value, fill=Variable_full)) + 
  geom_bar(stat="identity") +
  facet_wrap( ~ Region, nrow=1, strip.position="bottom") +
  scale_fill_manual( values=
                       c(   "Index" = "#3BA351",   "My Body: SRH Empowerment" = "#E4519C",    "My Life: Gender and Autonomy" = "#FFBF3B",
                            "My Life: Educational Empowerment" = "#E15E27",  "My Life: Economic Empowerment"= "#EA9200", 
                            "My World: Youth Policy and Political Participation" ="#389AD6" , "My World: Safety and Security" = "#80C3DA")) +
  labs(fill="", x="", y="", title="Global Average Young People Empowerment Index Scores") + 
  theme_bw() +
  guides(fill = guide_legend(nrow = 2)) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.background = element_blank(),
        legend.position = "top",
        axis.text = element_text(size=13),
        strip.text = element_text(size=13))

# If you would like to export these results, uncomment the line below
 ggsave("Asia Averages YPEI Scores 111921.jpg",  dpi=300, width=10, height=6, units = "in")
 ggsave("Asia Averages YPEI Scores 111921.eps",  dpi=300, width=10, height=6, units = "in")
 

ggplot(subset(unfpa, Region== "Global" | Region=="East & Southern Africa"), aes(x=Variable_full, y=Value, fill=Variable_full)) + 
  geom_bar(stat="identity") +
  facet_wrap( ~ Region, nrow=1, strip.position="bottom") +
  scale_fill_manual( values=
                       c(   "Index" = "#3BA351",   "My Body: SRH Empowerment" = "#E4519C",    "My Life: Gender and Autonomy" = "#FFBF3B",
                            "My Life: Educational Empowerment" = "#E15E27",  "My Life: Economic Empowerment"= "#EA9200", 
                            "My World: Youth Policy and Political Participation" ="#389AD6" , "My World: Safety and Security" = "#80C3DA")) +
  labs(fill="", x="", y="", title="Global Average Young People Empowerment Index Scores") + 
  theme_bw() +
  guides(fill = guide_legend(nrow = 2)) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.background = element_blank(),
        legend.position = "top",
        axis.text = element_text(size=13),
        strip.text = element_text(size=13))

# If you would like to export these results, uncomment the line below
 ggsave("East South Africa Averages Scores 111921.jpg", dpi=300, width=10, height=6, units = "in")
 ggsave("East South Africa Averages Scores 111921.eps", dpi=300, width=10, height=6, units = "in")
 

ggplot(subset(unfpa, Region== "Global" | Region=="Eastern Europe & Central Asia"), aes(x=Variable_full, y=Value, fill=Variable_full)) + 
  geom_bar(stat="identity") +
  facet_wrap( ~ Region, nrow=1, strip.position="bottom") +
  scale_fill_manual( values=
                       c(   "Index" = "#3BA351",   "My Body: SRH Empowerment" = "#E4519C",    "My Life: Gender and Autonomy" = "#FFBF3B",
                            "My Life: Educational Empowerment" = "#E15E27",  "My Life: Economic Empowerment"= "#EA9200", 
                            "My World: Youth Policy and Political Participation" ="#389AD6" , "My World: Safety and Security" = "#80C3DA")) +
  labs(fill="", x="", y="", title="Global Average Young People Empowerment Index Scores") + 
  theme_bw() +
  guides(fill = guide_legend(nrow = 2)) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.background = element_blank(),
        legend.position = "top",
        axis.text = element_text(size=13),
        strip.text = element_text(size=13))

# If you would like to export these results, uncomment the line below
 ggsave("EEurope CAsia Averages YPEI Scores 111921.jpg",  dpi=300, width=10, height=6, units = "in")
 ggsave("EEurope CAsia Averages YPEI Scores 111921.eps",  dpi=300, width=10, height=6, units = "in")
 

ggplot(subset(unfpa, Region== "Global" | Region=="Latin America & the Caribbean"), aes(x=Variable_full, y=Value, fill=Variable_full)) + 
  geom_bar(stat="identity") +
  facet_wrap( ~ Region, nrow=1, strip.position="bottom") +
  scale_fill_manual( values=
                       c(   "Index" = "#3BA351",   "My Body: SRH Empowerment" = "#E4519C",    "My Life: Gender and Autonomy" = "#FFBF3B",
                            "My Life: Educational Empowerment" = "#E15E27",  "My Life: Economic Empowerment"= "#EA9200", 
                            "My World: Youth Policy and Political Participation" ="#389AD6" , "My World: Safety and Security" = "#80C3DA")) +
  labs(fill="", x="", y="", title="Global Average Young People Empowerment Index Scores") + 
  theme_bw() +
  guides(fill = guide_legend(nrow = 2)) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.background = element_blank(),
        legend.position = "top",
        axis.text = element_text(size=13),
        strip.text = element_text(size=13))

# If you would like to export these results, uncomment the line below
 ggsave("LAC Averages YPEI Scores 111921.jpg", dpi=300, width=10, height=6, units = "in")
 ggsave("LAC Averages YPEI Scores 111921.eps", dpi=300, width=10, height=6, units = "in")
 

ggplot(subset(unfpa, Region== "Global" | Region=="West & Central Africa"), aes(x=Variable_full, y=Value, fill=Variable_full)) + 
  geom_bar(stat="identity") +
  facet_wrap( ~ Region, nrow=1, strip.position="bottom") +
  scale_fill_manual( values=
                       c(   "Index" = "#3BA351",   "My Body: SRH Empowerment" = "#E4519C",    "My Life: Gender and Autonomy" = "#FFBF3B",
                            "My Life: Educational Empowerment" = "#E15E27",  "My Life: Economic Empowerment"= "#EA9200", 
                            "My World: Youth Policy and Political Participation" ="#389AD6" , "My World: Safety and Security" = "#80C3DA")) +
  labs(fill="", x="", y="", title="Global Average Young People Empowerment Index Scores") + 
  theme_bw() +
  guides(fill = guide_legend(nrow = 2)) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.background = element_blank(),
        legend.position = "top",
        axis.text = element_text(size=13),
        strip.text = element_text(size=13))

# If you would like to export these results, uncomment the line below
ggsave("W C Africa Averages YPEI Scores 111921.jpg", dpi=300, width=10, height=6, units = "in")
ggsave("W C Africa Averages YPEI Scores 111921.eps", dpi=300, width=10, height=6, units = "in")


############################################################################

# Creating Spider Charts which compare a region with global results

spider <- regional %>% filter(Region=="Global" |
                                Region=="UNFPA Total"     |                                   
                                Region=="UNFPA: Arab States"    |                             
                                Region=="UNFPA: Asia & the Pacific"    |                     
                                Region=="UNFPA: East & Southern Africa"  |                    
                                Region=="UNFPA: Eastern Europe & Central Asia" |              
                                Region=="UNFPA: Latin America & the Caribbean"   |           
                                Region=="UNFPA: West & Central Africa"  |   
                                Region=="UNFPA: Non-UNFPA Country"   ) %>%
  mutate(Region=case_when(Region=="Global"  ~ "Global",
                          Region=="UNFPA Total" ~ "UNFPA Total",                                    
                          Region=="UNFPA: Arab States" ~ "Arab States",                                
                          Region=="UNFPA: Asia & the Pacific"   ~ "Asia & the Pacific",                     
                          Region=="UNFPA: East & Southern Africa"    ~ "East & Southern Africa",                  
                          Region=="UNFPA: Eastern Europe & Central Asia"   ~ "Eastern Europe & Central Asia",          
                          Region=="UNFPA: Latin America & the Caribbean"  ~ "Latin America & the Caribbean",             
                          Region=="UNFPA: West & Central Africa"   ~ "West & Central Africa",   
                          Region=="UNFPA: Non-UNFPA Country"  ~ "Non-UNFPA Country" )) %>%
  select(Region, Economics, Education, Gender, Politics, Safety, SRH) %>%
  rename("My Life:\nEconomic Empowerment"=  Economics,
         "My Life:\nEducational\nEmpowerment"= Education,
         "My Life:\nGender and\nAutonomy"= Gender,   
         "My World:\nYouth Policy and\nPolitical Participation"= Politics,  
         "My World:\nSafety and\nSecurity" =  Safety, 
         "My Body:\nSRH Empowerment"  = SRH) 


  
# https://www.datanovia.com/en/blog/beautiful-radar-chart-in-r-using-fmsb-and-ggplot-packages/


ggradar(
  spider[c(1, 3), ], 
  values.radar = c("0", ".5", "1"),
  group.colours = c( "#3BA351",  "#FFBF3B"),
  grid.min = 0, grid.mid = .5, grid.max = 1,  background.circle.colour = "white",
  gridline.mid.colour = "grey"
)

# If you would like to export these results, uncomment the line below
ggsave("Arab Spider Plot 111921.jpg", dpi=300, width=14, height=8, units = "in")
ggsave("Arab Spider Plot 111921.eps", dpi=300, width=14, height=8, units = "in")

ggradar(
  spider[c(1, 4), ], 
  values.radar = c("0", ".5", "1"),
  group.colours = c( "#3BA351",  "#FFBF3B"),
  grid.min = 0, grid.mid = .5, grid.max = 1,  background.circle.colour = "white",
  gridline.mid.colour = "grey"
)
# If you would like to export these results, uncomment the line below
 ggsave("Asia Spider Plot 111921.jpg",  dpi=300, width=14, height=8, units = "in")
 ggsave("Asia Spider Plot 111921.eps",  dpi=300, width=14, height=8, units = "in")
 
ggradar(
  spider[c(1, 5), ], 
  values.radar = c("0", ".5", "1"),
  group.colours = c( "#3BA351",  "#FFBF3B"),
  grid.min = 0, grid.mid = .5, grid.max = 1,  background.circle.colour = "white",
  gridline.mid.colour = "grey"
)
# If you would like to export these results, uncomment the line below
 ggsave("E S Africa Spider Plot 111921.jpg", dpi=300, width=14, height=8, units = "in")
 ggsave("E S Africa Spider Plot 111921.eps", dpi=300, width=14, height=8, units = "in")
 

ggradar(
  spider[c(1, 6), ], 
  values.radar = c("0", ".5", "1"),
  group.colours = c( "#3BA351",  "#FFBF3B"),
  grid.min = 0, grid.mid = .5, grid.max = 1,  background.circle.colour = "white",
  gridline.mid.colour = "grey"
)
# If you would like to export these results, uncomment the line below
 ggsave("E Europe C Asia Spider Plot 111921.jpg", dpi=300, width=14, height=8, units = "in")
 ggsave("E Europe C Asia Spider Plot 111921.eps", dpi=300, width=14, height=8, units = "in")
 
ggradar(
  spider[c(1, 7), ], 
  values.radar = c("0", ".5", "1"),
  group.colours = c( "#FFBF3B", "#3BA351" ),
  grid.min = 0, grid.mid = .5, grid.max = 1,  background.circle.colour = "white",
  gridline.mid.colour = "grey"
)
# If you would like to export these results, uncomment the line below
 ggsave("LAC Spider Plot 111921.jpg", dpi=300, width=14, height=8, units = "in")
 ggsave("LAC Spider Plot 111921.eps", dpi=300, width=14, height=8, units = "in")
 

ggradar(
  spider[c(1, 9), ], 
  values.radar = c("0", ".5", "1"),
  group.colours = c( "#FFBF3B", "#3BA351" ),
  grid.min = 0, grid.mid = .5, grid.max = 1,  background.circle.colour = "white",
  gridline.mid.colour = "grey"
)
# If you would like to export these results, uncomment the line below
 ggsave("W C Africa Spider Plot 111921.jpg",  dpi=300, width=14, height=8, units = "in")
 ggsave("W C Africa Spider Plot 111921.eps",  dpi=300, width=14, height=8, units = "in")
 
###############################################
# Spider Charts to compare subdomains and domains within a region

spider2 <- regional %>% filter(Region=="Global" |
                                 Region=="UNFPA Total"     |                                   
                                 Region=="UNFPA: Arab States"    |                             
                                 Region=="UNFPA: Asia & the Pacific"    |                     
                                 Region=="UNFPA: East & Southern Africa"  |                    
                                 Region=="UNFPA: Eastern Europe & Central Asia" |              
                                 Region=="UNFPA: Latin America & the Caribbean"   |           
                                 Region=="UNFPA: West & Central Africa"  |   
                                 Region=="UNFPA: Non-UNFPA Country"   ) %>%
  mutate(Region=case_when(Region=="Global"  ~ "Global",
                          Region=="UNFPA Total" ~ "UNFPA Total",                                    
                          Region=="UNFPA: Arab States" ~ "Arab States",                                
                          Region=="UNFPA: Asia & the Pacific"   ~ "Asia & the Pacific",                     
                          Region=="UNFPA: East & Southern Africa"    ~ "East & Southern Africa",                  
                          Region=="UNFPA: Eastern Europe & Central Asia"   ~ "Eastern Europe & Central Asia",          
                          Region=="UNFPA: Latin America & the Caribbean"  ~ "Latin America & the Caribbean",             
                          Region=="UNFPA: West & Central Africa"   ~ "West & Central Africa",   
                          Region=="UNFPA: Non-UNFPA Country"  ~ "Non-UNFPA Country" )) %>%
  select(Region, Economics_Achievement, Economics_Agency, Economics_Resource,     
         Education_Achievement, Education_Agency, Education_Resource,     
         Gender_Achievement, Gender_Agency, Gender_Resource,        
         Politics_Achievement, Politics_Agency, Politics_Resource,      
         Safety_Achievement, Safety_Agency, Safety_Resource,       
         SRH_Achievement, SRH_Agency, SRH_Resource      ) %>%
  gather(Subdomain, Score, Economics_Achievement:SRH_Resource) %>%
  separate(Subdomain, c("Domain", "Subdomain"), sep="_") %>%
  spread(Domain, Score) %>%
   rename("My Life:\nEconomic Empowerment"=  Economics,
          "My Life:\nEducational\nEmpowerment"= Education,
          "My Life:\nGender and\nAutonomy"= Gender,   
          "My World:\nYouth Policy and\nPolitical Participation"= Politics,  
          "My World:\nSafety and\nSecurity" =  Safety, 
          "My Body:\nSRH Empowerment"  = SRH) %>%
   mutate(Subdomain = fct_relevel(Subdomain,  "Resource"  , "Agency", 
                                  "Achievement")) 

# Creating smaller datasets for each chart
global_spider <- spider2 %>% filter(Region=="Global") %>% select(-Region)
arab_spider <- spider2 %>% filter(Region=="Arab States" ) %>% select(-Region)
asia_spider <- spider2 %>% filter(Region== "Asia & the Pacific" ) %>% select(-Region)
esafrica_spider <- spider2 %>% filter(Region=="East & Southern Africa" ) %>% select(-Region)
eeuropecasia_spider <- spider2 %>% filter(Region== "Eastern Europe & Central Asia") %>% select(-Region)
lac_spider <- spider2 %>% filter(Region=="Latin America & the Caribbean") %>% select(-Region)
unfpa_spider <- spider2 %>% filter(Region=="UNFPA Total") %>% select(-Region)
wcafrica_spider <- spider2 %>% filter(Region== "West & Central Africa"   ) %>% select(-Region)

ggradar(
  global_spider, 
  values.radar = c("0", ".5", "1"),
  group.colours = c( "#E4519C", "#389AD6" , "#80C3DA"),
  grid.min = 0, grid.mid = .5, grid.max = 1,  background.circle.colour = "white",
  gridline.mid.colour = "grey",
  plot.title="Global"
)

# If you would like to export these results, uncomment the line below
 ggsave("Global Domain Spider Plot 111921.jpg", dpi=300, width=14, height=8, units = "in")
 ggsave("Global Domain Spider Plot 111921.eps", dpi=300, width=14, height=8, units = "in")
 
ggradar(
  arab_spider, 
  values.radar = c("0", ".5", "1"),
  group.colours = c( "#E4519C", "#389AD6" , "#80C3DA"),
  grid.min = 0, grid.mid = .5, grid.max = 1,  background.circle.colour = "white",
  gridline.mid.colour = "grey",
  plot.title="Arab States"
)

# If you would like to export these results, uncomment the line below
 ggsave("Arab States Domain Spider Plot 111921.jpg",   dpi=300, width=14, height=8, units = "in")
 ggsave("Arab States Domain Spider Plot 111921.eps",  dpi=300, width=14, height=8, units = "in")
 
ggradar(
  asia_spider, 
  values.radar = c("0", ".5", "1"),
  group.colours = c( "#E4519C", "#389AD6" , "#80C3DA"),
  grid.min = 0, grid.mid = .5, grid.max = 1,  background.circle.colour = "white",
  gridline.mid.colour = "grey",
  plot.title="Asia & the Pacific"
)

# If you would like to export these results, uncomment the line below
ggsave("Asia Domain Spider Plot 111921.jpg",  dpi=300, width=14, height=8, units = "in")
ggsave("Asia Domain Spider Plot 111921.eps",  dpi=300, width=14, height=8, units = "in")

ggradar(
  esafrica_spider, 
  values.radar = c("0", ".5", "1"),
  group.colours = c( "#E4519C", "#389AD6" , "#80C3DA"),
  grid.min = 0, grid.mid = .5, grid.max = 1,  background.circle.colour = "white",
  gridline.mid.colour = "grey",
  plot.title="East & Southern Africa"
)

# If you would like to export these results, uncomment the line below
 ggsave("East South Africa Domain Spider Plot 111921.jpg", dpi=300, width=14, height=8, units = "in")
 ggsave("East South Africa Domain Spider Plot 111921.eps", dpi=300, width=14, height=8, units = "in")
 
ggradar(
  eeuropecasia_spider, 
  values.radar = c("0", ".5", "1"),
  group.colours = c( "#E4519C", "#389AD6" , "#80C3DA"),
  grid.min = 0, grid.mid = .5, grid.max = 1,  background.circle.colour = "white",
  gridline.mid.colour = "grey",
  plot.title="Eastern Europe & Central Asia"
)

# If you would like to export these results, uncomment the line below
ggsave("East Europe Cent Asia Domain Spider Plot 111921.jpg", dpi=300, width=14, height=8, units = "in")
ggsave("East Europe Cent Asia Domain Spider Plot 111921.eps", dpi=300, width=14, height=8, units = "in")

ggradar(
  lac_spider, 
  values.radar = c("0", ".5", "1"),
  group.colours = c( "#E4519C", "#389AD6" , "#80C3DA"),
  grid.min = 0, grid.mid = .5, grid.max = 1,  background.circle.colour = "white",
  gridline.mid.colour = "grey",
  plot.title="Latin America & the Caribbean"
)

# If you would like to export these results, uncomment the line below
 ggsave("LAC Domain Spider Plot 111921.jpg",  dpi=300, width=14, height=8, units = "in")
 ggsave("LAC Domain Spider Plot 111921.eps",  dpi=300, width=14, height=8, units = "in")
 
ggradar(
  unfpa_spider, 
  values.radar = c("0", ".5", "1"),
  group.colours = c( "#E4519C", "#389AD6" , "#80C3DA"),
  grid.min = 0, grid.mid = .5, grid.max = 1,  background.circle.colour = "white",
  gridline.mid.colour = "grey",
  plot.title="UNFPA Total"
)
# If you would like to export these results, uncomment the line below
 ggsave("UNFPA Domain Spider Plot 111921.jpg", dpi=300, width=14, height=8, units = "in")
 ggsave("UNFPA Domain Spider Plot 111921.eps", dpi=300, width=14, height=8, units = "in")
 

ggradar(
  wcafrica_spider, 
  values.radar = c("0", ".5", "1"),
  group.colours = c( "#E4519C", "#389AD6" , "#80C3DA"),
  grid.min = 0, grid.mid = .5, grid.max = 1,  background.circle.colour = "white",
  gridline.mid.colour = "grey",
  plot.title="West & Central Africa"
)
# If you would like to export these results, uncomment the line below
 ggsave("West Central Africa Domain Spider Plot 111921.jpg", dpi=300, width=14, height=8, units = "in")
 ggsave("West Central Africa Domain Spider Plot 111921.eps", dpi=300, width=14, height=8, units = "in")