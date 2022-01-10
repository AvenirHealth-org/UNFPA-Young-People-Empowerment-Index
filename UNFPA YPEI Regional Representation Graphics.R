# Creating UNFPA Young People Empowerment Index Graphics 2
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


# Set the working directory where input files are stored and where you want results to be saved
setwd("C:/Users/KristinBietsch/files/UNFPA/Code for GitHub/Input Data")

regional <- read.csv("YEI Regional PopRepresented 120721.csv")

domains <- read.csv("UNFPA Youth Index Scales and Weights 120721.csv") %>% select(Domain, Subdomain, Indicator)

regional_long <- regional %>% gather(Indicator, Value, Account_Ratio:YouthPolicy_Num) %>%
  mutate(Value = replace_na(Value, 0)) %>%
  full_join(domains, by="Indicator") %>%
  mutate(Domain=case_when(
    Domain=="Economic Empowerment"   ~ "Economic\nEmpowerment"  ,                
    Domain=="Education"   ~ "Educational\nEmpowerment" ,                           
    Domain=="Gender and Autonomy" ~     "Gender and\nAutonomy"     ,             
    Domain=="Safety and Security"   ~     "Safety and\nSecurity" ,             
    Domain=="SRH Empowerment"    ~ "SRH\nEmpowerment" ,                         
    Domain=="Youth Policy and Political Participation" ~ "Youth Policy\nand Political\nParticipation")) %>%
  mutate(Subdomain = fct_relevel(Subdomain,  "Resource"  , "Agency", 
                                 "Achievement")) 

levels(as.factor(regional$Region))

ggplot(subset(regional_long, Region== "UNFPAGroup: Arab States" ), aes(x=Domain, y=Value, color=Domain, shape=Subdomain))+
  geom_point(size=6) +
  ylim(0, 1) +
  scale_shape_manual(values=c(0, 1, 2))+
  annotate("text", x=1, y=.285, label="Youth\nEmployment\nStrategy") +
  annotate("text", x=2, y=.385, label="Laws for\nSexuality\nEducation") +
  annotate("text", x=3, y=.2, label="Women\nExperienced\nViolence") +
  annotate("text", x=4, y=.66, label="Political Action") +
  annotate("text", x=5, y=0, label="Men's knowledge questions") +
  annotate("text", x=5, y=.14, label="Demand Satisfied") +
  annotate("text", x=6, y=.24, label="Government\nCollect\nData") +
  guides(colour = FALSE) +
  labs(title="Arab States", y="Proportion of Population Represented") +
  theme_bw()

ggsave("Arab State Indicator Representation.jpg", dpi=300, height=8, width=8, unit="in")
ggsave("Arab State Indicator Representation.eps", dpi=300, height=8, width=8, unit="in")


ggplot(subset(regional_long, Region==  "UNFPAGroup: Asia & the Pacific"     ), aes(x=Domain, y=Value, color=Domain, shape=Subdomain))+
  geom_point(size=6) +
  ylim(0, 1) +
  scale_shape_manual(values=c(0, 1, 2))+
  annotate("text", x=1, y=.47, label="Youth\nEmployment\nStrategy") +
  annotate("text", x=2, y=.475, label="Laws for\nSexuality\nEducation") +
  annotate("text", x=3, y=.441, label="Restricted\nCivil\nLiberties") +
  annotate("text", x=4, y=.857, label="Political\nAction\nand\nRespect for\nHuman Rights") +
  annotate("text", x=5, y=.193, label="HIV Incidence") +
  annotate("text", x=6, y=.547, label="Government\nCollect\nData") +
  guides(colour = FALSE) +
  labs(title="Asia & the Pacific", y="Proportion of Population Represented") +
  theme_bw()

ggsave("Asia and the Pacific Indicator Representation.jpg", dpi=300, height=8, width=8, unit="in")
ggsave("Asia and the Pacific Indicator Representation.eps", dpi=300, height=8, width=8, unit="in")

ggplot(subset(regional_long, Region== "UNFPAGroup: East & Southern Africa"    ), aes(x=Domain, y=Value, color=Domain, shape=Subdomain))+
  geom_point(size=6) +
  ylim(0, 1) +
  annotate("text", x=1, y=.16, label="Better\nStandard\nof Living") +
  annotate("text", x=2, y=.385, label="Laws for\nSexuality\nEducation") +
  annotate("text", x=3, y=.66, label="Restricted\nCivil\nLiberties") +
  annotate("text", x=4, y=.223, label="Political\nAction\nand\nRespect for\nHuman Rights") +
  annotate("text", x=5, y=.3, label="Laws for\nMaternity\nCare") +
  annotate("text", x=6, y=.26, label="Vote and\nPolitical\nParticipation") +
  scale_shape_manual(values=c(0, 1, 2))+
  guides(colour = FALSE) +
  labs(title="East & Southern Africa", y="Proportion of Population Represented") +
  theme_bw()

ggsave("East and Southern Africa Indicator Representation.jpg", dpi=300, height=8, width=8, unit="in")
ggsave("East and Southern Africa Indicator Representation.eps", dpi=300, height=8, width=8, unit="in")

ggplot(subset(regional_long, Region== "UNFPAGroup: Eastern Europe & Central Asia"), aes(x=Domain, y=Value, color=Domain, shape=Subdomain))+
  geom_point(size=6) +
  ylim(0, 1) +
  annotate("text", x=1, y=.616, label="Better\nStandard\nof Living") +
  annotate("text", x=2, y=.431, label="Laws for\nSexuality\nEducation") +
  annotate("text", x=3, y=.187, label="Women\nExperienced\nViolence") +
  annotate("text", x=4, y=.7, label="Political\nAction") +
  annotate("text", x=5, y=.178, label="Men's HIV Knowledge") +
  annotate("text", x=5.6, y=.24, label="Men's Fertility\nKnowledge") +
  annotate("text", x=6, y=.5, label="Government\nCollect\nData") +
  scale_shape_manual(values=c(0, 1, 2))+
  guides(colour = FALSE) +
  labs(title="Eastern Europe & Central Asia", y="Proportion of Population Represented") +
  theme_bw()

ggsave("Eastern Europe and Central Asia Indicator Representation.jpg", dpi=300, height=8, width=8, unit="in")
ggsave("Eastern Europe and Central Asia Indicator Representation.eps", dpi=300, height=8, width=8, unit="in")

ggplot(subset(regional_long, Region== "UNFPAGroup: Latin America & the Caribbean"  ), aes(x=Domain, y=Value, color=Domain, shape=Subdomain))+
  geom_point(size=6) +
  ylim(0, 1) +
  annotate("text", x=1, y=.487, label="Youth\nEmployment\nStrategy") +
  annotate("text", x=2, y=.457, label="Laws for\nSexuality\nEducation") +
  annotate("text", x=3, y=.16, label="Women\nExperienced\nViolence") +
  annotate("text", x=4, y=.76, label="Political\nAction\nand\nRespect for\nHuman Rights") +
  annotate("text", x=5, y=.122, label="Men's HIV\nKnowledge") +
  annotate("text", x=4.4, y=.2, label="Men's Fertility\nKnowledge") +
  annotate("text", x=5.6, y=.24, label="Women's HIV\nKnowledge") +
  annotate("text", x=6, y=.32, label="Government\nCollect Data") +
  scale_shape_manual(values=c(0, 1, 2))+
  guides(colour = FALSE) +
  labs(title="Latin America & the Caribbean", y="Proportion of Population Represented") +
  theme_bw()

ggsave("Latin America and the Caribbean Indicator Representation.jpg", dpi=300, height=8, width=8, unit="in")
ggsave("Latin America and the Caribbean Indicator Representation.eps", dpi=300, height=8, width=8, unit="in")

ggplot(subset(regional_long, Region==  "UNFPAGroup: West & Central Africa" ), aes(x=Domain, y=Value, color=Domain, shape=Subdomain))+
  geom_point(size=6) +
  ylim(0, 1) +
  annotate("text", x=1, y=.37, label="Better\nStandard\nof Living") +
  annotate("text", x=2, y=.796, label="Laws for\nSexuality\nEducation") +
  annotate("text", x=3, y=.757, label="Restricted\nCivil\nLiberties") +
  annotate("text", x=4, y=.403, label="Political\nAction\nand\nRespect for\nHuman Rights") +
  annotate("text", x=5, y=.58, label="Laws for\nMaternity\nCare") +
  annotate("text", x=6, y=.44, label="Vote and\nPolitical\nParticipation") +
  scale_shape_manual(values=c(0, 1, 2))+
  guides(colour = FALSE) +
  labs(title="West & Central Africa", y="Proportion of Population Represented") +
  theme_bw()

ggsave("West and Central Africa Indicator Representation.jpg", dpi=300, height=8, width=8, unit="in")
ggsave("West and Central Africa Indicator Representation.eps", dpi=300, height=8, width=8, unit="in")

ggplot(subset(regional_long, Region==  "UNFPA Total"  ), aes(x=Domain, y=Value, color=Domain, shape=Subdomain))+
  geom_point(size=6) +
  ylim(0, 1) +
  annotate("text", x=1, y=.463, label="Better\nStandard\nof Living") +
  annotate("text", x=2, y=.49, label="Laws for\nSexuality\nEducation") +
  annotate("text", x=3, y=.512, label="Women\nExperienced\nViolence") +
  annotate("text", x=4, y=.71, label="Political\nAction\nand\nRespect for\nHuman Rights") +
  annotate("text", x=5, y=.37, label="HIV Incidence\nand Laws for\nMaternity Care") +
  annotate("text", x=6, y=.5, label="Government\nCollect\nData") +
  scale_shape_manual(values=c(0, 1, 2))+
  guides(colour = FALSE) +
  labs(title="UNFPA Countries", y="Proportion of Population Represented") +
  theme_bw()

ggsave("UNFPA Countries Indicator Representation.jpg", dpi=300, height=8, width=8, unit="in")
ggsave("UNFPA Countries Indicator Representation.eps", dpi=300, height=8, width=8, unit="in")
