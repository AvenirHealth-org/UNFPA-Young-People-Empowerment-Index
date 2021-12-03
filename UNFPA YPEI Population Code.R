# UNFPA Young People Empowerment Index
# November 2021
# Code written by Kristin Bietsch, PhD; Avenir Health; kbietsch@avenirhealth.org


# Population Prep

# The goal of this file it to create country level population for various age groups to be used for weighted the indicators for the UNFPA Young People Empowerment Index

# Population groups are both (men and women) 10-24, both 12-17, both 15-17, both 15-24, both 15-34, both 16-24, both 18-29,
# men 15-24, women 10-24, women 15-19, women 15-24, women 20-24
# and for laws that apply to countries, the weight will be 1 per country

# The packages required for this code:
library(dplyr)
library(stringr)
library(tidyr)


# Set the working directory where input files are stored and where you want results to be saved
setwd("C:/Users/KristinBietsch/files/UNFPA/Code for GitHub")



#####################################################################################
# We are loading a single sheet from the World Population Prospects, the estimates of female population by age group
# Additional data can be downloaded from:
#https://population.un.org/wpp/Download/Standard/Population/
pop_by_age_female <- read.csv("Input data/WPP2019_INT_F03_3_POPULATION_BY_AGE_ANNUAL_FEMALE_Estimates.csv")

# Dataframe is transformed into a matrix
pop_female_matrix <- as.matrix(pop_by_age_female)
# select only the rows and columns we want from the matrix
pop_female_matrix_clean <- pop_female_matrix[17:18121, c( 5, 8, 9:109)]

pop_female_clean <- as.data.frame(pop_female_matrix_clean) %>%  # transforming back into a dataframe
  rename(ISONum=X.4, Year=X.7) %>%  # renameing headers
  gather(AgeFull, FemalePop, X.8 : X.108)  %>% # transforming the dataset from one row per country to one row per country and age
  mutate(Age =  str_remove(AgeFull, "[X]")) %>%
  mutate(Age =  as.numeric(str_remove(Age, "[.]"))-8) %>% # transforming the age variable into a numeric variable
  filter(FemalePop!="..." ) %>% # removing missing data
  mutate(FemalePop = as.numeric(gsub("[[:space:]]", "", FemalePop))*1000) %>% # removing the space in the thousands place and multiplying by 1000
  filter(Year==2020) %>% # only using population in 2020
  select(ISONum, Age, FemalePop) # selection country ISONumber, Age, and Population

# Creating the dataframe for just women 10-24
women1024 <- pop_female_clean %>% 
  filter(Age>=10 & Age<=24) %>%
  group_by(ISONum) %>%
  summarise(Women1024=sum(FemalePop)) # adding together all the populations for ages 10-24 by country

women1519 <- pop_female_clean %>%
  filter(Age>=15 & Age<=19) %>%
  group_by(ISONum) %>%
  summarise(Women1519=sum(FemalePop))

women1524 <- pop_female_clean %>%
  filter(Age>=15 & Age<=24) %>%
  group_by(ISONum) %>%
  summarise(Women1524=sum(FemalePop))

women2024 <- pop_female_clean %>%
  filter(Age>=20 & Age<=24) %>%
  group_by(ISONum) %>%
  summarise(Women2024=sum(FemalePop))


women1549 <- pop_female_clean %>%
  filter(Age>=15 & Age<=49) %>%
  group_by(ISONum) %>%
  summarise(Women1549=sum(FemalePop))

#####################################################################################
pop_by_age_both <- read.csv("Input data/WPP2019_INT_F03_1_POPULATION_BY_AGE_ANNUAL_BOTH_SEXES_Estimates.csv")

#Both_1024
#Both_1217
#Both_1517
#Both_1524
#Both_1534
#Both_1624
#Both_1829

pop_both_matrix <- as.matrix(pop_by_age_both)
pop_both_matrix_clean <- pop_both_matrix[17:18121, c( 5, 8, 9:109)]

pop_both_clean <- as.data.frame(pop_both_matrix_clean) %>% 
  rename(ISONum=X.4, Year=X.7) %>% 
  gather(AgeFull, Pop, X.8 : X.108)  %>%
  mutate(Age =  str_remove(AgeFull, "[X]")) %>%
  mutate(Age =  as.numeric(str_remove(Age, "[.]"))-8) %>%
  filter(Pop!="..." ) %>%
  mutate(Pop = as.numeric(gsub("[[:space:]]", "", Pop))*1000) %>%
  filter(Year==2020) %>%
  select(ISONum, Age, Pop)

bothpop1217 <- pop_both_clean %>%
  filter(Age>=12 & Age<=17) %>%
  group_by(ISONum) %>%
  summarise(Both1217=sum(Pop))

bothpop1524 <- pop_both_clean %>%
  filter(Age>=15 & Age<=24) %>%
  group_by(ISONum) %>%
  summarise(Both1524=sum(Pop))

bothpop1624 <- pop_both_clean %>%
  filter(Age>=16 & Age<=24) %>%
  group_by(ISONum) %>%
  summarise(Both1624=sum(Pop))

bothpop1024 <- pop_both_clean %>%
  filter(Age>=10 & Age<=24) %>%
  group_by(ISONum) %>%
  summarise(Both1024=sum(Pop))

bothpop1517 <- pop_both_clean %>%
  filter(Age>=15 & Age<=17) %>%
  group_by(ISONum) %>%
  summarise(Both1517=sum(Pop))

bothpop1534 <- pop_both_clean %>%
  filter(Age>=15 & Age<=34) %>%
  group_by(ISONum) %>%
  summarise(Both1534=sum(Pop))

bothpop1829 <- pop_both_clean %>%
  filter(Age>=18 & Age<=29) %>%
  group_by(ISONum) %>%
  summarise(Both1829=sum(Pop))


########################################################################
pop_by_age_male <- read.csv("Input data/WPP2019_INT_F03_2_POPULATION_BY_AGE_ANNUAL_MALE_Estimates.csv")

pop_male_matrix <- as.matrix(pop_by_age_male)
pop_male_matrix_clean <- pop_male_matrix[17:18121, c( 5, 8, 9:109)]

pop_male_clean <- as.data.frame(pop_male_matrix_clean) %>% 
  rename(ISONum=X.4, Year=X.7) %>% 
  gather(AgeFull, MalePop, X.8 : X.108) %>%
  mutate(Age =  str_remove(AgeFull, "[X]")) %>%
  mutate(Age =  as.numeric(str_remove(Age, "[.]"))-8) %>%
  filter(MalePop!="..." ) %>%
  mutate(MalePop = as.numeric(gsub("[[:space:]]", "", MalePop))*1000) %>%
  filter(Year==2020) %>%
  select(ISONum, Age, MalePop)

men1524 <- pop_male_clean %>%
  filter(Age>=15 & Age<=24) %>%
  group_by(ISONum) %>%
  summarise(Men1524=sum(MalePop))

########################################################################

# Combining all the population groups we have created
population_data <- full_join(bothpop1024, bothpop1217, by="ISONum") %>%
  full_join(bothpop1524, by="ISONum") %>%
  full_join(bothpop1517, by="ISONum") %>%
  full_join(bothpop1534, by="ISONum") %>%
  full_join(bothpop1624, by="ISONum") %>%
  full_join(bothpop1829, by="ISONum") %>%
  full_join(men1524, by="ISONum") %>%
  full_join(women1024, by="ISONum") %>%
  full_join(women1519, by="ISONum") %>%
  full_join(women1524, by="ISONum") %>%
  full_join(women2024, by="ISONum") %>%
  full_join(women1549, by="ISONum") %>%
  mutate(One=1)


# This file will save to your working directory.  It wil be used in the index creation file.
write.csv(population_data, "Input Data/WPP2019 Data for YPEI.csv", row.names = F, na="")
