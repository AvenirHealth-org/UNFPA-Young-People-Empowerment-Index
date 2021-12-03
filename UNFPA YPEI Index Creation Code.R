# Creating UNFPA Young People Empowerment Index
# November 2021
# Code written by Kristin Bietsch, PhD; Avenir Health; kbietsch@avenirhealth.org

# The packages required for this code:
library(survey)
library(tibble)
library(dplyr)
library(tidyr)
library(haven)
library(stringr)
library(questionr)
library(sjlabelled)

options(scipen=999)
memory.limit(size = 2e6) 


# Set the working directory where input files are stored and where you want results to be saved
setwd("C:/Users/KristinBietsch/files/UNFPA/Code for GitHub/Input Data")

# This file is very important, it lists all the indicators, the min and max for scaling,
# the weights of the indicator within domain/subdomain, and the population used for weighting
scales <- read.csv("UNFPA Youth Index Scales and Weights 111221.csv")

# This file lists all countries and their UNFPA regions
countryiso <- read.csv("Country Regions Large.csv") 

# This file is the population data we created in UNFPA YPEI Population Code
population <- read.csv("WPP2019 Data for YPEI.csv")

# Some countriers are very small and the UN does not make population estimates for the 
# age breakdowns, we want to identify these countries so we can exclude them from analysis
poptest <- full_join(countryiso, population, by="ISONum")
exclude_pop <- poptest %>% filter(is.na(Both1024)) %>% select(ISONum) %>% mutate(exclude=1)

# Reshaping population data from wide to long
population <- population %>%
  gather(Population, Pop, Both1024:One)


# This is the full dataset of input data we created in UNFPA YPEI Input Data Code
full_data <- read.csv("Full Input Dataset 111221.csv")

analysis_data <- full_data  %>%
  full_join(countryiso, by="ISONum") %>% 
  filter(!is.na(Country)) %>%
  mutate(UpperSec_Ratio=UpperSec_Male/UpperSec_Female,
         Account_Ratio=account_t_d_5/account_t_d_6)   %>%
  full_join(exclude_pop, by="ISONum") %>% filter(is.na(exclude)) %>% # Excluding due to lack of population estimates
  select(ISONum,  Country, ISOAlph,   Subregion,   SDG,  UNFPAGroup, UNFPATotal, Global,
         SH_LGR_ACSRHES1,   SH_LGR_ACSRHES2,  SH_LGR_ACSRHES3, SH_LGR_ACSRHES4, 
         ProviderDisc,  HIVKnow_w_1524,   HIVKnow_m_1524,   knows_ltm_stm_w,  knows_ltm_stm_m, 
         fert_know_w, fert_know_m,  DS_1524,   HIV,   ASFR15, 
         SH_STA_MORT,    UpperSec_Ratio, Res_Civil,  SP_DYN_MRBF18,  violence_w_1524, 
         Educ_GDP, SE.SEC.ENRR,   UpperSec_Both,  gwp1_n_5,  gwp2_n_5, 
         SL_CPA_YEMP, Account_Ratio,   SL_TLF_NEET_Both,  BetterOff,   WorkPov,  YouthPolicy_Num, 
         CollectData,  Vote_National,  active_member,  Parliament30,  INFORM, 
         pol_part,  humanrights,  Displacement,  InterViolence,  SelfHarm)

###########################################################################
# For each indicator, we want to know the percent of the population represented by the data
# This section of code calulates how many people (of each population category) live in each region

total_pop <- full_join(population, exclude_pop, by="ISONum") %>% 
  filter(is.na(exclude)) %>% select(-exclude) %>%
  left_join(countryiso, by="ISONum")

SDG_pop <- total_pop %>% filter(SDG!="") %>% group_by(Population, SDG) %>% summarise(Pop=sum(Pop))
UNFPAGroup_pop <- total_pop %>% filter(UNFPAGroup!="") %>% group_by(Population, UNFPAGroup) %>% summarise(Pop=sum(Pop))
UNFPATotal_pop <- total_pop %>% filter(UNFPATotal!="") %>% group_by(Population, UNFPATotal) %>% summarise(Pop=sum(Pop))
Global_pop <-  total_pop %>% filter(Global!="") %>% group_by(Population, Global) %>% summarise(Pop=sum(Pop))
###########################################################################
# Filling in Missing Data

# List of Indicators
indicators <- scales %>% select(Indicator) %>% mutate(score=1)

regional_data <- analysis_data %>%
  select(  -Country, -ISOAlph, -Subregion, -UNFPAGroup, -UNFPATotal, -Global ) %>% # removing unnecessary variables
  gather(Indicator, Value, SH_LGR_ACSRHES1:SelfHarm) %>%
  full_join(scales, by="Indicator") %>%
  full_join(population, by=c("ISONum", "Population")) %>%
  filter(!is.na(Value))  %>%
  group_by(SDG, Indicator) %>%
  mutate(Pop_Indic_Region= sum(Pop),    # The population for that variable and SDG region
         share_Pop_Indic_Region= Pop/Pop_Indic_Region) %>%  # The share of the population (of countries with available data) for that variable and SDG region
  summarise(Value_regional=sum(Value * share_Pop_Indic_Region), # Population weighted regional value
            Pop_w_Data=sum(Pop)) %>% ungroup() # The population for that variable and SDG region


# Notes on Missing data
# If a country has missing data, SDG regional averages are used.
# In a few cases (for Australia/New Zealand and Pacific Islands) no regional averages exist
  # For missing data from Australia/New Zealand, take from Europe
  # For missing data for Pacific Islands, take from Eastern and South Eastern Asia
# Note: The code will not produce regional results for Austrailia/New Zealand or Oceania

# To be used for country missing data
regional2 <-  regional_data %>% select(-Pop_w_Data) %>% spread(SDG, Value_regional) %>% full_join(indicators, by="Indicator") %>%
  select(-score) %>% gather(SDG, Value, "Australia/New Zealand" : "Sub-Saharan Africa") %>%
  filter(SDG=="Europe and Northern America" | SDG=="Eastern and South-Eastern Asia") %>%
  mutate(SDG=case_when(SDG=="Europe and Northern America" ~ "Australia/New Zealand",
                       SDG=="Eastern and South-Eastern Asia" ~  "Oceania (excluding Australia and New Zealand)")) %>%
  rename(Value2=Value)

regional2_data <-  regional_data %>% select(-Pop_w_Data) %>% spread(SDG, Value_regional) %>% full_join(indicators, by="Indicator") %>%
  select(-score) %>% gather(SDG, Value, "Australia/New Zealand" : "Sub-Saharan Africa") %>%
  full_join(regional2, by=c("Indicator", "SDG")) %>%
  rename(Value1=Value)


#################################################################################
#################################################################################
#### This next section of code calculates regional results for SDG regions,  ####
#### UNFPA regions, all UNFPA countries, and all countries                   ####
#################################################################################
#################################################################################

# SDG Results

#################################################################################
# For each indicator and SDG regions, the share of the population represented by the data  
regional_Pop_W_data <- regional_data %>% select(SDG, Indicator, Pop_w_Data) %>% 
  full_join(scales, by="Indicator") %>% full_join(SDG_pop, by=c("SDG", "Population")) %>%
  mutate(Pop_Share=Pop_w_Data/Pop) %>% # the share of the total population represented with available data
  select(SDG, Indicator, Pop_Share) %>%
  filter(!is.na(Pop_Share)) %>%
  spread(Indicator, Pop_Share)  %>%
  rename(Region=SDG) %>%
  mutate(Region=paste("SDG", Region, sep=": "))


regional_results <- regional2_data %>% select(-Value2) %>% # We are not producing results for regions with no data
  rename(Full_Value=Value1) %>%
  full_join(scales, by="Indicator") %>%
  mutate(Capped_Value = case_when(Indicator=="DS_1524" & Full_Value>75 ~ 75, # we are capping some indicators as noted in "scales"
                                  Indicator=="UpperSec_Ratio" & Full_Value < 1 ~ 1,
                                  Indicator=="SE.SEC.ENRR" & Full_Value >100  ~ 100,
                                  Indicator=="Account_Ratio" & Full_Value >1  ~ 1,
                                  TRUE ~ Full_Value)) %>% # capped data
  mutate(Scaled_Value= (Capped_Value - Min)/( Max - Min )) # Scaling the indicator based on Min and Max in "scales"

scale_results_regional <- regional_results %>%
  mutate(Domain=case_when(Domain==  "Economic Empowerment"  ~ "Economics",
                          Domain==   "Education"   ~ "Education",
                          Domain== "Gender and Autonomy" ~ "Gender",
                          Domain==    "Safety and Security" ~ "Safety",
                          Domain==  "SRH Empowerment"      ~ "SRH",
                          Domain== "Youth Policy and Political Participation" ~ "Politics")) %>% 
  mutate(Indicator= paste(Domain, Subdomain, Indicator, sep="_")) %>%
  select(SDG, Indicator,  Scaled_Value) %>%
  spread(Indicator, Scaled_Value)

sub_domain_regional <- regional_results %>% 
  select(SDG, Domain,  Subdomain, SubWeight, DomainWeight, Scaled_Value) %>%
  group_by(SDG, Domain, Subdomain) %>%
  summarise(Value= sum(Scaled_Value * SubWeight)) %>% # creating weighted subdomain scores
  ungroup() %>%
  mutate(Domain=case_when(Domain==  "Economic Empowerment"  ~ "Economics",
                          Domain==   "Education"   ~ "Education",
                          Domain== "Gender and Autonomy" ~ "Gender",
                          Domain==    "Safety and Security" ~ "Safety",
                          Domain==  "SRH Empowerment"      ~ "SRH",
                          Domain== "Youth Policy and Political Participation" ~ "Politics")) %>%
  mutate(Group=paste(Domain, Subdomain, sep="_")) %>% select(-Domain, -Subdomain) %>%
  spread(Group, Value)

domain_regional <- regional_results %>% 
  select(SDG, Domain,  Subdomain, SubWeight, DomainWeight, Scaled_Value) %>%
  group_by(SDG, Domain) %>%
  summarise(Value= sum(Scaled_Value * SubWeight * DomainWeight)) %>% # Creating weighted domain scores
  ungroup() %>%
  mutate(Domain=case_when(Domain==  "Economic Empowerment"  ~ "Economics",
                          Domain==   "Education"   ~ "Education",
                          Domain== "Gender and Autonomy" ~ "Gender",
                          Domain==    "Safety and Security" ~ "Safety",
                          Domain==  "SRH Empowerment"      ~ "SRH",
                          Domain== "Youth Policy and Political Participation" ~ "Politics")) %>%
  spread(Domain, Value) %>%
  mutate(Index = (Economics * Education * Gender * Safety * SRH * Politics)^(1/6)) %>%
  full_join(sub_domain_regional, by="SDG") %>% # joining the subdomain data
  full_join(scale_results_regional, by="SDG") %>%  # joining the variable data
  rename(Region=SDG) %>%
  mutate(Region=paste("SDG", Region, sep=": "))




###################################################################################################
# Repeating what was done about for SDGs, but for UNFPA country groupings
# see notes in SDG section above

UNFPAGroup_data <- analysis_data %>%
  select(  -Country, -ISOAlph,   -Subregion,   -SDG, -UNFPATotal, -Global,) %>%
  gather(Indicator, Value, SH_LGR_ACSRHES1:SelfHarm) %>%
  full_join(scales, by="Indicator") %>%
  full_join(population, by=c("ISONum", "Population")) %>%
  filter(!is.na(Value)) %>%
  group_by(UNFPAGroup, Indicator)  %>%
  mutate(Pop_Indic_Region= sum(Pop),
         share_Pop_Indic_Region= Pop/Pop_Indic_Region) %>% 
  summarise(Full_Value=sum(Value * share_Pop_Indic_Region),
            Pop_w_Data=sum(Pop)) %>% ungroup()

UNFPAGroup_Pop_W_data <- UNFPAGroup_data %>% select(UNFPAGroup, Indicator, Pop_w_Data) %>% 
  full_join(scales, by="Indicator") %>% full_join(UNFPAGroup_pop, by=c("UNFPAGroup", "Population")) %>%
  mutate(Pop_Share=Pop_w_Data/Pop) %>%
  select(UNFPAGroup, Indicator, Pop_Share) %>%
  filter(!is.na(Pop_Share)) %>%
  spread(Indicator, Pop_Share)  %>%
  rename(Region=UNFPAGroup) %>%
  mutate(Region=paste("UNFPAGroup", Region, sep=": "))


UNFPAGroup_results <-  UNFPAGroup_data %>% select(-Pop_w_Data) %>%
  spread(UNFPAGroup, Full_Value) %>% full_join(indicators, by="Indicator") %>%
  select(-score) %>% gather(UNFPAGroup, Full_Value,  "Arab States" :    "West & Central Africa"  ) %>%
  full_join(scales, by="Indicator") %>%
  mutate(Capped_Value = case_when(Indicator=="DS_1524" & Full_Value>75 ~ 75,
                                  Indicator=="UpperSec_Ratio" & Full_Value < 1 ~ 1,
                                  Indicator=="SE.SEC.ENRR" & Full_Value >100  ~ 100,
                                  Indicator=="Account_Ratio" & Full_Value >1  ~ 1,
                                  TRUE ~ Full_Value)) %>% # capped data
  mutate(Scaled_Value= (Capped_Value - Min)/( Max - Min )) 


scale_results_UNFPAGroup <- UNFPAGroup_results %>%
  mutate(Domain=case_when(Domain==  "Economic Empowerment"  ~ "Economics",
                          Domain==   "Education"   ~ "Education",
                          Domain== "Gender and Autonomy" ~ "Gender",
                          Domain==    "Safety and Security" ~ "Safety",
                          Domain==  "SRH Empowerment"      ~ "SRH",
                          Domain== "Youth Policy and Political Participation" ~ "Politics")) %>% 
  mutate(Indicator= paste(Domain, Subdomain, Indicator, sep="_")) %>%
  select(UNFPAGroup, Indicator,  Scaled_Value) %>%
  spread(Indicator, Scaled_Value)

sub_domain_UNFPAGroup <- UNFPAGroup_results %>% 
  select(UNFPAGroup, Domain,  Subdomain, SubWeight, DomainWeight, Scaled_Value) %>%
  group_by(UNFPAGroup, Domain, Subdomain) %>%
  summarise(Value= sum(Scaled_Value * SubWeight))  %>%
  ungroup() %>%
  mutate(Domain=case_when(Domain==  "Economic Empowerment"  ~ "Economics",
                          Domain==   "Education"   ~ "Education",
                          Domain== "Gender and Autonomy" ~ "Gender",
                          Domain==    "Safety and Security" ~ "Safety",
                          Domain==  "SRH Empowerment"      ~ "SRH",
                          Domain== "Youth Policy and Political Participation" ~ "Politics")) %>%
  mutate(Group=paste(Domain, Subdomain, sep="_")) %>% select(-Domain, -Subdomain) %>%
  spread(Group, Value)

domain_UNFPAGroup <- UNFPAGroup_results %>% 
  select(UNFPAGroup, Domain,  Subdomain, SubWeight, DomainWeight, Scaled_Value) %>%
  group_by(UNFPAGroup, Domain) %>%
  summarise(Value= sum(Scaled_Value * SubWeight * DomainWeight)) %>%
  ungroup() %>%
  mutate(Domain=case_when(Domain==  "Economic Empowerment"  ~ "Economics",
                          Domain==   "Education"   ~ "Education",
                          Domain== "Gender and Autonomy" ~ "Gender",
                          Domain==    "Safety and Security" ~ "Safety",
                          Domain==  "SRH Empowerment"      ~ "SRH",
                          Domain== "Youth Policy and Political Participation" ~ "Politics")) %>%
  spread(Domain, Value) %>%
  mutate(Index = (Economics * Education * Gender * Safety * SRH * Politics)^(1/6)) %>%
  full_join(sub_domain_UNFPAGroup, by="UNFPAGroup") %>%
  full_join(scale_results_UNFPAGroup, by="UNFPAGroup") %>%  # joining the variable data
  rename(Region=UNFPAGroup) %>%
  mutate(Region=paste("UNFPA", Region, sep=": "))



########################################################################################
# Repeating what was done about for UNFPA country combined
# see notes in SDG section above


UNFPATotal_data <- analysis_data %>%
  select(  -Country, -ISOAlph,   -Subregion,   -SDG,  -UNFPAGroup, -Global) %>%
  gather(Indicator, Value, SH_LGR_ACSRHES1:SelfHarm)  %>%
  full_join(scales, by="Indicator") %>%
  full_join(population, by=c("ISONum", "Population")) %>%
  filter(!is.na(Value))  %>%
  group_by(UNFPATotal, Indicator)  %>%
  mutate(Pop_Indic_Region= sum(Pop),
         share_Pop_Indic_Region= Pop/Pop_Indic_Region) %>% 
  summarise(Full_Value=sum(Value * share_Pop_Indic_Region),
            Pop_w_Data=sum(Pop)) %>% ungroup()  

UNFPATotal_Pop_W_data <- UNFPATotal_data %>% select(UNFPATotal, Indicator, Pop_w_Data)  %>% 
  full_join(scales, by="Indicator") %>% full_join(UNFPATotal_pop, by=c("UNFPATotal", "Population")) %>%
  mutate(Pop_Share=Pop_w_Data/Pop) %>%
  select(UNFPATotal, Indicator, Pop_Share) %>%
  filter(!is.na(Pop_Share)) %>%
  spread(Indicator, Pop_Share)  %>%
  rename(Region=UNFPATotal) %>%
  filter(Region=="UNFPA Country") %>%
  mutate(Region= "UNFPA Total")


UNFPATotal_results <- UNFPATotal_data  %>% select(-Pop_w_Data) %>%
  spread(UNFPATotal, Full_Value) %>% full_join(indicators, by="Indicator") %>%
  select(-score) %>% gather(UNFPATotal, Full_Value,   "Non-UNFPA Country": "UNFPA Country"  ) %>%
  filter(UNFPATotal!= "Non-UNFPA Country")  %>%
  full_join(scales, by="Indicator") %>%
  mutate(Capped_Value = case_when(Indicator=="DS_1524" & Full_Value>75 ~ 75,
                                  Indicator=="UpperSec_Ratio" & Full_Value < 1 ~ 1,
                                  Indicator=="SE.SEC.ENRR" & Full_Value >100  ~ 100,
                                  Indicator=="Account_Ratio" & Full_Value >1  ~ 1,
                                  TRUE ~ Full_Value)) %>% # capped data
  mutate(Scaled_Value= (Capped_Value - Min)/( Max - Min ))

scale_results_UNFPATotal <- UNFPATotal_results %>%
  mutate(Domain=case_when(Domain==  "Economic Empowerment"  ~ "Economics",
                          Domain==   "Education"   ~ "Education",
                          Domain== "Gender and Autonomy" ~ "Gender",
                          Domain==    "Safety and Security" ~ "Safety",
                          Domain==  "SRH Empowerment"      ~ "SRH",
                          Domain== "Youth Policy and Political Participation" ~ "Politics")) %>% 
  mutate(Indicator= paste(Domain, Subdomain, Indicator, sep="_")) %>%
  select(UNFPATotal, Indicator,  Scaled_Value) %>%
  spread(Indicator, Scaled_Value)

sub_domain_UNFPATotal <- UNFPATotal_results %>% 
  select(UNFPATotal, Domain,  Subdomain, SubWeight, DomainWeight, Scaled_Value) %>%
  group_by(UNFPATotal, Domain, Subdomain) %>%
  summarise(Value= sum(Scaled_Value * SubWeight))  %>%
  ungroup() %>%
  mutate(Domain=case_when(Domain==  "Economic Empowerment"  ~ "Economics",
                          Domain==   "Education"   ~ "Education",
                          Domain== "Gender and Autonomy" ~ "Gender",
                          Domain==    "Safety and Security" ~ "Safety",
                          Domain==  "SRH Empowerment"      ~ "SRH",
                          Domain== "Youth Policy and Political Participation" ~ "Politics")) %>%
  mutate(Group=paste(Domain, Subdomain, sep="_")) %>% select(-Domain, -Subdomain) %>%
  spread(Group, Value)

domain_UNFPATotal <- UNFPATotal_results %>% 
  select(UNFPATotal, Domain,  Subdomain, SubWeight, DomainWeight, Scaled_Value) %>%
  group_by(UNFPATotal, Domain) %>%
  summarise(Value= sum(Scaled_Value * SubWeight * DomainWeight)) %>%
  ungroup() %>%
  mutate(Domain=case_when(Domain==  "Economic Empowerment"  ~ "Economics",
                          Domain==   "Education"   ~ "Education",
                          Domain== "Gender and Autonomy" ~ "Gender",
                          Domain==    "Safety and Security" ~ "Safety",
                          Domain==  "SRH Empowerment"      ~ "SRH",
                          Domain== "Youth Policy and Political Participation" ~ "Politics")) %>%
  spread(Domain, Value) %>%
  mutate(Index = (Economics * Education * Gender * Safety * SRH * Politics)^(1/6)) %>%
  full_join(sub_domain_UNFPATotal, by="UNFPATotal") %>%
  full_join(scale_results_UNFPATotal, by="UNFPATotal") %>%  # joining the variable data
  rename(Region=UNFPATotal) %>%
  mutate(Region= "UNFPA Total")
##############################################################################################################
# Repeating what was done about for all countryes
# see notes in SDG section above

Global_data <- analysis_data %>%
  select( -Country, -ISOAlph,   -Subregion,   -SDG,  -UNFPAGroup, -UNFPATotal) %>%
  gather(Indicator, Value, SH_LGR_ACSRHES1:SelfHarm) %>%
  full_join(scales, by="Indicator") %>%
  full_join(population, by=c("ISONum", "Population")) %>%
  filter(!is.na(Value))  %>%
  group_by(Global, Indicator)  %>%
  mutate(Pop_Indic_Region= sum(Pop),
         share_Pop_Indic_Region= Pop/Pop_Indic_Region) %>% 
  summarise(Full_Value=sum(Value * share_Pop_Indic_Region),
            Pop_w_Data=sum(Pop)) %>% ungroup() 

Global_Pop_W_data <- Global_data %>% select(Global, Indicator, Pop_w_Data)  %>% 
  full_join(scales, by="Indicator") %>% full_join(Global_pop, by=c("Global", "Population")) %>%
  mutate(Pop_Share=Pop_w_Data/Pop) %>%
  select(Global, Indicator, Pop_Share) %>%
  filter(!is.na(Pop_Share)) %>%
  spread(Indicator, Pop_Share)  %>%
  rename(Region=Global)  %>%
  mutate(Region= "Global")

Global_results <- Global_data %>% select(-Pop_w_Data) %>%
  full_join(indicators, by="Indicator") %>%
  select(-score) %>%  full_join(scales, by="Indicator") %>%
  mutate(Capped_Value = case_when(Indicator=="DS_1524" & Full_Value>75 ~ 75,
                                  Indicator=="UpperSec_Ratio" & Full_Value < 1 ~ 1,
                                  Indicator=="SE.SEC.ENRR" & Full_Value >100  ~ 100,
                                  Indicator=="Account_Ratio" & Full_Value >1  ~ 1,
                                  TRUE ~ Full_Value)) %>% # capped data
  mutate(Scaled_Value= (Capped_Value - Min)/( Max - Min ))

scale_results_Global <- Global_results %>%
  mutate(Domain=case_when(Domain==  "Economic Empowerment"  ~ "Economics",
                          Domain==   "Education"   ~ "Education",
                          Domain== "Gender and Autonomy" ~ "Gender",
                          Domain==    "Safety and Security" ~ "Safety",
                          Domain==  "SRH Empowerment"      ~ "SRH",
                          Domain== "Youth Policy and Political Participation" ~ "Politics")) %>% 
  mutate(Indicator= paste(Domain, Subdomain, Indicator, sep="_")) %>%
  select(Global, Indicator,  Scaled_Value) %>%
  spread(Indicator, Scaled_Value)

sub_domain_global <- Global_results %>% 
  select(Global, Domain,  Subdomain, SubWeight, DomainWeight, Scaled_Value) %>%
  group_by(Global, Domain, Subdomain) %>%
  summarise(Value= sum(Scaled_Value * SubWeight))  %>%
  ungroup() %>%
  mutate(Domain=case_when(Domain==  "Economic Empowerment"  ~ "Economics",
                          Domain==   "Education"   ~ "Education",
                          Domain== "Gender and Autonomy" ~ "Gender",
                          Domain==    "Safety and Security" ~ "Safety",
                          Domain==  "SRH Empowerment"      ~ "SRH",
                          Domain== "Youth Policy and Political Participation" ~ "Politics")) %>%
  mutate(Group=paste(Domain, Subdomain, sep="_")) %>% select(-Domain, -Subdomain) %>%
  spread(Group, Value)

domain_global <- Global_results %>% 
  select(Global, Domain,  Subdomain, SubWeight, DomainWeight, Scaled_Value) %>%
  group_by(Global, Domain) %>%
  summarise(Value= sum(Scaled_Value * SubWeight * DomainWeight)) %>%
  ungroup() %>%
  mutate(Domain=case_when(Domain==  "Economic Empowerment"  ~ "Economics",
                          Domain==   "Education"   ~ "Education",
                          Domain== "Gender and Autonomy" ~ "Gender",
                          Domain==    "Safety and Security" ~ "Safety",
                          Domain==  "SRH Empowerment"      ~ "SRH",
                          Domain== "Youth Policy and Political Participation" ~ "Politics")) %>%
  spread(Domain, Value) %>%
  mutate(Index = (Economics * Education * Gender * Safety * SRH * Politics)^(1/6)) %>%
  full_join(sub_domain_global, by="Global") %>%
  full_join(scale_results_Global, by="Global") %>%  # joining the variable data
  rename(Region=Global)  %>%
  mutate(Region= "Global")
##############################################################################################################
# Combine together all the domain summary documents

domain_regions_total <- bind_rows(domain_global, domain_UNFPATotal, domain_UNFPAGroup, domain_regional) %>%
  select(Region , Index , Economics , Education , Gender , Politics , Safety , SRH , 
         Economics_Resource , Economics_Resource_gwp1_n_5 , Economics_Resource_gwp2_n_5 , Economics_Resource_SL_CPA_YEMP ,  
         Economics_Agency , Economics_Agency_Account_Ratio ,  
         Economics_Achievement , Economics_Achievement_BetterOff , Economics_Achievement_SL_TLF_NEET_Both , Economics_Achievement_WorkPov ,  
         Education_Resource , Education_Resource_Educ_GDP , Education_Resource_SH_LGR_ACSRHES3 ,   
         Education_Agency , Education_Agency_SE.SEC.ENRR ,  
         Education_Achievement , Education_Achievement_UpperSec_Both ,  
         Gender_Resource , Gender_Resource_UpperSec_Ratio , 
         Gender_Agency , Gender_Agency_Res_Civil ,  
         Gender_Achievement , Gender_Achievement_SP_DYN_MRBF18 , Gender_Achievement_violence_w_1524 , 
         Politics_Resource , Politics_Resource_CollectData , Politics_Resource_YouthPolicy_Num ,
         Politics_Agency , Politics_Agency_active_member , Politics_Agency_Vote_National , 
         Politics_Achievement , Politics_Achievement_Parliament30 ,  
         Safety_Resource , Safety_Resource_INFORM ,  
         Safety_Agency , Safety_Agency_humanrights , Safety_Agency_pol_part ,  
         Safety_Achievement , Safety_Achievement_Displacement , Safety_Achievement_InterViolence , Safety_Achievement_SelfHarm , 
         SRH_Resource , SRH_Resource_ProviderDisc , SRH_Resource_SH_LGR_ACSRHES1 , SRH_Resource_SH_LGR_ACSRHES2 , SRH_Resource_SH_LGR_ACSRHES4 ,  
         SRH_Agency , SRH_Agency_DS_1524 , SRH_Agency_fert_know_m , SRH_Agency_fert_know_w , SRH_Agency_HIVKnow_m_1524 , SRH_Agency_HIVKnow_w_1524 , SRH_Agency_knows_ltm_stm_m , SRH_Agency_knows_ltm_stm_w , 
         SRH_Achievement , SRH_Achievement_ASFR15 , SRH_Achievement_HIV , SRH_Achievement_SH_STA_MORT )


# If you would like to export these results, uncomment the line below
write.csv(domain_regions_total, "YEI Regional Results Weighted 111221.csv", row.names = F, na="")

# Combine information on the population represented for each region
pop_regions_total <- bind_rows(Global_Pop_W_data, UNFPATotal_Pop_W_data, UNFPAGroup_Pop_W_data, regional_Pop_W_data )

# If you would like to export these results, uncomment the line below
#write.csv(pop_regions_total, "YEI Regional PopRepresented 111221.csv", row.names = F, na="")

#################################################################################
#################################################################################
#### This next section of code calculates country results                    ####
####                                                                         ####
#################################################################################
#################################################################################

# Filling in Missing Values

complete_data <- analysis_data %>%
  select(-Country, -ISOAlph, -Subregion, -UNFPAGroup, -UNFPATotal, -Global) %>%
  gather(Indicator, Value, SH_LGR_ACSRHES1:SelfHarm) %>%
  full_join(regional2_data, by=c("SDG", "Indicator")) %>%
  mutate(Full_Value=case_when(!is.na(Value) ~ Value,
                              !is.na(Value1) ~ Value1,
                              !is.na(Value2) ~ Value2)) 


# Top Coding and Scaling Data
scale_data <- complete_data %>%
  full_join(scales, by="Indicator") %>%
  mutate(Capped_Value = case_when(Indicator=="DS_1524" & Full_Value>75 ~ 75,
                                  Indicator=="UpperSec_Ratio" & Full_Value < 1 ~ 1,
                                  Indicator=="SE.SEC.ENRR" & Full_Value >100  ~ 100,
                                  Indicator=="Account_Ratio" & Full_Value >1  ~ 1,
                                  TRUE ~ Full_Value)) %>% # capped data
  mutate(Scaled_Value= (Capped_Value - Min)/( Max - Min )) %>%
  filter(!is.na(ISONum))

# If you would like the number of countries (before missing data was handled) per indicator
indicator_N <- analysis_data %>%
  select(-Country, -ISOAlph, -Subregion, -UNFPAGroup, -UNFPATotal, -Global) %>%
  gather(Indicator, Value, SH_LGR_ACSRHES1:SelfHarm) %>%
  filter(!is.na(Value))  %>%
  mutate(n=1) %>%
  group_by( Indicator) %>%
  summarise(N=sum(n))

# If you would like to export these results, uncomment the line below
#write.csv(indicator_N, "YEI Number of Countries per Indicator 111221.csv", row.names = F, na="")


# The number of indicators per country
# We will use this later to limit the number of countries shown to those with 20 indicators
country_N <- analysis_data %>%
  select(-Country, -ISOAlph, -Subregion, -UNFPAGroup, -UNFPATotal, -Global) %>%
  gather(Indicator, Value, SH_LGR_ACSRHES1:SelfHarm) %>%
  filter(!is.na(Value))  %>%
  mutate(n=1) %>%
  group_by( ISONum) %>%
  summarise(N=sum(n)) %>%
  rename(Indicator_N=N)


# Subdomain Coding
sub_domain_index_data <- scale_data %>% 
  select(ISONum, Domain,  Subdomain, SubWeight, DomainWeight, Scaled_Value) %>%
  group_by(ISONum, Domain, Subdomain) %>%
  summarise(Value= sum(Scaled_Value * SubWeight))  %>%
  ungroup() %>%
  mutate(Domain=case_when(Domain==  "Economic Empowerment"  ~ "Economics",
                          Domain==   "Education"   ~ "Education",
                          Domain== "Gender and Autonomy" ~ "Gender",
                          Domain==    "Safety and Security" ~ "Safety",
                          Domain==  "SRH Empowerment"      ~ "SRH",
                          Domain== "Youth Policy and Political Participation" ~ "Politics")) %>%
  mutate(Group=paste(Domain, Subdomain, sep="_")) %>% select(-Domain, -Subdomain) %>%
  spread(Group, Value) 

# Domain Coding, adding in subdomain and number of indicators per country
domain_index_data <- scale_data %>% 
  select(ISONum, Domain,  Subdomain, SubWeight, DomainWeight, Scaled_Value) %>%
  group_by(ISONum, Domain) %>%
  summarise(Value= sum(Scaled_Value * SubWeight * DomainWeight)) %>%
  ungroup() %>%
  mutate(Domain=case_when(Domain==  "Economic Empowerment"  ~ "Economics",
                          Domain==   "Education"   ~ "Education",
                          Domain== "Gender and Autonomy" ~ "Gender",
                          Domain==    "Safety and Security" ~ "Safety",
                          Domain==  "SRH Empowerment"      ~ "SRH",
                          Domain== "Youth Policy and Political Participation" ~ "Politics")) %>%
  spread(Domain, Value) %>%
  mutate(Index = (Economics * Education * Gender * Safety * SRH * Politics)^(1/6)) %>%
  full_join(sub_domain_index_data, by="ISONum") %>%
  full_join(countryiso, by="ISONum") %>%
  full_join(country_N, by="ISONum") %>%
  mutate(Indicator_N=case_when(is.na(Indicator_N) ~ 0,
                               !is.na(Indicator_N) ~ Indicator_N)) %>%
  select(Country, ISONum, ISOAlph, Subregion, SDG, UNFPAGroup, Index,     
         Economics, Education, Gender, Politics, Safety, SRH,
         Economics_Achievement, Economics_Agency,      
         Economics_Resource, Education_Achievement, Education_Agency, Education_Resource, Gender_Achievement,    
         Gender_Agency, Gender_Resource, Politics_Achievement, Politics_Agency, Politics_Resource,     
         Safety_Achievement, Safety_Agency, Safety_Resource, SRH_Achievement, SRH_Agency,            
         SRH_Resource, Indicator_N)

# If you would like to export these results, uncomment the line below
#write.csv(domain_index_data, "YEI_IndexCountryResults PopWeight 111221.csv", row.names = F, na="")

#################################################################################
#################################################################################
#### This next section of exports indicators and subdomains per country      ####
#### by domain                                                               ####
#################################################################################
#################################################################################

scale_results_education <- scale_data %>% filter(Domain=="Education") %>%
  mutate(Indicator= paste(Domain, Subdomain, Indicator, sep="_")) %>%
  select(ISONum, Indicator,  Scaled_Value) %>%
  spread(Indicator, Scaled_Value) %>% 
  full_join(domain_index_data, by="ISONum") %>%
  select(Country, ISONum, ISOAlph, Subregion, SDG, UNFPAGroup,
         Education_Resource_Educ_GDP , Education_Resource_SH_LGR_ACSRHES3, Education_Resource,
         Education_Agency_SE.SEC.ENRR,  Education_Agency,  Education_Achievement_UpperSec_Both,    
         Education_Achievement,    Education)   

# If you would like to export these results, uncomment the line below
#write.csv(scale_results_education, "YEI Education Results PopWeight 111221.csv", row.names = F, na="")

scale_results_economics <- scale_data %>% filter(Domain=="Economic Empowerment" ) %>%
  mutate(Domain="Economic_Empowerment") %>%
  mutate(Indicator= paste(Domain, Subdomain, Indicator, sep="_")) %>%
  select(ISONum, Indicator,  Scaled_Value) %>%
  spread(Indicator, Scaled_Value) %>% 
  full_join(domain_index_data, by="ISONum") %>%
  select(Country, ISONum, ISOAlph, Subregion, SDG, UNFPAGroup,
         Economic_Empowerment_Resource_gwp1_n_5,           
         Economic_Empowerment_Resource_gwp2_n_5,            Economic_Empowerment_Resource_SL_CPA_YEMP,   
         Economics_Resource,
         Economic_Empowerment_Agency_Account_Ratio, Economics_Agency,  
         Economic_Empowerment_Achievement_BetterOff,      
         Economic_Empowerment_Achievement_SL_TLF_NEET_Both, Economic_Empowerment_Achievement_WorkPov,       
         Economics_Achievement,    Economics)   

# If you would like to export these results, uncomment the line below
#write.csv(scale_results_economics, "YEI Economics Results PopWeight 111221.csv", row.names = F, na="")

scale_results_gender <- scale_data %>% filter(Domain=="Gender and Autonomy"  ) %>%
  mutate(Domain="Gender_and_Autonomy" ) %>%
  mutate(Indicator= paste(Domain, Subdomain, Indicator, sep="_")) %>%
  select(ISONum, Indicator,  Scaled_Value) %>%
  spread(Indicator, Scaled_Value) %>% 
  full_join(domain_index_data, by="ISONum") %>%
  select(Country, ISONum, ISOAlph, Subregion, SDG, UNFPAGroup,
         Gender_and_Autonomy_Resource_UpperSec_Ratio,
         Gender_Resource,
         Gender_and_Autonomy_Agency_Res_Civil,
         Gender_Agency,  
         Gender_and_Autonomy_Achievement_SP_DYN_MRBF18,
         Gender_and_Autonomy_Achievement_violence_w_1524,
         Gender_Achievement,    Gender)   

# If you would like to export these results, uncomment the line below
#write.csv(scale_results_gender, "YEI Gender Results PopWeight 111221.csv", row.names = F, na="")

scale_results_safety <- scale_data %>% filter(Domain=="Safety and Security"  ) %>%
  mutate(Domain="Safety_and_Security" ) %>%
  mutate(Indicator= paste(Domain, Subdomain, Indicator, sep="_")) %>%
  select(ISONum, Indicator,  Scaled_Value) %>%
  spread(Indicator, Scaled_Value) %>% 
  full_join(domain_index_data, by="ISONum") %>%
  select(Country, ISONum, ISOAlph, Subregion, SDG, UNFPAGroup,
         Safety_and_Security_Resource_INFORM ,  
         Safety_Resource,
         Safety_and_Security_Agency_humanrights, 
         Safety_and_Security_Agency_pol_part,  
         Safety_Agency,  
         Safety_and_Security_Achievement_Displacement,
         Safety_and_Security_Achievement_InterViolence, 
         Safety_and_Security_Achievement_SelfHarm,  
         Safety_Achievement,    Safety)   

# If you would like to export these results, uncomment the line below
#write.csv(scale_results_safety, "YEI Safety Results PopWeight 111221.csv", row.names = F, na="")


scale_results_SRH <- scale_data %>% filter(Domain=="SRH Empowerment"  ) %>%
  mutate(Domain="SRH_Empowerment") %>%
  mutate(Indicator= paste(Domain, Subdomain, Indicator, sep="_")) %>%
  select(ISONum, Indicator,  Scaled_Value) %>%
  spread(Indicator, Scaled_Value) %>% 
  full_join(domain_index_data, by="ISONum") %>%
  select(Country, ISONum, ISOAlph, Subregion, SDG, UNFPAGroup,
         SRH_Empowerment_Resource_ProviderDisc,    
         SRH_Empowerment_Resource_SH_LGR_ACSRHES1,    
         SRH_Empowerment_Resource_SH_LGR_ACSRHES2, 
         SRH_Empowerment_Resource_SH_LGR_ACSRHES4,  
         SRH_Resource, 
         SRH_Empowerment_Agency_DS_1524,          
         SRH_Empowerment_Agency_fert_know_m,    
         SRH_Empowerment_Agency_fert_know_w,     
         SRH_Empowerment_Agency_HIVKnow_m_1524,    
         SRH_Empowerment_Agency_HIVKnow_w_1524,      
         SRH_Empowerment_Agency_knows_ltm_stm_m,
         SRH_Empowerment_Agency_knows_ltm_stm_w,
         SRH_Agency, 
         SRH_Empowerment_Achievement_ASFR15,        
         SRH_Empowerment_Achievement_HIV,           
         SRH_Empowerment_Achievement_SH_STA_MORT,       
         SRH_Achievement,
         SRH)       
# If you would like to export these results, uncomment the line below
#write.csv(scale_results_SRH, "YEI SRH Results PopWeight 111221.csv", row.names = F, na="")


scale_results_politics <- scale_data %>% filter(Domain== "Youth Policy and Political Participation"  ) %>%
  mutate(Domain= "Politics") %>%
  mutate(Indicator= paste(Domain, Subdomain, Indicator, sep="_")) %>%
  select(ISONum, Indicator,  Scaled_Value) %>%
  spread(Indicator, Scaled_Value) %>% 
  full_join(domain_index_data, by="ISONum") %>%
  select(Country, ISONum, ISOAlph, Subregion, SDG, UNFPAGroup,
         Politics_Resource_CollectData,     
         Politics_Resource_YouthPolicy_Num,
         Politics_Resource, 
         Politics_Agency_active_member,    
         Politics_Agency_Vote_National,  
         Politics_Agency,  
         Politics_Achievement_Parliament30,
         Politics_Achievement,              
         Politics)       

# If you would like to export these results, uncomment the line below
#write.csv(scale_results_politics, "YEI Politics Results PopWeight 111221.csv", row.names = F, na="")


