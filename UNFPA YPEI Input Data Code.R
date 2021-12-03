# Creating Master File of Input Data for UNFPA Young People Empowerment Index
# November 2021
# Code written by Kristin Bietsch, PhD; Avenir Health; kbietsch@avenirhealth.org

# Note, when using APIs, if the APIs are updated, the data will be updated


# The packages required for this code:
library(dplyr)
library(tidyr)
library(stringr)
require(RJSONIO)
library(xlsx)
library(wpp2019)
library(jsonlite) 
library(data.table)
library(wbstats)
library(readxl)

# Removing scientific notation and increasing memory size
options(scipen=999)
memory.limit(size = 2e6) 

# Set the working directory where input files are stored and where you want results to be saved
setwd("C:/Users/KristinBietsch/files/UNFPA/Code for GitHub")

##################################################################################
# Structure of the dataset
# We are going to create small datasets for each indicator with 3 columns: an ISO number for each country, the value, and the year the value refers to
# At the end we will merge all datasets together into a dataframe
# Included in the input data folder is the file used to create the original YPEI, "YPEI Analysis Data 111021.csv". 
# This code will use the current data from APIs when available, and other datasets when not.
##################################################################################

# Country Names, ISO, and Regions
countryiso <- read.csv("Input data/Country Regions.csv")


##################################################################################

# Data from SDG API
# https://unstats.un.org/sdgs/indicators/database/

# SDG 5.6.2 S.1 Extent to which countries have laws and regulations that guarantee full and equal access to women and men aged 15 years and older to sexual and reproductive health care information and education: Section 1: Maternity Care (percent)
# SH_LGR_ACSRHES1
url <- c("https://unstats.un.org/SDGAPI/v1/sdg/Series/Data?seriesCode=SH_LGR_ACSRHES1&pageSize=500")
jsondata<-fromJSON(url) 
dta<-data.table(jsondata$data)

SH_LGR_ACSRHES1 <-select(dta, geoAreaCode, value , timePeriodStart) %>%
  rename(ISONum=geoAreaCode, SH_LGR_ACSRHES1=value, SH_LGR_ACSRHES1_Year=timePeriodStart) %>%
  mutate(ISONum=as.numeric(as.character(ISONum))) %>%
  full_join(countryiso, by="ISONum") %>% filter(!is.na(Country)) %>% select(ISONum, SH_LGR_ACSRHES1, SH_LGR_ACSRHES1_Year)

# SDG 5.6.2 S.2 Extent to which countries have laws and regulations that guarantee full and equal access to women and men aged 15 years and older to sexual and reproductive health care information and education: Section 2: Contraceptive and Family Planning (percent)
# SH_LGR_ACSRHES2
url <- c("https://unstats.un.org/SDGAPI/v1/sdg/Series/Data?seriesCode=SH_LGR_ACSRHES2&pageSize=500")
jsondata<-fromJSON(url) 
dta<-data.table(jsondata$data)

SH_LGR_ACSRHES2 <-select(dta, geoAreaCode, value , timePeriodStart) %>%
  rename(ISONum=geoAreaCode, SH_LGR_ACSRHES2=value, SH_LGR_ACSRHES2_Year=timePeriodStart) %>%
  mutate(ISONum=as.numeric(as.character(ISONum))) %>%
  full_join(countryiso, by="ISONum") %>% filter(!is.na(Country)) %>% select(ISONum, SH_LGR_ACSRHES2, SH_LGR_ACSRHES2_Year)

# SDG 5.6.2 S.3 Extent to which countries have laws and regulations that guarantee full and equal access to women and men aged 15 years and older to sexual and reproductive health care information and education: Section 3: Sexuality Education (percent)
# SH_LGR_ACSRHES3
url <- c("https://unstats.un.org/SDGAPI/v1/sdg/Series/Data?seriesCode=SH_LGR_ACSRHES3&pageSize=500")
jsondata<-fromJSON(url) 
dta<-data.table(jsondata$data)

SH_LGR_ACSRHES3 <-select(dta, geoAreaCode, value , timePeriodStart) %>%
  rename(ISONum=geoAreaCode, SH_LGR_ACSRHES3=value, SH_LGR_ACSRHES3_Year=timePeriodStart) %>%
  mutate(ISONum=as.numeric(as.character(ISONum))) %>%
  full_join(countryiso, by="ISONum") %>% filter(!is.na(Country)) %>% select(ISONum, SH_LGR_ACSRHES3, SH_LGR_ACSRHES3_Year)

# SDG 5.6.2 S.4 Extent to which countries have laws and regulations that guarantee full and equal access to women and men aged 15 years and older to sexual and reproductive health care information and education: Section 4: HIV and HPV (percent)
# SH_LGR_ACSRHES4
url <- c("https://unstats.un.org/SDGAPI/v1/sdg/Series/Data?seriesCode=SH_LGR_ACSRHES4&pageSize=500")
jsondata<-fromJSON(url) 
dta<-data.table(jsondata$data)

SH_LGR_ACSRHES4 <-select(dta, geoAreaCode, value , timePeriodStart) %>%
  rename(ISONum=geoAreaCode, SH_LGR_ACSRHES4=value, SH_LGR_ACSRHES4_Year=timePeriodStart) %>%
  mutate(ISONum=as.numeric(as.character(ISONum))) %>%
  full_join(countryiso, by="ISONum") %>% filter(!is.na(Country)) %>% select(ISONum, SH_LGR_ACSRHES4, SH_LGR_ACSRHES4_Year)

# SDG 3.1.1 Maternal mortality ratio
# SH_STA_MORT
url <- c("https://unstats.un.org/SDGAPI/v1/sdg/Series/Data?seriesCode=SH_STA_MORT&pageSize=5000")
jsondata<-fromJSON(url) 
dta<-data.table(jsondata$data)

SH_STA_MORT <-select(dta, geoAreaCode, value , timePeriodStart) %>% 
  group_by(geoAreaCode) %>% 
  mutate(Recent=max(timePeriodStart)) %>% filter(timePeriodStart==Recent) %>% select(-Recent) %>%
  mutate(value=as.numeric(as.character(value))) %>% 
  rename(ISONum=geoAreaCode, SH_STA_MORT=value, SH_STA_MORT_Year=timePeriodStart) %>%
  mutate(ISONum=as.numeric(as.character(ISONum))) 


#Indicator 5.3.1: Proportion of women aged 20-24 years who were married or in a union before age 18 (%) 
#SP_DYN_MRBF18
url <- c("https://unstats.un.org/SDGAPI/v1/sdg/Series/Data?seriesCode=SP_DYN_MRBF18&pageSize=500")
jsondata<-fromJSON(url) 
dta<-data.table(jsondata$data)

SP_DYN_MRBF18 <-select(dta, geoAreaCode, geoAreaName, timePeriodStart, value)   %>%
  group_by(geoAreaCode, geoAreaName) %>%
  mutate(recent=max(timePeriodStart)) %>% ungroup() %>% 
  filter(timePeriodStart==recent) %>% select(-recent) %>%
  rename(ISONum=geoAreaCode, SP_DYN_MRBF18=value, SP_DYN_MRBF18_Year=timePeriodStart) %>%
  select(ISONum, SP_DYN_MRBF18, SP_DYN_MRBF18_Year) %>%
  mutate(ISONum=as.numeric(as.character(ISONum)))

class(SP_DYN_MRBF18$ISONum)

#Indicator 8.b.1: Existence of a developed and operationalized national strategy for youth employment, as a distinct strategy or as part of a national employment strategy 
#SL_CPA_YEMP
url <- c("https://unstats.un.org/SDGAPI/v1/sdg/Series/Data?seriesCode=SL_CPA_YEMP&pageSize=500")
jsondata<-fromJSON(url) 
dta<-data.table(jsondata$data)

# Legend
# 0 - The country has not developed any national strategy for youth employment or taken steps to develop or adopt one.; 
# 1 - The country is in the process of developing a national strategy for youth employment.
# 2 - The country has developed and adopted a national strategy for youth employment
# 3 - The country has operationalized a national strategy for youth employment.
SL_CPA_YEMP <-select(dta, geoAreaCode, geoAreaName, timePeriodStart, value)   %>%
  group_by(geoAreaCode, geoAreaName) %>%
  mutate(recent=max(timePeriodStart)) %>% ungroup() %>% 
  filter(timePeriodStart==recent) %>% select(-recent) %>%
  mutate(Value_Label=case_when(value==0 ~ "The country has not developed any national strategy for youth employment or taken steps to develop or adopt one",
                               value==1 ~ "The country is in the process of developing a national strategy for youth employment" , 
                               value==2 ~ "The country has developed and adopted a national strategy for youth employment" , 
                               value==3 ~ "The country has operationalized a national strategy for youth employment" )) %>%
  rename(ISONum=geoAreaCode, SL_CPA_YEMP_Year=timePeriodStart, SL_CPA_YEMP=value, SL_CPA_YEMP_Label=Value_Label) %>%
  select(ISONum, SL_CPA_YEMP, SL_CPA_YEMP_Label, SL_CPA_YEMP_Year)  %>%
  mutate(ISONum=as.numeric(as.character(ISONum)))


#Indicator 8.6.1 Proportion of youth () 15-24 not in education, employment or training, by sex and age (%) 
#SL_TLF_NEET
url <- c("https://unstats.un.org/SDGAPI/v1/sdg/Series/Data?seriesCode=SL_TLF_NEET&pageSize=20000")
jsondata<-fromJSON(url) 
dta<-data.table(jsondata$data)

SL_TLF_NEET <- dta %>% select(geoAreaCode, geoAreaName, timePeriodStart, value, dimensions.Sex) %>%
  spread(dimensions.Sex, value) %>%
  group_by(geoAreaCode, geoAreaName) %>%
  mutate(recent=max(timePeriodStart)) %>% ungroup() %>% 
  filter(timePeriodStart==recent) %>%
  rename(ISONum=geoAreaCode, SL_TLF_NEET_Year=timePeriodStart, SL_TLF_NEET_Both=BOTHSEX) %>%
  select(ISONum, SL_TLF_NEET_Both, SL_TLF_NEET_Year)  %>%
  mutate(ISONum=as.numeric(as.character(ISONum)))
##################################################################################

# Data from DHS API

# Demand Satisfied 
# 	FP_NADA_W_PDM	Demand for family planning satisfied by modern methods (all women)	Percentage of demand for family planning satisfied by modern methods is calculated as the number of all women using modern methods of family planning divided by the number of all women with demand for family planning (either with unmet need or currently using any family planning)
url<-("http://api.dhsprogram.com/rest/dhs/data?f=json&indicatorIds=FP_NADA_W_PDM&surveyid=all&breakdown=ALL&perpage=20000&APIkey=AVEHTH-279664")
jsondata<-fromJSON(url) 
dta<-data.table(jsondata$Data)
dta<-select(dta, CountryName, SurveyId, Value, 
            CharacteristicCategory, CharacteristicLabel, SurveyYear, DHS_CountryCode)   

demand_satisfied<- dta %>% 
  filter(CharacteristicCategory== "Age (grouped)") %>% 
  filter( CharacteristicLabel=="15-24" ) %>%
  group_by(DHS_CountryCode)  %>%
  mutate(recent=max(SurveyYear)) %>% ungroup() %>% 
  filter(SurveyYear==recent) %>%
  rename(DS_1524=Value, DS_Year=SurveyYear) %>%
  left_join(countryiso, by="DHS_CountryCode") %>% 
  select(ISONum, DS_1524,  DS_Year)

# HIV Knowledge, Women
#HA_CKNA_W_CKA	Comprehensive correct knowledge about AIDS [Women]	Percentage of women who correctly identify the two major ways of preventing the sexual transmission of HIV (using condoms and limiting sex to one faithful, uninfected partner), who reject the two most common local misconceptions about HIV transmission, and who know that a healthy-looking person can have HIV.
url<-("http://api.dhsprogram.com/rest/dhs/data?f=json&indicatorIds=HA_CKNA_W_CKA&surveyid=all&breakdown=ALL&perpage=20000&APIkey=AVEHTH-279664")
jsondata<-fromJSON(url) 
dta<-data.table(jsondata$Data)
dta<-select(dta, SurveyId, Value, 
            CharacteristicCategory, CharacteristicLabel, SurveyYear, DHS_CountryCode)  

HIV_w_Knowledge <- dta %>%
  filter(CharacteristicLabel== "15-24")  %>%
  group_by(DHS_CountryCode)  %>%
  mutate(recent=max(SurveyYear)) %>% ungroup() %>% 
  filter(SurveyYear==recent) %>%
  rename(HIVKnow_w_1524=Value, HIVKnow_w_Year=SurveyYear) %>%
  left_join(countryiso, by="DHS_CountryCode") %>% 
  select(ISONum, HIVKnow_w_1524,  HIVKnow_w_Year)


# HIV Knowledge, Men
#HA_CKNA_M_CKA	Comprehensive correct knowledge about AIDS [Men]	Percentage of men who correctly identify the two major ways of preventing the sexual transmission of HIV (using condoms and limiting sex to one faithful, uninfected partner), who reject the two most common local misconceptions about HIV transmission, and who know that a healthy-looking person can have HIV.
url<-("http://api.dhsprogram.com/rest/dhs/data?f=json&indicatorIds=HA_CKNA_M_CKA&surveyid=all&breakdown=ALL&perpage=20000&APIkey=AVEHTH-279664")
jsondata<-fromJSON(url) 
dta<-data.table(jsondata$Data)
dta<-select(dta, SurveyId, Value, 
            CharacteristicCategory, CharacteristicLabel,  SurveyYear, DHS_CountryCode)   

HIV_m_Knowledge <- dta %>% 
  filter(CharacteristicLabel== "15-24")  %>%
  group_by(DHS_CountryCode)  %>%
  mutate(recent=max(SurveyYear)) %>% ungroup() %>% 
  filter(SurveyYear==recent) %>%
  rename(HIVKnow_m_1524=Value, HIVKnow_m_Year=SurveyYear) %>%
  left_join(countryiso, by="DHS_CountryCode") %>% 
  select(ISONum, HIVKnow_m_1524,  HIVKnow_m_Year)

  
# Women who have experienced violence
# DV_FMVL_W_POS	Women who experienced physical or sexual violence	Percentage of women who experienced physical or sexual violence
url<-("http://api.dhsprogram.com/rest/dhs/data?f=json&indicatorIds=DV_FMVL_W_POS&surveyid=all&breakdown=ALL&perpage=20000&APIkey=AVEHTH-279664")
jsondata<-fromJSON(url) 
dta<-data.table(jsondata$Data)
dta<-select(dta, SurveyId, Value, 
            CharacteristicCategory, CharacteristicLabel,  SurveyYear, DHS_CountryCode)

violence <- dta  %>% 
  filter(CharacteristicLabel== "15-24")  %>%
  group_by(DHS_CountryCode)  %>%
  mutate(recent=max(SurveyYear)) %>% ungroup() %>% 
  filter(SurveyYear==recent)  %>%
  rename(violence_w_1524=Value, Violence_Year=SurveyYear) %>%
  left_join(countryiso, by="DHS_CountryCode") %>% 
  select(ISONum, violence_w_1524,  Violence_Year)
  
##################################################################################
# World Population Prospect API
# Age specific fertility rate, 15-19

# Dataframes which come as part of the wpp2019 package
as.data.frame(data(percentASFR))
as.data.frame(data(tfr))

# Age contributions to TFR
per_ASFR <- percentASFR %>% select("country_code",  "age",  "2015-2020" ) %>% rename( dis1520 = "2015-2020" )

asfr <- tfr %>% select("country_code",   "2015-2020" ) %>% 
  rename( tfr1520 = "2015-2020" ) %>% 
  full_join(per_ASFR, by="country_code") %>%
  mutate(asfr= (((dis1520/100)*tfr1520)/5)*1000   ) %>%
  filter(age=="15-19") %>%
  rename(ISONum=country_code, ASFR15=asfr) %>%
  select(ISONum, ASFR15) %>%
  mutate(ASFR15_Year="2015-2020")

##################################################################################

# World Bank API Data

# gwp1_n_5	Physical Points of Service	Access to a mobile phone (% ages 15-34)
gwp1_n_5_full <- wb_data(
  indicator = "gwp1_n_5",
  start_date = 2010, end_date = 2020) 

gwp1_n_5 <- gwp1_n_5_full %>%
  filter(!is.na(gwp1_n_5)) %>% 
  group_by(iso3c) %>%
  mutate(recent=max(date)) %>% ungroup() %>% 
  filter(date==recent) %>% select(iso3c, date, gwp1_n_5) %>%
  rename(ISOAlph=iso3c) %>% 
  full_join(countryiso, by="ISOAlph") %>%
  select(ISONum, gwp1_n_5, date) %>% filter(!is.na(gwp1_n_5)) %>%
  rename(gwp1_n_5_Year=date)

# gwp2_n_5	Physical Points of Service	Access to internet (% ages 15-34)
gwp2_n_5_full <- wb_data(
  indicator = "gwp2_n_5",
  start_date = 2010, end_date = 2020) 

gwp2_n_5 <- gwp2_n_5_full %>%
  filter(!is.na(gwp2_n_5)) %>% 
  group_by(iso3c) %>%
  mutate(recent=max(date)) %>% ungroup() %>% 
  filter(date==recent) %>% select(iso3c, date, gwp2_n_5) %>%
  rename(ISOAlph=iso3c) %>% 
  full_join(countryiso, by="ISOAlph") %>%
  select(ISONum, gwp2_n_5, date) %>% filter(!is.na(gwp2_n_5)) %>%
  rename(gwp2_n_5_Year=date)

# account_t_d_5	Usage of Financial Services	Account (% ages 15-34)	Denotes the percentage of respondents, ages 15-34, who report having an account (by themselves or together with someone else) at a bank or another type of financial institution or personally using a mobile money service in the past 12 months.
account_t_d_5_full <- wb_data(
  indicator = "account_t_d_5",
  start_date = 2010, end_date = 2020) 

account_t_d_5 <- account_t_d_5_full %>%
  filter(!is.na(account_t_d_5)) %>% 
  group_by(iso3c) %>%
  mutate(recent=max(date)) %>% ungroup() %>% 
  filter(date==recent) %>% select(iso3c, date, account_t_d_5) %>%
  rename(ISOAlph=iso3c) %>% 
  full_join(countryiso, by="ISOAlph") %>%
  select(ISONum, account_t_d_5, date) %>% filter(!is.na(account_t_d_5)) %>%
  rename(account_t_d_5_Year=date)

# account_t_d_6	Usage of Financial Services	Account (% ages 35-59)	Denotes the percentage of respondents, ages 35-59, who report having an account (by themselves or together with someone else) at a bank or another type of financial institution or personally using a mobile money service in the past 12 months.
account_t_d_6_full <- wb_data(
  indicator = "account_t_d_6",
  start_date = 2010, end_date = 2020) 

account_t_d_6 <- account_t_d_6_full %>%
  filter(!is.na(account_t_d_6)) %>% 
  group_by(iso3c) %>%
  mutate(recent=max(date)) %>% ungroup() %>% 
  filter(date==recent) %>% select(iso3c, date, account_t_d_6) %>%
  rename(ISOAlph=iso3c) %>% 
  full_join(countryiso, by="ISOAlph") %>%
  select(ISONum, account_t_d_6, date) %>% filter(!is.na(account_t_d_6)) %>%
  rename(account_t_d_6_Year=date)

#SE.SEC.ENRR
#School enrollment, secondary (% gross)
SE.SEC.ENRR_full <- wb_data(
  indicator = "SE.SEC.ENRR",
  start_date = 2010, end_date = 2020) 

SE.SEC.ENRR <- SE.SEC.ENRR_full %>%
  filter(!is.na(SE.SEC.ENRR)) %>% 
  group_by(iso3c) %>%
  mutate(recent=max(date)) %>% ungroup() %>% 
  filter(date==recent) %>% select(iso3c, date, SE.SEC.ENRR) %>%
  rename(ISOAlph=iso3c) %>% 
  full_join(countryiso, by="ISOAlph") %>%
  select(ISONum, SE.SEC.ENRR, date) %>% filter(!is.na(SE.SEC.ENRR)) %>%
  rename(SE.SEC.ENRR_Year=date)

#SE.XPD.TOTL.GD.ZS
#Government expenditure on education, total (% of GDP) is calculated by dividing total government expenditure for all levels of education by the GDP, and multiplying by 100. Aggregate data are based on World Bank estimates.
#Data on education are collected by the UNESCO Institute for Statistics from official responses to its annual education survey. All the data are mapped to the International Standard Classification of Education (ISCED) to ensure the comparability of education programs at the international level. The current version was formally adopted by UNESCO Member States in 2011. GDP data come from the World Bank. 
#The reference years reflect the school year for which the data are presented. In some countries the school year spans two calendar years (for example, from September 2010 to June 2011); in these cases the reference year refers to the year in which the school year ended (2011 in the example)."

SE.XPD.TOTL.GD.ZS_full <- wb_data(
  indicator = "SE.XPD.TOTL.GD.ZS",
  start_date = 2010, end_date = 2020) 

Educ_GDP <- SE.XPD.TOTL.GD.ZS_full %>%
  filter(!is.na(SE.XPD.TOTL.GD.ZS)) %>% 
  group_by(iso3c) %>%
  mutate(recent=max(date)) %>% ungroup() %>% 
  filter(date==recent) %>% select(iso3c, date, SE.XPD.TOTL.GD.ZS) %>%
  rename(ISOAlph=iso3c) %>% 
  full_join(countryiso, by="ISOAlph") %>%
  select(ISONum, SE.XPD.TOTL.GD.ZS, date) %>% filter(!is.na(SE.XPD.TOTL.GD.ZS)) %>%
  rename(Educ_GDP_Year=date, Educ_GDP=SE.XPD.TOTL.GD.ZS)

######################################################################################
# 4 variables for the dataset are constructed from DHS microdata
# Code to construct these variables (which requires requesting and downloading data from DHS at: https://www.dhsprogram.com/Data/)
# is in the file "UNFPA YPEU DHS Microdata Analysis.r"
# Load the results below:
  # Knows fertile cycle, women 15-24: fert_know_w.csv
  # Knows fertile cycle, men 15-24: fert_know_m.csv
  # Knows long term and short term methods of contraception, women 15-24: know_ltm_stm_w.csv
  # Knows long term and short term methods of contraception, men 15-24: know_ltm_stm_m.csv

fert_know_w <- read.csv("Input data/fert_know_w.csv")
fert_know_m <- read.csv("Input data/fert_know_m.csv")
know_ltm_stm_w <- read.csv("Input data/know_ltm_stm_w.csv")
know_ltm_stm_m <- read.csv("Input data/know_ltm_stm_m.csv")

####################################################################################

# 5 variables for the dataset are constructed from World Value Survey microdata
# Code to construct these variables (which requires downloading data from VWS at: worldvaluessurvey.org)
# is in the file "UNFPA YPEI VWS Microdata Analysis.r"
# Load the results below:
# active_member.csv
# BetterOff.csv
# humanrights.csv
# pol_part.csv
# Vote_National.csv

active_member <- read.csv("Input data/active_member.csv")
BetterOff <- read.csv("Input data/BetterOff.csv")
humanrights <- read.csv("Input data/humanrights.csv")
pol_part <- read.csv("Input data/pol_part.csv")
Vote_National <- read.csv("Input data/Vote_National.csv")

####################################################################################
# National Composite Index on Family Planning (NCIFP)
# For Provider discrimination against youth and collect data on youth
# Data is available at http://www.track20.org/pages/data_analysis/policy/NCIFP.php
# A simplified version, with ISONum, provider discrimination against youth score, and year of data is available in as: providerdisc_NCIFP.csv

ncifp <- read_excel("Input Data/2017_NCIFP_report_results_final.xlsx", sheet = "2017 Individual scores unweight")

ncifp_mat <- as.matrix(ncifp)
ncifp_mat_clean <- ncifp_mat[12:93, c(2,22,65)]
ncifp_clean <- as.data.frame(ncifp_mat_clean) %>% rename(Country=1, CollectData=2,  ProviderDisc=3) %>%
  mutate(ISONum=case_when(Country=="Afghanistan" ~ 4,
                          Country=="Armenia" ~ 51,
                          Country=="Bangladesh" ~ 50,
                          Country=="Bhutan" ~ 64,
                          Country=="Bolivia" ~ 68,
                          Country=="Burkina Faso" ~ 854,
                          Country=="Burundi" ~ 108,
                          Country=="Cambodia" ~ 116,
                          Country=="Cameroon" ~ 120,
                          Country=="CAR" ~ 140,
                          Country=="Chad" ~ 148,
                          Country=="Colombia" ~ 170,
                          Country=="Congo (Brazzaville)" ~ 178,
                          Country=="Cote d'Ivoire" ~ 384,
                          Country=="Dominican Republic" ~ 214,
                          Country=="DR Congo" ~ 180,
                          Country=="Egypt" ~ 818,
                          Country=="El Salvador" ~ 222,
                          Country=="Eritrea" ~ 232,
                          Country=="Eswatini" ~ 748,
                          Country=="Ethiopia" ~ 231,
                          Country=="Gambia" ~ 270,
                          Country=="Georgia" ~ 268,
                          Country=="Ghana" ~ 288,
                          Country=="Guatemala" ~ 320,
                          Country=="Guinea" ~ 324,
                          Country=="Guinea-Bissau" ~ 624,
                          Country=="Haiti" ~ 332,
                          Country=="Honduras" ~ 340,
                          Country=="India" ~ 356,
                          Country=="Iraq" ~ 368,
                          Country=="Jamaica" ~ 388,
                          Country=="Jordan" ~ 400,
                          Country=="Kazakhstan" ~ 398,
                          Country=="Kenya" ~ 404,
                          Country=="Kyrgyzstan" ~ 417,
                          Country=="Lao PDR" ~ 418,
                          Country=="Lesotho" ~ 426,
                          Country=="Liberia" ~ 430,
                          Country=="Madagascar" ~ 450,
                          Country=="Malawi" ~ 454,
                          Country=="Malaysia" ~ 458,
                          Country=="Mali" ~ 466,
                          Country=="Mauritania" ~ 478,
                          Country=="Mexico" ~ 484,
                          Country=="Moldova" ~ 498,
                          Country=="Mongolia" ~ 496,
                          Country=="Morocco" ~ 504,
                          Country=="Mozambique" ~ 508,
                          Country=="Myanmar" ~ 104,
                          Country=="Namibia" ~ 516,
                          Country=="Nepal" ~ 524,
                          Country=="Nicaragua" ~ 558,
                          Country=="Niger" ~ 562,
                          Country=="Nigeria" ~ 566,
                          Country=="Pakistan" ~ 586,
                          Country=="Palestine" ~ 275,
                          Country=="Panama" ~ 591,
                          Country=="Papua New Guinea" ~ 598,
                          Country=="Peru" ~ 604,
                          Country=="Philippines" ~ 608,
                          Country=="Romania" ~ 642,
                          Country=="Russia" ~ 643,
                          Country=="Rwanda" ~ 646,
                          Country=="Sao Tome & Principe" ~ 678,
                          Country=="Senegal" ~ 686,
                          Country=="Sierra Leone" ~ 694,
                          Country=="Solomon Islands" ~ 90,
                          Country=="Somalia" ~ 706,
                          Country=="South Sudan" ~ 728,
                          Country=="Sri Lanka" ~ 144,
                          Country=="Tajikistan" ~ 762,
                          Country=="Tanzania" ~ 834,
                          Country=="Timor-Leste" ~ 626,
                          Country=="Togo" ~ 768,
                          Country=="Turkmenistan" ~ 795,
                          Country=="Uganda" ~ 800,
                          Country=="Ukraine" ~ 804,
                          Country=="Uzbekistan" ~ 860,
                          Country=="Viet Nam" ~ 704,
                          Country=="Zambia" ~ 894,
                          Country=="Zimbabwe" ~ 716)) %>%
  select(-Country)


############################################################################
# HIV incidence for young people 15-24 is available from UNAIDS (https://aidsinfo.unaids.org/)
# We have downloaded a csv of data and stored it in input data
hiv <- read.csv("Input Data/New HIV infections_HIV incidence per 1000 population - Young people (15-24)_Population_ All young people (15-24).csv")

# We want to clean data, select median estimates, handle missing values, assign ISONum to countries, and select the most recent piece of data from each country
hiv_clean <- hiv %>% select(Country, X1990, X1991, X1992, X1993, X1994, X1995, X1996, X1997, 
                            X1998, X1999, X2000, X2001, X2002, X2003, X2004, X2005, X2006, 
                            X2007, X2008, X2009, X2010, X2011, X2012, X2013, X2014, X2015, X2016, X2017, X2018, X2019) %>%
  filter(X1990 !=  "... " ) %>%
  filter(X2019 !=  "... " ) %>%
  gather(Year, Value, X1990:X2019) %>%
  mutate(Value=as.character(Value)) %>%
  mutate(Value_num=case_when(Value=="<0.01 " ~ "0",
                             Value!="<0.01 " ~ Value),
         HIV=as.numeric(Value_num)) %>%
  mutate(Year=as.numeric(str_sub(Year, 2, -1))) %>%
  mutate(ISONum=case_when(Country== "Afghanistan" ~ 4,
                          Country== "Albania" ~ 8,
                          Country== "Algeria" ~ 12,
                          Country== "Angola" ~ 24,
                          Country== "Argentina" ~ 32,
                          Country== "Armenia" ~ 51,
                          Country== "Australia" ~ 36,
                          Country== "Azerbaijan" ~ 31,
                          Country== "Barbados" ~ 52,
                          Country== "Belarus" ~ 112,
                          Country== "Benin" ~ 204,
                          Country== "Bolivia (Plurinational State of)" ~ 68,
                          Country== "Botswana" ~ 72,
                          Country== "Bulgaria" ~ 100,
                          Country== "Burkina Faso" ~ 854,
                          Country== "Burundi" ~ 108,
                          Country== "Côte d'Ivoire" ~ 384,
                          Country== "Cabo Verde" ~ 132,
                          Country== "Cambodia" ~ 116,
                          Country== "Cameroon" ~ 120,
                          Country== "Central African Republic" ~ 140,
                          Country== "Chad" ~ 148,
                          Country== "Chile" ~ 152,
                          Country== "Colombia" ~ 170,
                          Country== "Comoros" ~ 174,
                          Country== "Congo" ~ 178,
                          Country== "Costa Rica" ~ 188,
                          Country== "Croatia" ~ 191,
                          Country== "Cuba" ~ 192,
                          Country== "Democratic Republic of the Congo" ~ 180,
                          Country== "Djibouti" ~ 262,
                          Country== "Dominican Republic" ~ 214,
                          Country== "Ecuador" ~ 218,
                          Country== "Egypt" ~ 818,
                          Country== "El Salvador" ~ 222,
                          Country== "Equatorial Guinea" ~ 226,
                          Country== "Eritrea" ~ 232,
                          Country== "Eswatini" ~ 748,
                          Country== "Ethiopia" ~ 231,
                          Country== "Fiji" ~ 242,
                          Country== "Gabon" ~ 266,
                          Country== "Gambia" ~ 270,
                          Country== "Ghana" ~ 288,
                          Country== "Guatemala" ~ 320,
                          Country== "Guinea" ~ 324,
                          Country== "Guinea-Bissau" ~ 624,
                          Country== "Guyana" ~ 328,
                          Country== "Haiti" ~ 332,
                          Country== "Honduras" ~ 340,
                          Country== "Iran (Islamic Republic of)" ~ 364,
                          Country== "Italy" ~ 380,
                          Country== "Jamaica" ~ 388,
                          Country== "Kazakhstan" ~ 398,
                          Country== "Kenya" ~ 404,
                          Country== "Kyrgyzstan" ~ 417,
                          Country== "Lao People's Democratic Republic" ~ 418,
                          Country== "Latvia" ~ 428,
                          Country== "Lebanon" ~ 422,
                          Country== "Lesotho" ~ 426,
                          Country== "Liberia" ~ 430,
                          Country== "Libya" ~ 434,
                          Country== "Lithuania" ~ 440,
                          Country== "Madagascar" ~ 450,
                          Country== "Malawi" ~ 454,
                          Country== "Malaysia" ~ 458,
                          Country== "Mauritius" ~ 480,
                          Country== "Mongolia" ~ 496,
                          Country== "Montenegro" ~ 499,
                          Country== "Morocco" ~ 504,
                          Country== "Mozambique" ~ 508,
                          Country== "Myanmar" ~ 104,
                          Country== "Namibia" ~ 516,
                          Country== "Nepal" ~ 524,
                          Country== "Netherlands" ~ 528,
                          Country== "New Zealand" ~ 554,
                          Country== "Nicaragua" ~ 558,
                          Country== "Niger" ~ 562,
                          Country== "Nigeria" ~ 566,
                          Country== "Oman" ~ 512,
                          Country== "Pakistan" ~ 586,
                          Country== "Papua New Guinea" ~ 598,
                          Country== "Paraguay" ~ 600,
                          Country== "Peru" ~ 604,
                          Country== "Philippines" ~ 608,
                          Country== "Republic of Moldova" ~ 498,
                          Country== "Romania" ~ 642,
                          Country== "Rwanda" ~ 646,
                          Country== "Senegal" ~ 686,
                          Country== "Serbia" ~ 688,
                          Country== "Sierra Leone" ~ 694,
                          Country== "Singapore" ~ 702,
                          Country== "Somalia" ~ 706,
                          Country== "South Africa" ~ 710,
                          Country== "South Sudan" ~ 728,
                          Country== "Spain" ~ 724,
                          Country== "Sri Lanka" ~ 144,
                          Country== "Sudan" ~ 729,
                          Country== "Suriname" ~ 740,
                          Country== "Syrian Arab Republic" ~ 760,
                          Country== "Tajikistan" ~ 762,
                          Country== "Thailand" ~ 764,
                          Country== "Timor-Leste" ~ 626,
                          Country== "Togo" ~ 768,
                          Country== "Trinidad and Tobago" ~ 780,
                          Country== "Tunisia" ~ 788,
                          Country== "Uganda" ~ 800,
                          Country== "Ukraine" ~ 804,
                          Country== "United Republic of Tanzania" ~ 834,
                          Country== "Uzbekistan" ~ 860,
                          Country== "Venezuela (Bolivarian Republic of)" ~ 862,
                          Country== "Viet Nam" ~ 704,
                          Country== "Yemen" ~ 887,
                          Country== "Zambia" ~ 894,
                          Country== "Zimbabwe" ~ 716)) %>%
  rename(HIV_Year=Year) %>%
  select(ISONum, HIV, HIV_Year) %>%
  filter(!is.na(ISONum)) %>% 
  group_by(ISONum) %>%
  mutate(recent=max(HIV_Year)) %>%
  filter(recent==HIV_Year) %>%
  select(-recent)

###################################################################################
# Secondary completion rate is include in the SDG database, but the number of data points is so large that many machines cannot load them all
# An excel version of the file was downloaded from the SDG website and is available in the input data folder. 

SE_TOT_CPLR_full <- read_excel("Input Data/SE_TOT_CPLR.xlsx", sheet = "Data")

SE_TOT_CPLR <- SE_TOT_CPLR_full %>% rename( level="Education level") %>% filter(level=="UPPSEC") %>% filter(Quantile=="_T") %>% filter(Location=="ALLAREA") %>%
  select(GeoAreaCode, GeoAreaName, Sex, "1996": "2018" ) %>%
  gather(Year, Value,  "1996": "2018") %>%
  filter(!is.na(Value)) %>%
  spread(Sex, Value) %>%
  group_by(GeoAreaCode, GeoAreaName,) %>%
  mutate(recent=max(Year)) %>% ungroup() %>% 
  filter(Year==recent) %>%
  rename(UpperSec_Both= BOTHSEX, UpperSec_Female= FEMALE, UpperSec_Male= MALE, ISONum=GeoAreaCode, UpperSec_Year=Year ) %>%
  select(ISONum, UpperSec_Both, UpperSec_Female, UpperSec_Male, UpperSec_Year) 

###################################################################################
# Restricted Civil Liberties for Women
# The Social Institutions & Gender Index contains Restricted civil liberties indicator
# Data is available at: https://www.genderindex.org/ranking/
# A cleaned version with the indicator and ISO number is in the input data folder as "Res_Civil_Sigi.csv"

Res_Civil <- read.csv("Input Data/Res_Civil_Sigi.csv")

###############################################################
# Working poverty rate (percentage of employed youth - 15-24 -  living below US$1.90 PPP) (%)
# The data comes from the ILO:
# https://www.ilo.org/shinyapps/bulkexplorer54/?lang=en&segment=indicator&id=SDG_0111_SEX_AGE_RT_A
# An excel version is available in the input data folder

work_pov_full <- read.csv("Input Data/SDG_0111_SEX_AGE_RT_A-filtered-2021-08-02.csv")

# Want to select those 15-24, add in the ISONum for each country
work_pov <- work_pov_full %>% rename(WorkPov_Year=time, WorkPov=obs_value, country=1) %>%
  mutate(ISONum=case_when(country=="Afghanistan" ~ 4,
                          country=="Albania" ~ 8,
                          country=="Algeria" ~ 12,
                          country=="Angola" ~ 24,
                          country=="Argentina" ~ 32,
                          country=="Armenia" ~ 51,
                          country=="Azerbaijan" ~ 31,
                          country=="Bahamas" ~ 44,
                          country=="Bangladesh" ~ 50,
                          country=="Barbados" ~ 52,
                          country=="Belarus" ~ 112,
                          country=="Belize" ~ 84,
                          country=="Benin" ~ 204,
                          country=="Bhutan" ~ 64,
                          country=="Bolivia" ~ 68,
                          country=="Bosnia and Herzegovina" ~ 70,
                          country=="Botswana" ~ 72,
                          country=="Brazil" ~ 76,
                          country=="Brunei Darussalam" ~ 96,
                          country=="Burkina Faso" ~ 854,
                          country=="Burundi" ~ 108,
                          country=="Côte d'Ivoire" ~ 384,
                          country=="Cambodia" ~ 116,
                          country=="Cameroon" ~ 120,
                          country=="Cape Verde" ~ 132,
                          country=="Central African Republic" ~ 140,
                          country=="Chad" ~ 148,
                          country=="Chile" ~ 152,
                          country=="China" ~ 156,
                          country=="Colombia" ~ 170,
                          country=="Comoros" ~ 174,
                          country=="Congo" ~ 178,
                          country=="Congo, Democratic Republic of the" ~ 180,
                          country=="Costa Rica" ~ 188,
                          country=="Cuba" ~ 192,
                          country=="Dominican Republic" ~ 214,
                          country=="Ecuador" ~ 218,
                          country=="Egypt" ~ 818,
                          country=="El Salvador" ~ 222,
                          country=="Equatorial Guinea" ~ 226,
                          country=="Eritrea" ~ 232,
                          country=="Eswatini" ~ 748,
                          country=="Ethiopia" ~ 231,
                          country=="Fiji" ~ 242,
                          country=="Gabon" ~ 266,
                          country=="Gambia" ~ 270,
                          country=="Georgia" ~ 268,
                          country=="Ghana" ~ 288,
                          country=="Guatemala" ~ 320,
                          country=="Guinea" ~ 324,
                          country=="Guinea-Bissau" ~ 624,
                          country=="Guyana" ~ 328,
                          country=="Haiti" ~ 332,
                          country=="Honduras" ~ 340,
                          country=="Hong Kong, China" ~ 344,
                          country=="India" ~ 356,
                          country=="Indonesia" ~ 360,
                          country=="Iran, Islamic Republic of" ~ 364,
                          country=="Iraq" ~ 368,
                          country=="Jamaica" ~ 388,
                          country=="Jordan" ~ 400,
                          country=="Kazakhstan" ~ 398,
                          country=="Kenya" ~ 404,
                          country=="Korea, Democratic People's Republic of" ~ 408,
                          country=="Korea, Republic of" ~ 410,
                          country=="Kuwait" ~ 414,
                          country=="Kyrgyzstan" ~ 417,
                          country=="Lao People's Democratic Republic" ~ 418,
                          country=="Lebanon" ~ 422,
                          country=="Lesotho" ~ 426,
                          country=="Liberia" ~ 430,
                          country=="Libya" ~ 434,
                          country=="Macau, China" ~ 446,
                          country=="Madagascar" ~ 450,
                          country=="Malawi" ~ 454,
                          country=="Malaysia" ~ 458,
                          country=="Maldives" ~ 462,
                          country=="Mali" ~ 466,
                          country=="Mauritania" ~ 478,
                          country=="Mauritius" ~ 480,
                          country=="Mexico" ~ 484,
                          country=="Moldova, Republic of" ~ 498,
                          country=="Mongolia" ~ 496,
                          country=="Montenegro" ~ 499,
                          country=="Morocco" ~ 504,
                          country=="Mozambique" ~ 508,
                          country=="Myanmar" ~ 104,
                          country=="Namibia" ~ 516,
                          country=="Nepal" ~ 524,
                          country=="Nicaragua" ~ 558,
                          country=="Niger" ~ 562,
                          country=="Nigeria" ~ 566,
                          country=="North Macedonia" ~ 807,
                          country=="Occupied Palestinian Territory" ~ 275,
                          country=="Oman" ~ 512,
                          country=="Pakistan" ~ 586,
                          country=="Panama" ~ 591,
                          country=="Papua New Guinea" ~ 598,
                          country=="Paraguay" ~ 600,
                          country=="Peru" ~ 604,
                          country=="Philippines" ~ 608,
                          country=="Puerto Rico" ~ 630,
                          country=="Qatar" ~ 634,
                          country=="Russian Federation" ~ 643,
                          country=="Rwanda" ~ 646,
                          country=="Saudi Arabia" ~ 682,
                          country=="Senegal" ~ 686,
                          country=="Serbia" ~ 688,
                          country=="Sierra Leone" ~ 694,
                          country=="Singapore" ~ 702,
                          country=="Solomon Islands" ~ 90,
                          country=="Somalia" ~ 706,
                          country=="South Africa" ~ 710,
                          country=="Sri Lanka" ~ 144,
                          country=="Sudan" ~ 729,
                          country=="Suriname" ~ 740,
                          country=="Syrian Arab Republic" ~ 760,
                          country=="Taiwan, China" ~ 158,
                          country=="Tajikistan" ~ 762,
                          country=="Tanzania, United Republic of" ~ 834,
                          country=="Thailand" ~ 764,
                          country=="Timor-Leste" ~ 626,
                          country=="Togo" ~ 768,
                          country=="Trinidad and Tobago" ~ 780,
                          country=="Tunisia" ~ 788,
                          country=="Turkey" ~ 792,
                          country=="Turkmenistan" ~ 795,
                          country=="Uganda" ~ 800,
                          country=="Ukraine" ~ 804,
                          country=="United Arab Emirates" ~ 784,
                          country=="Uruguay" ~ 858,
                          country=="Uzbekistan" ~ 860,
                          country=="Venezuela, Bolivarian Republic of" ~ 862,
                          country=="Viet Nam" ~ 704,
                          country=="Yemen" ~ 887,
                          country=="Zambia" ~ 894,
                          country=="Zimbabwe" ~ 716)) %>%
  filter(!is.na(ISONum)) %>%
  select(ISONum, WorkPov, WorkPov_Year)

###############################################################################################
# Youth interpersonal violence: Interpersonal violence death rate, ages 15 to 24, per 100,000 
# Youth Suicide Mortality
# Both indicators come from Institute for Health Metrics and Evaluation
# Data can be downloaded from http://ghdx.healthdata.org/gbd-results-tool
# A copy of the downloaded results in the input data folder "IHME - Interpersonal Violence and Self Harm Death Rates.xlsx"

harm <- read_excel("Input Data/IHME - Interpersonal Violence and Self Harm Death Rates.xlsx", sheet = "IHME-GBD_2019_DATA-7d051dd5-1")

# Want to rename variables, assign ISONum, and select desired age range
harm_clean <- harm %>% filter(age_name=="10 to 24") %>% select(location_name, cause_name, year, val) %>%
  gather(Variable, Value, year:val) %>% mutate(Variable=paste(cause_name, Variable, sep="_")) %>% select(-cause_name) %>%
  spread(Variable, Value) %>%
  rename(InterViolence=2, InterViolence_Year=3, SelfHarm=4, SelfHarm_Year=5) %>%
  mutate(ISONum=case_when(location_name=="Afghanistan" ~ 4,
                          location_name=="Albania" ~ 8,
                          location_name=="Algeria" ~ 12,
                          location_name=="American Samoa" ~ 16,
                          location_name=="Andorra" ~ 20,
                          location_name=="Angola" ~ 24,
                          location_name=="Antigua and Barbuda" ~ 28,
                          location_name=="Argentina" ~ 32,
                          location_name=="Armenia" ~ 51,
                          location_name=="Australia" ~ 36,
                          location_name=="Austria" ~ 40,
                          location_name=="Azerbaijan" ~ 31,
                          location_name=="Bahamas" ~ 44,
                          location_name=="Bahrain" ~ 48,
                          location_name=="Bangladesh" ~ 50,
                          location_name=="Barbados" ~ 52,
                          location_name=="Belarus" ~ 112,
                          location_name=="Belgium" ~ 56,
                          location_name=="Belize" ~ 84,
                          location_name=="Benin" ~ 204,
                          location_name=="Bermuda" ~ 60,
                          location_name=="Bhutan" ~ 64,
                          location_name=="Bolivia (Plurinational State of)" ~ 68,
                          location_name=="Bosnia and Herzegovina" ~ 70,
                          location_name=="Botswana" ~ 72,
                          location_name=="Brazil" ~ 76,
                          location_name=="Brunei Darussalam" ~ 96,
                          location_name=="Bulgaria" ~ 100,
                          location_name=="Burkina Faso" ~ 854,
                          location_name=="Burundi" ~ 108,
                          location_name=="Côte d'Ivoire" ~ 384,
                          location_name=="Cabo Verde" ~ 132,
                          location_name=="Cambodia" ~ 116,
                          location_name=="Cameroon" ~ 120,
                          location_name=="Canada" ~ 124,
                          location_name=="Central African Republic" ~ 140,
                          location_name=="Chad" ~ 148,
                          location_name=="Chile" ~ 152,
                          location_name=="China" ~ 156,
                          location_name=="Colombia" ~ 170,
                          location_name=="Comoros" ~ 174,
                          location_name=="Congo" ~ 178,
                          location_name=="Cook Islands" ~ 184,
                          location_name=="Costa Rica" ~ 188,
                          location_name=="Croatia" ~ 191,
                          location_name=="Cuba" ~ 192,
                          location_name=="Cyprus" ~ 196,
                          location_name=="Czechia" ~ 203,
                          location_name=="Democratic People's Republic of Korea" ~ 408,
                          location_name=="Democratic Republic of the Congo" ~ 180,
                          location_name=="Denmark" ~ 208,
                          location_name=="Djibouti" ~ 262,
                          location_name=="Dominica" ~ 212,
                          location_name=="Dominican Republic" ~ 214,
                          location_name=="Ecuador" ~ 218,
                          location_name=="Egypt" ~ 818,
                          location_name=="El Salvador" ~ 222,
                          location_name=="Equatorial Guinea" ~ 226,
                          location_name=="Eritrea" ~ 232,
                          location_name=="Estonia" ~ 233,
                          location_name=="Eswatini" ~ 748,
                          location_name=="Ethiopia" ~ 231,
                          location_name=="Fiji" ~ 242,
                          location_name=="Finland" ~ 246,
                          location_name=="France" ~ 250,
                          location_name=="Gabon" ~ 266,
                          location_name=="Gambia" ~ 270,
                          location_name=="Georgia" ~ 268,
                          location_name=="Germany" ~ 276,
                          location_name=="Ghana" ~ 288,
                          location_name=="Greece" ~ 300,
                          location_name=="Greenland" ~ 304,
                          location_name=="Grenada" ~ 308,
                          location_name=="Guam" ~ 316,
                          location_name=="Guatemala" ~ 320,
                          location_name=="Guinea" ~ 324,
                          location_name=="Guinea-Bissau" ~ 624,
                          location_name=="Guyana" ~ 328,
                          location_name=="Haiti" ~ 332,
                          location_name=="Honduras" ~ 340,
                          location_name=="Hungary" ~ 348,
                          location_name=="Iceland" ~ 352,
                          location_name=="India" ~ 356,
                          location_name=="Indonesia" ~ 360,
                          location_name=="Iran (Islamic Republic of)" ~ 364,
                          location_name=="Iraq" ~ 368,
                          location_name=="Ireland" ~ 372,
                          location_name=="Israel" ~ 376,
                          location_name=="Italy" ~ 380,
                          location_name=="Jamaica" ~ 388,
                          location_name=="Japan" ~ 392,
                          location_name=="Jordan" ~ 400,
                          location_name=="Kazakhstan" ~ 398,
                          location_name=="Kenya" ~ 404,
                          location_name=="Kiribati" ~ 296,
                          location_name=="Kuwait" ~ 414,
                          location_name=="Kyrgyzstan" ~ 417,
                          location_name=="Lao People's Democratic Republic" ~ 418,
                          location_name=="Latvia" ~ 428,
                          location_name=="Lebanon" ~ 422,
                          location_name=="Lesotho" ~ 426,
                          location_name=="Liberia" ~ 430,
                          location_name=="Libya" ~ 434,
                          location_name=="Lithuania" ~ 440,
                          location_name=="Luxembourg" ~ 442,
                          location_name=="Madagascar" ~ 450,
                          location_name=="Malawi" ~ 454,
                          location_name=="Malaysia" ~ 458,
                          location_name=="Maldives" ~ 462,
                          location_name=="Mali" ~ 466,
                          location_name=="Malta" ~ 470,
                          location_name=="Marshall Islands" ~ 584,
                          location_name=="Mauritania" ~ 478,
                          location_name=="Mauritius" ~ 480,
                          location_name=="Mexico" ~ 484,
                          location_name=="Micronesia (Federated States of)" ~ 583,
                          location_name=="Monaco" ~ 492,
                          location_name=="Mongolia" ~ 496,
                          location_name=="Montenegro" ~ 499,
                          location_name=="Morocco" ~ 504,
                          location_name=="Mozambique" ~ 508,
                          location_name=="Myanmar" ~ 104,
                          location_name=="Namibia" ~ 516,
                          location_name=="Nauru" ~ 520,
                          location_name=="Nepal" ~ 524,
                          location_name=="Netherlands" ~ 528,
                          location_name=="New Zealand" ~ 554,
                          location_name=="Nicaragua" ~ 558,
                          location_name=="Niger" ~ 562,
                          location_name=="Nigeria" ~ 566,
                          location_name=="Niue" ~ 570,
                          location_name=="North Macedonia" ~ 807,
                          location_name=="Northern Mariana Islands" ~ 580,
                          location_name=="Norway" ~ 578,
                          location_name=="Oman" ~ 512,
                          location_name=="Pakistan" ~ 586,
                          location_name=="Palau" ~ 585,
                          location_name=="Palestine" ~ 275,
                          location_name=="Panama" ~ 591,
                          location_name=="Papua New Guinea" ~ 598,
                          location_name=="Paraguay" ~ 600,
                          location_name=="Peru" ~ 604,
                          location_name=="Philippines" ~ 608,
                          location_name=="Poland" ~ 616,
                          location_name=="Portugal" ~ 620,
                          location_name=="Puerto Rico" ~ 630,
                          location_name=="Qatar" ~ 634,
                          location_name=="Republic of Korea" ~ 410,
                          location_name=="Republic of Moldova" ~ 498,
                          location_name=="Romania" ~ 642,
                          location_name=="Russian Federation" ~ 643,
                          location_name=="Rwanda" ~ 646,
                          location_name=="Saint Kitts and Nevis" ~ 659,
                          location_name=="Saint Lucia" ~ 662,
                          location_name=="Saint Vincent and the Grenadines" ~ 670,
                          location_name=="Samoa" ~ 882,
                          location_name=="San Marino" ~ 674,
                          location_name=="Sao Tome and Principe" ~ 678,
                          location_name=="Saudi Arabia" ~ 682,
                          location_name=="Senegal" ~ 686,
                          location_name=="Serbia" ~ 688,
                          location_name=="Seychelles" ~ 690,
                          location_name=="Sierra Leone" ~ 694,
                          location_name=="Singapore" ~ 702,
                          location_name=="Slovakia" ~ 703,
                          location_name=="Slovenia" ~ 705,
                          location_name=="Solomon Islands" ~ 90,
                          location_name=="Somalia" ~ 706,
                          location_name=="South Africa" ~ 710,
                          location_name=="South Sudan" ~ 728,
                          location_name=="Spain" ~ 724,
                          location_name=="Sri Lanka" ~ 144,
                          location_name=="Sudan" ~ 729,
                          location_name=="Suriname" ~ 740,
                          location_name=="Sweden" ~ 752,
                          location_name=="Switzerland" ~ 756,
                          location_name=="Syrian Arab Republic" ~ 760,
                          location_name=="Taiwan (Province of China)" ~ 158,
                          location_name=="Tajikistan" ~ 762,
                          location_name=="Thailand" ~ 764,
                          location_name=="Timor-Leste" ~ 626,
                          location_name=="Togo" ~ 768,
                          location_name=="Tokelau" ~ 772,
                          location_name=="Tonga" ~ 776,
                          location_name=="Trinidad and Tobago" ~ 780,
                          location_name=="Tunisia" ~ 788,
                          location_name=="Turkey" ~ 792,
                          location_name=="Turkmenistan" ~ 795,
                          location_name=="Tuvalu" ~ 798,
                          location_name=="Uganda" ~ 800,
                          location_name=="Ukraine" ~ 804,
                          location_name=="United Arab Emirates" ~ 784,
                          location_name=="United Kingdom" ~ 826,
                          location_name=="United Republic of Tanzania" ~ 834,
                          location_name=="United States of America" ~ 840,
                          location_name=="United States Virgin Islands" ~ 850,
                          location_name=="Uruguay" ~ 858,
                          location_name=="Uzbekistan" ~ 860,
                          location_name=="Vanuatu" ~ 548,
                          location_name=="Venezuela (Bolivarian Republic of)" ~ 862,
                          location_name=="Viet Nam" ~ 704,
                          location_name=="Yemen" ~ 887,
                          location_name=="Zambia" ~ 894,
                          location_name=="Zimbabwe" ~ 716)) %>%
  select(ISONum, InterViolence, InterViolence_Year, SelfHarm, SelfHarm_Year)

####################################################################################
# Inform Risk Index 2022 is a global humanitarian risk analysis
# Excel results from:
# https://drmkc.jrc.ec.europa.eu/inform-index/INFORM-Risk

inform <- read_excel("Input Data/INFORM_Risk_Mid2021_v053.xlsx", sheet = "INFORM Risk Mid2021 (a-z)")

inform_mat <- as.matrix(inform)
inform_mat_clean <- inform_mat[2:192, c(1, 2, 34)]

inform_clean <- as.data.frame(inform_mat_clean) %>% rename(ISOAlph=2, INFORM=3) %>% 
  mutate(INFORM=as.numeric(as.character(INFORM))) %>% left_join(countryiso, by="ISOAlph") %>%
  mutate(INFORM_Year=2021) %>%
  select(ISONum, INFORM, INFORM_Year)

##########################################################################
# For the share for youth displaced, collected data from UNHCR and Population data from UNPD
# https://www.unhcr.org/refugee-statistics/download/?url=5EGlre	
# https://www.unhcr.org/refugee-statistics/methodology/
# https://population.un.org/wpp/Download/Standard/Interpolated/	
# We have put together an excel file:
  
disp <- read_excel("Input Data/UNHCR _ UNPD Youth Displacement Indicator 7.30.21.xlsx", sheet = "Sheet1")

disp_mat <- as.matrix(disp)
disp_mat_clean <- disp_mat[4:206, c(1, 3, 7, 8 )]

disp_clean <- as.data.frame(disp_mat_clean) %>% rename(Displacement_Year=1, ISONum=2, Disp_num=3, Pop=4) %>%
  filter(!is.na(ISONum)) %>% filter(!is.na(Pop)) %>%
  mutate(Disp_num=as.numeric(as.character(Disp_num)), Pop=as.numeric(as.character(Pop))) %>%
  mutate(Displacement=Disp_num/Pop) %>%
  select(ISONum, Displacement, Displacement_Year)  %>%
  mutate(ISONum=as.numeric(as.character(ISONum)))

###################################################################
# NATIONAL YOUTH POLICY OVERVIEW
# https://www.youthpolicy.org/nationalyouthpolicies/
policy <- read_excel("Input Data/Youth Policy Full.xlsx", sheet = "Sheet1")

table(policy$YouthPolicy)

policy_clean <- policy %>% mutate(YouthPolicy_Num=case_when(YouthPolicy=="Yes" ~ 1,
                                                            YouthPolicy=="No" ~ 0,
                                                            YouthPolicy=="Draft" ~ .5)) %>%
  select(-Country) %>% mutate(YouthPolicy_Year=2021)

######################################################################
# Percent of Parliament under 30
# Inter-Parliamentary Union
# Youth Participation in National Parliaments
# https://www.ipu.org/youth2021
# Page 61 and 62 of their 2021 report

parliament30 <- read.csv("Input Data/Parliment30.csv") %>% filter(!is.na(ISONum))

###################################################################
# Combine all data sets together
full_data <- full_join(SH_LGR_ACSRHES1, SH_LGR_ACSRHES2,  by="ISONum") %>%
  full_join(SH_LGR_ACSRHES3,  by="ISONum") %>%
  full_join(SH_LGR_ACSRHES4,  by="ISONum") %>%
  full_join(ncifp_clean,  by="ISONum") %>%
  full_join(HIV_w_Knowledge,  by="ISONum") %>%
  full_join(HIV_m_Knowledge,  by="ISONum") %>%
  full_join(know_ltm_stm_w,  by="ISONum") %>%
  full_join(know_ltm_stm_m,  by="ISONum") %>%
  full_join(fert_know_w,  by="ISONum") %>%
  full_join(fert_know_m,  by="ISONum") %>%
  full_join(demand_satisfied,  by="ISONum") %>%
  full_join(hiv_clean,  by="ISONum") %>%
  full_join(asfr,  by="ISONum") %>%
  full_join(SH_STA_MORT,  by="ISONum") %>%
  full_join(SE_TOT_CPLR,  by="ISONum") %>%
  full_join(Res_Civil,  by="ISONum")  %>%
  full_join(SP_DYN_MRBF18,  by="ISONum") %>%
  full_join(violence,  by="ISONum") %>%
  full_join(Educ_GDP,  by="ISONum") %>%
  full_join(SE.SEC.ENRR,  by="ISONum") %>%
  full_join(gwp1_n_5,  by="ISONum") %>%
  full_join(gwp2_n_5,  by="ISONum") %>%
  full_join(SL_CPA_YEMP,  by="ISONum") %>%
  full_join(account_t_d_5, by="ISONum") %>%
  full_join(account_t_d_6,  by="ISONum") %>%
  full_join(SL_TLF_NEET,  by="ISONum") %>%
  full_join(BetterOff,  by="ISONum") %>%
  full_join(work_pov,  by="ISONum") %>%
  full_join(policy_clean,  by="ISONum") %>%
  full_join(Vote_National,  by="ISONum") %>%
  full_join(active_member,  by="ISONum") %>%
  full_join(parliament30,  by="ISONum") %>%
  full_join(inform_clean,  by="ISONum") %>%
  full_join(pol_part,  by="ISONum") %>%
  full_join(humanrights,  by="ISONum") %>%
  full_join(disp_clean,  by="ISONum") %>%
  full_join(harm_clean,  by="ISONum")

# Saving file as CSV to read into our analysis code
write.csv(full_data, "Input Data/Full Input Dataset 111221.csv", na="", row.names = F)
  