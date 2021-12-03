# DHS Microdata Analysis for UNFPA Young People Empowerment Index
# November 2021
# Code written by Kristin Bietsch, PhD; Avenir Health; kbietsch@avenirhealth.org

library(survey)
library(tibble)
library(dplyr)
library(tidyr)
library(haven)
library(stringr)
library(questionr)
library(sjlabelled)
library(xlsx)


options(scipen=999)
memory.limit(size = 2e6) 


# Load the master list of DHS surveys (will need to update this file as new DHS become available)
surveys <- read.xlsx2("C:/Users/KristinBietsch/files/UNFPA/Code for GitHub/Input data/Master DHS Survey List.xlsx",sheetName="Sheet1",startRow=1,header=TRUE,
                      colClasses = c("character", "character", "character", "numeric",
                                     "character", "character", "character", "character", "numeric",
                                     "character", "numeric", "character", "character", "character", "character"));


# Creating a list of women's surveys
women_survey <- surveys %>% mutate(IRfile=paste(Survey, ".DTA", sep="")) %>% select(API_ID, IRfile) 

# Creating a list of men's surveys
men_survey <- surveys %>% filter(MR!="") %>% mutate(MRfile=paste(MR, ".DTA", sep="")) %>% select(API_ID, MRfile) %>% 
  filter(API_ID!="BJ1996DHS")  %>% filter(API_ID!="KE1993DHS") %>% filter(API_ID!="MW1992DHS") %>% 
  filter(API_ID!="SN1997DHS") %>% filter(API_ID!="SN1993DHS")


# Create results frame
knows_ltm_stm_w_df <- setNames(data.frame(matrix(ncol = 3,  nrow = 0)),  c( "Survey", "Age", "knows_ltm_stm_w")) %>% mutate(Survey=as.character(Survey)) %>% mutate(Age=as.character(Age))
fert_know_w_df <- setNames(data.frame(matrix(ncol = 3,  nrow = 0)),  c( "Survey", "Age", "fert_know_w")) %>% mutate(Survey=as.character(Survey)) %>% mutate(Age=as.character(Age))
knows_ltm_stm_m_df <- setNames(data.frame(matrix(ncol = 3,  nrow = 0)),  c( "Survey", "Age", "knows_ltm_stm_m")) %>% mutate(Survey=as.character(Survey)) %>% mutate(Age=as.character(Age))
fert_know_m_df <- setNames(data.frame(matrix(ncol = 3,  nrow = 0)),  c( "Survey", "Age", "fert_know_m")) %>% mutate(Survey=as.character(Survey)) %>% mutate(Age=as.character(Age))

# We are going to run a loop through all the women's surveys
for (row in 1:nrow(women_survey)) {
  women_data <- women_survey[row, "IRfile"]
  countryname <- women_survey[row, "API_ID"]
  
  # You will need to reset the directory to where you DHS surveys are stored
  setwd("C:/Users/KristinBietsch/Files/DHSLoop")
  
  # Loading surveys, only the variables we need
  allwomen <- read_dta(women_data, col_select = any_of(c("v005", "v013", "v304_01", "v304_02", "v304_03", "v304_04", "v304_05", "v304_06", "v304_07", "v304_11" , "v304_13" ,  "v304_14" , "v304_15" , "v217")))
  women <- allwomen
  
  women$sampleweights <- women$v005/100000
  
  # Only creating variables if the input variables exist
  if (exists("v304_02", women) & exists("v304_06", women) & exists("v304_07", women) & exists("v304_11", women) & sum(women$v304_02, na.rm=T)>0) {
    women <- women %>% mutate(knows_ltm_w = case_when(v304_02==1 | v304_06==1 | v304_07==1  | v304_11==1 | v304_02==2 | v304_06==2 | v304_07==2  | v304_11==2 ~ 1,
                                                      v304_02==0 & v304_06==0 & v304_07==0  & v304_11==0  ~ 0,
                                                      v304_02==8 | v304_06==8 | v304_07==8  | v304_11==8  ~ 0 ,
                                                      v304_02==7 | v304_06==7 | v304_07==7  | v304_11==7  ~ 0 ),
                              knows_stm_w= case_when(v304_01==1 | v304_03==1 | v304_04==1  | v304_05==1 | v304_13==1 | v304_14==1 | v304_15==1 | v304_01==2 | v304_03==2 | v304_04==2  | v304_05==2 | v304_13==2 | v304_14==2 | v304_15==2 ~ 1,
                                                     v304_01==0 & v304_03==0 & v304_04==0  & v304_05==0  & v304_13==0  & v304_14==0  & v304_15==0  ~ 0,
                                                     v304_01==8 | v304_03==8 | v304_04==8  | v304_05==8  | v304_13==8  | v304_14==8  | v304_15==8  ~ 0 ,
                                                     v304_01==7 | v304_03==7 | v304_04==7  | v304_05==7  | v304_13==7  | v304_14==7  | v304_15==7  ~ 0 ),
                              knows_ltm_stm_w = case_when(knows_ltm_w==1 & knows_stm_w==1 ~ 1, knows_ltm_w!=1 | knows_stm_w!=1 ~ 0))
    
    women1524 <- filter(women, v013<=2)
    
    knows_ltm_stm_w1524 <-    as.data.frame(prop.table(wtd.table( women1524$knows_ltm_stm_w,  weights=women1524$sampleweights )))    %>% filter(Var1==1)  %>% rename(knows_ltm_stm_w=Freq) %>% mutate(Survey=countryname)  %>% mutate(Age="15-24") %>% select(Survey, Age, knows_ltm_stm_w)
    
    
    knows_ltm_stm_w_df <- bind_rows(knows_ltm_stm_w_df , knows_ltm_stm_w1524)
    
    
  }
  
  if (exists("v217", women) & sum(women$v217, na.rm=T)>0) {
    women <- women %>% mutate(fert_know_w=case_when(v217==3 ~ 1, v217!=3 ~ 0))
    
    women1524 <- filter(women, v013<=2)
    
    fert_know_w1524 <-    as.data.frame(prop.table(wtd.table( women1524$fert_know_w,  weights=women1524$sampleweights ))) %>% filter(Var1==1)  %>% rename(fert_know_w=Freq) %>% mutate(Survey=countryname) %>% mutate(Age="15-24") %>% select(Survey,   Age, fert_know_w)
    fert_know_w_df <- bind_rows(fert_know_w_df , fert_know_w1524)
    
  }
  
}



#######################

# Loop for the men's surveys
for (row in 1:nrow(men_survey)) {
  men_data <- men_survey[row, "MRfile"]
  countryname <- men_survey[row, "API_ID"]
  
  setwd("C:/Users/KristinBietsch/Files/DHSLoop")
  
  allmen <- read_dta(men_data, col_select = any_of(c("mv005", "mv013", "mv304_01", "mv304_02", "mv304_03", "mv304_04", "mv304_05", "mv304_06", "mv304_07", "mv304_11" , "mv304_13" ,  "mv304_14" , "mv304_15" ,  "mv217")))
  men <- allmen
  
  men$sampleweights <- men$mv005/100000
  
  if (exists("mv304_02", men) & exists("mv304_06", men) & exists("mv304_07", men) & exists("mv304_11", men)  & sum(men$mv304_02, na.rm=T)>0) {
    
    
    men <- men %>% mutate(knows_ltm_m = case_when(mv304_02==1 | mv304_06==1 | mv304_07==1  | mv304_11==1 | mv304_02==2 | mv304_06==2 | mv304_07==2  | mv304_11==2 ~ 1,
                                                  mv304_02==0 & mv304_06==0 & mv304_07==0  & mv304_11==0  ~ 0,
                                                  mv304_02==8 | mv304_06==8 | mv304_07==8  | mv304_11==8  ~ 0 ,
                                                  mv304_02==7 | mv304_06==7 | mv304_07==7  | mv304_11==7  ~ 0 ),
                          knows_stm_m= case_when(mv304_01==1 | mv304_03==1 | mv304_04==1  | mv304_05==1 | mv304_13==1 | mv304_14==1 | mv304_15==1 | mv304_01==2 | mv304_03==2 | mv304_04==2  | mv304_05==2 | mv304_13==2 | mv304_14==2 | mv304_15==2 ~ 1,
                                                 mv304_01==0 & mv304_03==0 & mv304_04==0  & mv304_05==0  & mv304_13==0  & mv304_14==0  & mv304_15==0  ~ 0,
                                                 mv304_01==8 | mv304_03==8 | mv304_04==8  | mv304_05==8  | mv304_13==8  | mv304_14==8  | mv304_15==8  ~ 0 ,
                                                 mv304_01==7 | mv304_03==7 | mv304_04==7  | mv304_05==7  | mv304_13==7  | mv304_14==7  | mv304_15==7  ~ 0 ),
                          knows_ltm_stm_m = case_when(knows_ltm_m==1 & knows_stm_m==1 ~ 1, knows_ltm_m!=1 | knows_stm_m!=1 ~ 0))
    
    men1524 <- filter(men, mv013<=2)
    
    knows_ltm_stm_m1524 <-    as.data.frame(prop.table(wtd.table( men1524$knows_ltm_stm_m,  weights=men1524$sampleweights )))    %>% filter(Var1==1)  %>% rename(knows_ltm_stm_m=Freq) %>% mutate(Survey=countryname)  %>% mutate(Age="15-24") %>% select(Survey, Age, knows_ltm_stm_m)
    
    
    knows_ltm_stm_m_df <- bind_rows(knows_ltm_stm_m_df ,knows_ltm_stm_m1524)
    
    
  }
  
  if (exists("mv217", men)  & sum(men$mv217, na.rm=T)>0) {
    men <- men %>% mutate(fert_know_m=case_when(mv217==3 ~ 1, mv217!=3 ~ 0))
    
    men1524 <- filter(men, mv013<=2)

    fert_know_m1524 <-    as.data.frame(prop.table(wtd.table( men1524$fert_know_m,  weights=men1524$sampleweights ))) %>% filter(Var1==1)  %>% rename(fert_know_m=Freq) %>% mutate(Survey=countryname) %>% mutate(Age="15-24")  %>% select(Survey, Age, fert_know_m)
    fert_know_m_df <- bind_rows(fert_know_m_df ,   fert_know_m1524)
    
  }
  
}

#######################
# Clean files
fert_know_m_clean <- fert_know_m_df %>% rename(API_ID=Survey) %>% full_join(surveys, by="API_ID") %>%
  select(ISONum, StartYear, fert_know_m) %>% 
  filter(!is.na(fert_know_m)) %>%
  group_by(ISONum) %>%
  mutate(max=max(StartYear)) %>%
  ungroup() %>%
  filter(StartYear==max) %>%
  rename(fert_know_m_Year=StartYear) %>%
  select(ISONum, fert_know_m, fert_know_m_Year)

fert_know_w_clean <- fert_know_w_df %>% rename(API_ID=Survey) %>% full_join(surveys, by="API_ID") %>%
  select(ISONum, StartYear, fert_know_w) %>% 
  filter(!is.na(fert_know_w)) %>%
  group_by(ISONum) %>%
  mutate(max=max(StartYear)) %>%
  ungroup() %>%
  filter(StartYear==max) %>%
  rename(fert_know_w_Year=StartYear) %>%
  select(ISONum, fert_know_w, fert_know_w_Year)

know_ltm_stm_m_clean <- knows_ltm_stm_m_df %>% rename(API_ID=Survey) %>% full_join(surveys, by="API_ID") %>%
  select(ISONum, StartYear, knows_ltm_stm_m) %>% 
  filter(!is.na(knows_ltm_stm_m)) %>%
  group_by(ISONum) %>%
  mutate(max=max(StartYear)) %>%
  ungroup() %>%
  filter(StartYear==max) %>%
  rename(knows_ltm_stm_m_Year=StartYear) %>%
  select(ISONum, knows_ltm_stm_m, knows_ltm_stm_m_Year)

know_ltm_stm_w_clean <- knows_ltm_stm_w_df %>% rename(API_ID=Survey) %>% full_join(surveys, by="API_ID") %>%
  select(ISONum, StartYear, knows_ltm_stm_w) %>% 
  filter(!is.na(knows_ltm_stm_w)) %>%
  group_by(ISONum) %>%
  mutate(max=max(StartYear)) %>%
  ungroup() %>%
  filter(StartYear==max) %>%
  rename(knows_ltm_stm_w_Year=StartYear) %>%
  select(ISONum, knows_ltm_stm_w, knows_ltm_stm_w_Year)

#######################
# Write Files as CSV
write.csv(fert_know_m_clean, "C:/Users/KristinBietsch/files/UNFPA/Code for GitHub/Input data/fert_know_m.csv", na="", row.names = F)
write.csv(fert_know_w_clean, "C:/Users/KristinBietsch/files/UNFPA/Code for GitHub/Input data/fert_know_w.csv", na="", row.names = F)
write.csv(know_ltm_stm_m_clean, "C:/Users/KristinBietsch/files/UNFPA/Code for GitHub/Input data/know_ltm_stm_m.csv", na="", row.names = F)
write.csv(know_ltm_stm_w_clean, "C:/Users/KristinBietsch/files/UNFPA/Code for GitHub/Input data/know_ltm_stm_w.csv", na="", row.names = F)