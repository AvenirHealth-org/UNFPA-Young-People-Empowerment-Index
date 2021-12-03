# World Value Survey Microdata Analysis for UNFPA Young People Empowerment Index
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
library(zoo)
library(haven)
library(data.table)


options(scipen=999)
memory.limit(size = 2e6) 



# Microdata for the WVS can be downloaded at worldvaluessurvey.org
wvs2017 <- read_dta("C:/Users/KristinBietsch/files/UNFPA/Indicators/WVS_Cross-National_Wave_7_stata_v2_0.dta")
wvs2010 <- read_dta("C:/Users/KristinBietsch/files/UNFPA/Indicators/WV6_Data_stata_v20201117.dta")

# 2017-2020
#W_Weight- weight
#B_COUNTRY
#Age: Q262


# 2010-2014
#V258- weight
#V2 - COUNTRY
#Age: V242



########################################################
# The data collection takes place over several years

year17 <- wvs2017  %>% 
  group_by(B_COUNTRY) %>%
  summarise(Year=mean(A_YEAR)) %>%
  rename(ISONum=B_COUNTRY) %>% 
  mutate(Round=2017)

year10 <- wvs2010  %>% 
  group_by(V2) %>%
  summarise(Year=mean(V262)) %>% 
  rename(ISONum=V2) %>% 
  mutate(Round=2010)

year <- bind_rows(year17, year10)

##############################################################################
# Vote in National Elections (% 16-24)

# Check the coding here for each survey
attr(wvs2010$V227,"labels")
attr(wvs2017$Q222,"labels")

national10 <- wvs2010 %>% filter(!is.na(V227)) %>%
  filter(V242>=16 & V242<=24) %>%
  mutate(vote_national=case_when(V227==1 | V227==2 ~ 1, V227==3 ~ 0)) %>%
  group_by(vote_national, V2) %>% 
  summarise( N_weight = sum(V258)) %>%
  ungroup() %>% group_by(V2) %>%
  mutate(Total=sum(N_weight)) %>% 
  mutate(prop=N_weight/Total) %>%
  filter(vote_national==1) %>%
  select(V2, prop) %>%
  rename(ISONum=V2, Vote_National=prop) %>% 
  mutate(Round=2010)

national17 <- wvs2017 %>% filter(!is.na(Q222)) %>%
  filter(Q262>=16 & Q262<=24) %>%
  mutate(vote_national=case_when(Q222==1 | Q222==2 ~ 1, Q222==3 ~ 0)) %>%
  group_by(vote_national, B_COUNTRY) %>% 
  summarise( N_weight = sum(W_WEIGHT)) %>%
  ungroup() %>% group_by(B_COUNTRY) %>%
  mutate(Total=sum(N_weight)) %>% 
  mutate(prop=N_weight/Total) %>%
  filter(vote_national==1) %>%
  select(B_COUNTRY, prop) %>%
  rename(ISONum=B_COUNTRY, Vote_National=prop) %>% 
  mutate(Round=2017)

# If country is in both rounds, only want to keep the most recent
national <- bind_rows(national10, national17) %>% group_by(ISONum) %>%
  mutate(max=max(Round)) %>% filter(max==Round) %>% select(-max) %>% 
  left_join(year, by=c("ISONum", "Round")) %>% select(-Round) %>% 
  rename(Vote_National_Year=Year)

########################################################
# % of youth who feel their standard of living is better than that of their parents
# Better off is only available in 2017 survey
attr(wvs2017$Q56,"labels")

betteroff17 <- wvs2017 %>% filter(!is.na(Q56)) %>%
  filter(Q262>=16 & Q262<=24) %>%
  mutate(variable=case_when(Q56==1  ~ 1, Q56==2  | Q56==3 ~ 0)) %>% filter(!is.na(variable)) %>%
  group_by(variable, B_COUNTRY) %>% 
  summarise( N_weight = sum(W_WEIGHT)) %>%
  ungroup() %>% group_by(B_COUNTRY) %>%
  mutate(Total=sum(N_weight)) %>% 
  mutate(prop=N_weight/Total) %>%
  filter(variable==1) %>%
  select(B_COUNTRY, prop) %>%
  rename(ISONum=B_COUNTRY, BetterOff=prop) %>% 
  mutate(Round=2017) %>% left_join(year, by=c("ISONum", "Round")) %>% select(-Round) %>% rename(BetterOff_Year=Year)

########################################################
# % of Youth who feel there is a respect for human rights in their country
attr(wvs2010$V142,"labels")

humanrights10 <- wvs2010 %>% filter(!is.na(V142)) %>%
  filter(V242>=16 & V242<=24) %>%
  mutate(variable=case_when(V142==1 | V142==2 ~ 1, V142==3 | V142==4 ~ 0)) %>% filter(!is.na(variable)) %>%
  group_by(variable, V2) %>% 
  summarise( N_weight = sum(V258)) %>%
  ungroup() %>% group_by(V2) %>%
  mutate(Total=sum(N_weight)) %>% 
  mutate(prop=N_weight/Total) %>%
  filter(variable==1) %>%
  select(V2, prop) %>%
  rename(ISONum=V2, humanrights=prop) %>% 
  mutate(Round=2010)

attr(wvs2017$Q253,"labels")

humanrights17 <- wvs2017 %>% filter(!is.na(Q253)) %>%
  filter(Q262>=16 & Q262<=24) %>%
  mutate(variable=case_when(Q253==1 | Q253==2 ~ 1, Q253==3 | Q253==4 ~ 0)) %>% filter(!is.na(variable)) %>%
  group_by(variable, B_COUNTRY) %>% 
  summarise( N_weight = sum(W_WEIGHT)) %>%
  ungroup() %>% group_by(B_COUNTRY) %>%
  mutate(Total=sum(N_weight)) %>% 
  mutate(prop=N_weight/Total) %>%
  filter(variable==1) %>%
  select(B_COUNTRY, prop) %>%
  rename(ISONum=B_COUNTRY, humanrights=prop) %>% 
  mutate(Round=2017)

humanrights <- bind_rows(humanrights10, humanrights17) %>% group_by(ISONum) %>% mutate(max=max(Round)) %>% filter(max==Round) %>% select(-max)   %>% left_join(year, by=c("ISONum", "Round")) %>% select(-Round) %>% rename(humanrights_Year=Year)

#####################################################################
# Youth Participation in Political/Civic Organizations (% 16-24)
attr(wvs2010$V28,"labels")
attr(wvs2010$V29,"labels")
attr(wvs2010$V30,"labels")
attr(wvs2010$V32,"labels")
attr(wvs2010$V34,"labels")

sel <- select(wvs2010, V28, V29, V30, V32, V34)

active_member10 <- wvs2010  %>% filter(!is.na(V28)) %>% filter(!is.na(V29)) %>% filter(!is.na(V30)) %>% filter(!is.na(V32)) %>% filter(!is.na(V34)) %>%
  filter(V242>=16 & V242<=24) %>% 
  mutate(variable=case_when(V28==2 | V29==2 | V30==2 | V32==2 | V34==2 ~ 1, TRUE ~ 0)) %>%
  group_by(variable, V2) %>% 
  summarise( N_weight = sum(V258)) %>%
  ungroup() %>% group_by(V2) %>%
  mutate(Total=sum(N_weight)) %>% 
  mutate(prop=N_weight/Total) %>%
  filter(variable==1) %>%
  select(V2, prop) %>%
  rename(ISONum=V2, active_member=prop) %>% 
  mutate(Round=2010)

attr(wvs2017$Q97,"labels")
attr(wvs2017$Q98,"labels")
attr(wvs2017$Q99,"labels")
attr(wvs2017$Q101,"labels")
attr(wvs2017$Q103, "labels")

sel <- select(wvs2017, Q97, Q98, Q99, Q101, Q103)


active_member17 <- wvs2017  %>% filter(!is.na(Q97)) %>% filter(!is.na(Q98)) %>% filter(!is.na(Q99)) %>% filter(!is.na(Q101)) %>% filter(!is.na(Q103)) %>%
  filter(Q262>=16 & Q262<=24) %>%
  mutate(variable=case_when(Q97==2 | Q98==2 | Q99==2 | Q101==2 | Q103==2 ~ 1, TRUE ~ 0)) %>%
  group_by(variable, B_COUNTRY) %>% 
  summarise( N_weight = sum(W_WEIGHT)) %>%
  ungroup() %>% group_by(B_COUNTRY) %>%
  mutate(Total=sum(N_weight)) %>% 
  mutate(prop=N_weight/Total) %>%
  filter(variable==1) %>%
  select(B_COUNTRY, prop) %>%
  rename(ISONum=B_COUNTRY, active_member=prop) %>% 
  mutate(Round=2017)

active_member <- bind_rows(active_member10, active_member17) %>% group_by(ISONum) %>% mutate(max=max(Round)) %>% filter(max==Round) %>% select(-max)  %>% left_join(year, by=c("ISONum", "Round")) %>% select(-Round) %>% rename(active_member_Year=Year)

########################################################
# % of Youth who participated in political action
attr(wvs2010$V85,"labels")
attr(wvs2010$V87,"labels")
attr(wvs2010$V86,"labels")
attr(wvs2010$V88,"labels")

sel <- select(wvs2010, V85, V87, V86, V88)

pol_part10 <- wvs2010  %>% filter(!is.na(V85)) %>% filter(!is.na(V87)) %>% filter(!is.na(V86)) %>% filter(!is.na(V88)) %>% 
  filter(V242>=16 & V242<=24) %>% 
  mutate(variable=case_when(V85==1 | V86==1 | V87==1 | V88==1 ~ 1, TRUE ~ 0)) %>%
  group_by(variable, V2) %>% 
  summarise( N_weight = sum(V258)) %>%
  ungroup() %>% group_by(V2) %>%
  mutate(Total=sum(N_weight)) %>% 
  mutate(prop=N_weight/Total) %>%
  filter(variable==1) %>%
  select(V2, prop) %>%
  rename(ISONum=V2, pol_part=prop) %>% 
  mutate(Round=2010)

attr(wvs2017$Q209,"labels")
attr(wvs2017$Q210,"labels")
attr(wvs2017$Q211,"labels")
attr(wvs2017$Q212,"labels")

sel <- select(wvs2017, Q209, Q210, Q211, Q212)

pol_part17 <- wvs2017  %>% filter(!is.na(Q209)) %>% filter(!is.na(Q210)) %>% filter(!is.na(Q211)) %>% filter(!is.na(Q212)) %>% 
  filter(Q262>=16 & Q262<=24) %>%
  mutate(variable=case_when(Q209==1 | Q210==1 | Q211==1 | Q212==1  ~ 1, TRUE ~ 0)) %>%
  group_by(variable, B_COUNTRY) %>% 
  summarise( N_weight = sum(W_WEIGHT)) %>%
  ungroup() %>% group_by(B_COUNTRY) %>%
  mutate(Total=sum(N_weight)) %>% 
  mutate(prop=N_weight/Total) %>%
  filter(variable==1) %>%
  select(B_COUNTRY, prop) %>%
  rename(ISONum=B_COUNTRY, pol_part=prop) %>% 
  mutate(Round=2017)

pol_part <- bind_rows(pol_part10, pol_part17) %>% group_by(ISONum) %>% mutate(max=max(Round)) %>% filter(max==Round) %>% select(-max)  %>% left_join(year, by=c("ISONum", "Round")) %>% select(-Round) %>% rename(pol_part_Year=Year)

#######################################################
setwd("C:/Users/KristinBietsch/files/UNFPA/Code for GitHub/Input data")
write.csv(national, "Vote_National.csv", na="", row.names = F)
write.csv(betteroff17, "BetterOff.csv", na="", row.names = F)
write.csv(humanrights, "humanrights.csv", na="", row.names = F)
write.csv(active_member, "active_member.csv", na="", row.names = F)
write.csv(pol_part, "pol_part.csv", na="", row.names = F)
