##%######################################################%##
# Date 2019-07-24                                          #
####              Import the data we need               ####
#                                                          #
##%######################################################%##

library(haven)

path <- c("~/pCloud Drive/LSHTM/study/project/6533stata_NDNSRP20190719/UKDA-6533-stata/stata/stata11_se/")

ndns_rp_yr1_4a_indiv_uk <- read_dta(paste(path, "ndns_rp_yr1-4a_indiv_uk.dta", sep = ""))
ndns_rp_yr5_6a_indiv_uk <- read_dta(paste(path, "ndns_rp_yr5-6a_indiv.dta", sep = ""))
ndns_rp_yr7_8a_indiv_uk <- read_dta(paste(path, "ndns_rp_yr7-8a_indiv.dta", sep = ""))
ndns_rp_yr9a_indiv_uk <- read_dta(paste(path, "ndns_rp_yr9a_indiv.dta", sep = ""))

##%######################################################%##
#                                                          #
####         keep variables needed for nutrient         ####
####            profile calc and CA anlysis             ####
#                                                          #
##%######################################################%##
library(plyr)
library(tidyverse)
library(dplyr)
library(readr)
library(naniar)


indiv_yr1_4 <- ndns_rp_yr1_4a_indiv_uk %>% 
  select(seriali, serialh, cluster, addnum, area, nssec8, wti_CY1234, 
         IOut, Sex, age, agegr1, agegr2, DiaryD, quarter4, month, 
         HRPNo, AdChild, bmival, wstval, Diabetes, Glucose, A1C, cigsta3,
         dnoft3, dnnow, ethgr5, ethgr2, gor, qual7, Outcome) %>% 
  mutate(Years = "1-4", 
         cluster2 = NA, cluster3 = NA, 
         cluster4 = NA, cluster5 = NA) %>% 
  rename(cluster1 = cluster, wti = wti_CY1234, 
         Month = month) %>% 
  replace_with_na(replace = list(bmival = -1, wstval = -1, 
                                 qual7 = -8, dnnow = -1, 
                                 Glucose = -1, A1C = -1,
                                 cigsta3 = -1, nssec8 = -8, 
                                 ethgr5 = -4, ethgr2 = -4, 
                                 Diabetes = -1, dnoft3 = -9)) %>% 
  replace_with_na(replace = list(nssec8 = -1, qual7 = -1, 
                                 dnoft3 = -8, dnnow = -8)) %>% 
  replace_with_na(replace = list(nssec8 = 99, dnnow = -9,
                                 dnoft3 = -4)) %>% 
  replace_with_na(replace = list(dnoft3 = -1, dnnow = -4))
  



indiv_yr5_6 <- ndns_rp_yr5_6a_indiv_uk %>% 
  select(seriali, serialh, cluster1, cluster2, cluster3, cluster4, cluster5,
         addnum, area, nssec8, wti_Y56,
         IOut, Sex, age, agegr1, agegr2, DiaryD, Quarter, Month, 
         HRPNo, AdChild, bmival, wstval, Diabetes, Glucose, A1C, cigsta3,
         dnoft3, dnnow, ethgrp5, ethgrp2, gor, qual7, Outcome) %>% 
  mutate(Years = "5-6") %>% 
  rename(wti = wti_Y56, quarter4 = Quarter,
         ethgr5 = ethgrp5, ethgr2 = ethgrp2) %>% 
  replace_with_na(replace = list(bmival = -1, wstval = -1, 
                                 qual7 = -8, dnnow = -1, 
                                 Glucose = -1, A1C = -1,
                                 cigsta3 = -9, nssec8 = -8, 
                                 ethgr5 = -4, ethgr2 = -4, 
                                 Diabetes = -1, dnoft3 = -9)) %>% 
  replace_with_na(replace = list(nssec8 = -1, qual7 = -1, bmival = -8,
                                 dnoft3 = -8, dnnow = -8, wstval = -8, 
                                 Diabetes = -9, cigsta3 = -8)) %>% 
  replace_with_na(replace = list(nssec8 = 99, dnnow = -9, bmival = -4,
                                 dnoft3 = -4, wstval = -4, Diabetes = -8,
                                 cigsta3 = -4)) %>% 
  replace_with_na(replace = list(dnoft3 = -1, dnnow = -4, cigsta3 = -1,
                                 bmival = -9, wstval = -9, Diabetes = -2)) %>% 
  replace_with_na(replace = list(Diabetes = -4))

indiv_yr7_8 <- ndns_rp_yr7_8a_indiv_uk %>% 
  select(c(seriali, serialh, cluster1, cluster2, cluster3, cluster4, cluster5,
           addnum, area, nssec8, wti_Y78, 
           IOut, Sex, age, agegr1, agegr2, DiaryD, Quarter, Month, 
           HRPNo, AdChild, bmival, wstval, Diabetes, Glucose, A1C, cigsta3,
           dnoft3, dnnow, ethgrp5, ethgrp2, gor, qual7, Outcome)) %>% 
  mutate(Years = "7-8") %>% 
  rename(wti = wti_Y78, quarter4 = Quarter,
         ethgr5 = ethgrp5, ethgr2 = ethgrp2) %>% 
  replace_with_na(replace = list(bmival = -1, wstval = -1, 
                                 qual7 = -8, dnnow = -1, 
                                 Glucose = -1, A1C = -1,
                                 cigsta3 = -9, nssec8 = -8, 
                                 ethgr5 = -4, ethgr2 = -4, 
                                 Diabetes = -1, dnoft3 = -9)) %>% 
  replace_with_na(replace = list(nssec8 = -1, qual7 = -1, bmival = -8,
                                 dnoft3 = -8, dnnow = -8, wstval = -8, 
                                 Diabetes = -9, cigsta3 = -8)) %>% 
  replace_with_na(replace = list(nssec8 = 99, dnnow = -9, bmival = -4,
                                 dnoft3 = -4, wstval = -4, Diabetes = -8,
                                 cigsta3 = -4)) %>% 
  replace_with_na(replace = list(dnoft3 = -1, dnnow = -4, cigsta3 = -1,
                                 bmival = -9, wstval = -9, Diabetes = -2)) %>% 
  replace_with_na(replace = list(Diabetes = -4))


indiv_yr9 <- ndns_rp_yr9a_indiv_uk %>% 
  select(c(seriali, serialh, cluster1, cluster2, cluster3, cluster4, cluster5,
           addnum, area, nssec8, wti_Y9, 
           IOut, Sex, age, agegr1, agegr2, DiaryD, Quarter, month, 
           HRPNo, AdChild, bmival, wstval, Diabetes, Glucose, A1C, cigsta3,
           dnoft3, dnnow, ethgrp5, ethgrp2, GOR, qual7, Outcome)) %>% 
  mutate(Years = "9") %>% 
  rename(wti = wti_Y9, quarter4 = Quarter,
         ethgr5 = ethgrp5, ethgr2 = ethgrp2, 
         gor = GOR, Month = month) %>% 
  replace_with_na(replace = list(bmival = -1, wstval = -1, 
                                 qual7 = -8, dnnow = -1, 
                                 Glucose = -1, A1C = -1,
                                 cigsta3 = -9, nssec8 = -8, 
                                 ethgr5 = -4, ethgr2 = -4, 
                                 Diabetes = -1, dnoft3 = -9)) %>% 
  replace_with_na(replace = list(nssec8 = -1, qual7 = -1, bmival = -8,
                                 dnoft3 = -8, dnnow = -8, wstval = -8, 
                                 Diabetes = -9, cigsta3 = -8)) %>% 
  replace_with_na(replace = list(nssec8 = 99, dnnow = -9, bmival = -4,
                                 dnoft3 = -4, wstval = -4, Diabetes = -8,
                                 cigsta3 = -4)) %>% 
  replace_with_na(replace = list(dnoft3 = -1, dnnow = -4, cigsta3 = -1,
                                 bmival = -9, wstval = -9, Diabetes = -2)) %>% 
  replace_with_na(replace = list(Diabetes = -4))

##%######################################################%##
#                                                          #
####            merge individual data sets              ####
#                                                          #
##%######################################################%##

  
Indiv1_9 <- bind_rows(indiv_yr1_4, indiv_yr5_6, indiv_yr7_8, indiv_yr9)  
  
  
##%######################################################%##
#                                                          #
####     reduce records to those we need age >= 19      ####
#                                                          #
##%######################################################%##


Indiv1_9_adlt <- Indiv1_9 %>% 
  filter(age >= 19)

Indiv1_9_teen <- Indiv1_9 %>% 
  filter(age < 19 & age >= 11)


save(Indiv1_9_adlt, file = "Indiv1_9_adlt.Rdata")
save(Indiv1_9_teen, file = "Indiv1_9_teen.Rdata")

epiDisplay::tab1(Indiv1_9$age)
epiDisplay::tab1(Indiv1_9_adlt$age)
epiDisplay::tab1(Indiv1_9_teen$age)

rm(ndns_rp_yr1_4a_indiv_uk)
rm(ndns_rp_yr5_6a_indiv_uk)
rm(ndns_rp_yr9a_indiv_uk)
rm(ndns_rp_yr7_8a_indiv_uk)



##%######################################################%##
#                                                          #
####    import and manipulate food level diary data     ####
#   2019/08/01 updated                                     #
##%######################################################%##
 





ndns_rp_yr1_4food <- read_dta(paste(path, "ndns_rp_yr1-4a_foodleveldietarydata_uk_v2.dta", sep = ""))

FoodGrp1_4 <- ndns_rp_yr1_4food %>% 
  select(-matches("Pota|Calci|Manges|Phos|Iron|Haemiron|Nonhaemiron|Copper|Zinc")) %>% 
  select(-matches("Chloride|Retino|Totalcarote|Alphacaro|Betacaro|Betacry|Vitamin")) %>% 
  select(-matches("Thiam|Ribofla|Niacin|Trytopha|Folat|Pantothe|Biotin|Totalnit")) %>% 
  select(-matches("Nitrogen|Manga|Iodin|Seleniu|Chol|Othersugar|Glucose")) %>% 
  select(-matches("Fructose|Sucr|Maltose|Lactose|Nonmilk|Intrin|Beef|Lamb")) %>%  
  select(-matches("Pork|Processed|Otherred|Burger|Sausages|Offal|Poultry")) %>% 
  select(-matches("Processedpou|Gamebird|Whitefish|oilyfish|Canned|Shellfish")) %>% 
  select(-matches("Cottageche|Cheddarch|Otherchees|fattyacid|variab")) 
  
ndns_rp_yr5_6food <- read_dta(paste(path, "ndns_rp_yr5-6a_foodleveldietarydata_v2.dta", sep = ""))

FoodGrp5_6 <- ndns_rp_yr5_6food %>% 
  select(-matches("Pota|Calci|Manges|Phos|Iron|Haemiron|Nonhaemiron|Copper|Zinc")) %>% 
  select(-matches("Chloride|Retino|Totalcarote|Alphacaro|Betacaro|Betacry|Vitamin")) %>% 
  select(-matches("Thiam|Ribofla|Niacin|Trytopha|Folat|Pantothe|Biotin|Totalnit")) %>% 
  select(-matches("Nitrogen|Manga|Iodin|Seleniu|Chol|Othersugar|Glucose")) %>% 
  select(-matches("Fructose|Sucr|Maltose|Lactose|Nonmilk|Intrin|Beef|Lamb")) %>%  
  select(-matches("Pork|Processed|Otherred|Burger|Sausages|Offal|Poultry")) %>% 
  select(-matches("Processedpou|Gamebird|Whitefish|oilyfish|Canned|Shellfish")) %>% 
  select(-matches("Cottageche|Cheddarch|Otherchees|fattyacid|variab")) 

                  
ndns_rp_yr7_8food <- read_dta(paste(path, "ndns_rp_yr7-8a_foodleveldietarydata.dta", sep = ""))

FoodGrp7_8 <- ndns_rp_yr7_8food %>% 
  select(-matches("Pota|Calci|Manges|Phos|Iron|Haemiron|Nonhaemiron|Copper|Zinc")) %>% 
  select(-matches("Chloride|Retino|Totalcarote|Alphacaro|Betacaro|Betacry|Vitamin")) %>% 
  select(-matches("Thiam|Ribofla|Niacin|Trytopha|Folat|Pantothe|Biotin|Totalnit")) %>% 
  select(-matches("Nitrogen|Manga|Iodin|Seleniu|Chol|Othersugar|Glucose")) %>% 
  select(-matches("Fructose|Sucr|Maltose|Lactose|Nonmilk|Intrin|Beef|Lamb")) %>%  
  select(-matches("Pork|Processed|Otherred|Burger|Sausages|Offal|Poultry")) %>% 
  select(-matches("Processedpou|Gamebird|Whitefish|oilyfish|Canned|Shellfish")) %>% 
  select(-matches("Cottageche|Cheddarch|Otherchees|fattyacid|variab")) 


ndns_rp_yr9food <- read_dta(paste(path, "ndns_rp_yr9a_foodleveldietarydata.dta", sep = ""))

FoodGrp9 <- ndns_rp_yr9food %>% 
  select(-matches("Pota|Calci|Manges|Phos|Iron|Haemiron|Nonhaemiron|Copper|Zinc")) %>% 
  select(-matches("Chloride|Retino|Totalcarote|Alphacaro|Betacaro|Betacry|Vitamin")) %>% 
  select(-matches("Thiam|Ribofla|Niacin|Trytopha|Folat|Pantothe|Biotin|Totalnit")) %>% 
  select(-matches("Nitrogen|Manga|Iodin|Seleniu|Chol|Othersugar|Glucose")) %>% 
  select(-matches("Fructose|Sucr|Maltose|Lactose|Nonmilk|Intrin|Beef|Lamb")) %>%  
  select(-matches("Pork|Processed|Otherred|Burger|Sausages|Offal|Poultry")) %>% 
  select(-matches("Processedpou|Gamebird|Whitefish|oilyfish|Canned|Shellfish")) %>% 
  select(-matches("Cottageche|Cheddarch|Otherchees|fattyacid|variab")) 





##%######################################################%##
#                                                          #
####            merge food group data sets              ####
#                                                          #
##%######################################################%##


Food1_9 <- bind_rows(FoodGrp1_4, FoodGrp5_6, FoodGrp7_8, FoodGrp9)  



##%######################################################%##
#                                                          #
####     reduce records to those we need age >= 19      ####
#                                                          #
##%######################################################%##


Food1_9_adlt <- Food1_9 %>% 
  filter(Age >= 19)

Food1_9_teen <- Food1_9 %>% 
  filter(Age < 19 & Age >= 11)


save(Food1_9_adlt, file = "Food1_9_adlt.Rdata")
save(Food1_9_teen, file = "Food1_9_teen.Rdata")



