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

library(tidyverse)
indiv_yr1_4 <- ndns_rp_yr1_4a_indiv_uk %>% 
  select(c("seriali", "serialh", "cluster", "addnum", "area", "nssec8", "wti_CY1234", 
           "iout", "Sex", "Age", "agegr1", "agegr2", "diaryd", "quarter4", "month", 
           "hrpno", "adchild"))

indiv_yr5_6 <- ndns_rp_yr5_6a_indiv_uk %>% 
  select(c("seriali", "serialh", "cluster", "addnum", "area", "nssec8", "wti_CY1234",
           "iout", "Sex", "Age", "agegr1", "agegr2", "diaryd", "quarter4", "month", 
           "hrpno", "adchild"))

indiv_yr7_8 <- ndns_rp_yr7_8a_indiv_uk %>% 
  select(c("seriali", "serialh", "cluster", "addnum", "area", "nssec8", "wti_CY1234", 
           "iout", "Sex", "Age", "agegr1", "agegr2", "diaryd", "quarter4", "month", 
           "hrpno", "adchild"))

indiv_yr1_4 <- ndns_rp_yr1_4a_indiv_uk %>% 
  select(c("seriali", "serialh", "cluster", "addnum", "area", "nssec8", "wti_CY1234"))
