library(brms)
library(tidyverse)
library(naniar)

TFood <- TFood %>% 
  mutate(YesPudding = as.numeric(MainFoodGroupCode == 9)) %>% 
  mutate(Agecat = cut(Age, breaks = c(18, 41, 51, 61, 71, 81, 100), right = FALSE)) %>% 
  replace_with_na(replace = list(nssec8 = -9))

TFood <- TFood %>% 
  mutate(DM4cat = as.factor(DM4cat), 
         nssec8 = as.factor(nssec8)) 

dataForStan <- TFood %>% 
  select(seriali, DM4cat, Agecat, nssec8, Time3g, Sex, area) %>% 
  mutate(Sex = as.factor(Sex))

dataForStan <- fastDummies::dummy_cols(dataForStan, remove_first_dummy = TRUE)

fit <- brm(YesPudding ~ Time3g + DM4cat + Sex + Agecat + nssec8 + (1 | seriali), data = TFood,
           family = bernoulli("logit"))
summary(fit)

