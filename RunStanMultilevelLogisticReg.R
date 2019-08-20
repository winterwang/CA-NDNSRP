library(brms)
library(tidyverse)
library(naniar)
TFood <- TFood %>% 
  left_join(Indiv1_9_adlt[,c("seriali", "nssec8", "area")], by = "seriali")
names(TFood)[85] <- "DM4cat"
TFood <- TFood[, -82]
TFood <- TFood[, -c(6, 10, 11)]

TFood <- TFood %>% 
  mutate(YesPudding = as.numeric(MainFoodGroupCode == 9)) %>% 
  mutate(Agecat = cut(Age, breaks = c(18, 41, 51, 61, 71, 81, 100), right = FALSE)) %>% 
  replace_with_na(replace = list(nssec8 = -9))

TFood <- TFood %>% 
  mutate(DM4cat = as.factor(DM4cat), 
         nssec8 = as.factor(nssec8)) 

dataForStan <- TFood %>% 
  select(seriali, YesPudding, DM4cat, Agecat, nssec8, Time3g, Sex, area) %>% 
  mutate(Sex = as.factor(Sex))

dataForStan <- fastDummies::dummy_cols(dataForStan, select_columns = c("nssec8",
                                                                       "Agecat",
                                                                       "DM4cat",
                                                                       "Time3g"),
                                       remove_first_dummy = TRUE, ignore_na = TRUE)

dataForStan <- dataForStan %>% 
  mutate(Agecat_1 = `Agecat_[41,51)`,
         Agecat_2 = `Agecat_[51,61)`,
         Agecat_3 = `Agecat_[61,71)`, 
         Agecat_4 = `Agecat_[71,81)`,
         Agecat_5 = `Agecat_[81,100)`)

Indiv1_9_adlt <- Indiv1_9_adlt %>% 
  mutate(Agecat = cut(age, breaks = c(18, 41, 51, 61, 71, 81, 100), right = FALSE)) %>% 
  replace_with_na(replace = list(nssec8 = -9))

  
Indiv1_9_adlt <- fastDummies::dummy_cols(Indiv1_9_adlt, select_columns = c("DM4cat", 
                                                                           "nssec8", 
                                                                           "Agecat"),
                                         remove_first_dummy = TRUE, ignore_na = TRUE)
Indiv1_9_adlt <- Indiv1_9_adlt %>% 
  mutate(Agecat_1 = `Agecat_[41,51)`,
         Agecat_2 = `Agecat_[51,61)`,
         Agecat_3 = `Agecat_[61,71)`, 
         Agecat_4 = `Agecat_[71,81)`,
         Agecat_5 = `Agecat_[81,100)`)

length(unique(dataForStan$seriali))

Indiv1_9_adlt$ID <- seq.int(nrow(Indiv1_9_adlt))
dataForStan <- dataForStan %>% 
  left_join(Indiv1_9_adlt[, c("seriali", "ID")],  by = "seriali")

# use brm package
fit <- brm(YesPudding ~ Time3g + DM4cat + Sex + Agecat + nssec8 + (1 | seriali), data = TFood,
           family = bernoulli("logit"))
summary(fit)

# write my own stan program
library(rstan)

data <- list(N = 6802, I = 374344, DM4cat_1 = Indiv1_9_adlt$DM4cat_1, DM4cat_2 = Indiv1_9_adlt$DM4cat_2,
             DM4cat_3 = Indiv1_9_adlt$DM4cat_3, Agecat_1 = Indiv1_9_adlt$Agecat_1, 
             Agecat_2 = Indiv1_9_adlt$Agecat_2, Agecat_3 = Indiv1_9_adlt$Agecat_3, 
             Agecat_4 = Indiv1_9_adlt$Agecat_4, Agecat_5 = Indiv1_9_adlt$Agecat_5,
             nssec8_1 = Indiv1_9_adlt$nssec8_1, nssec8_2 = Indiv1_9_adlt$nssec8_2, 
             nssec8_3 = Indiv1_9_adlt$nssec8_3, nssec8_4 = Indiv1_9_adlt$nssec8_4, 
             nssec8_5 = Indiv1_9_adlt$nssec8_5, nssec8_6 = Indiv1_9_adlt$nssec8_6,
             nssec8_8 = Indiv1_9_adlt$nssec8_8, seriali = dataForStan$ID, 
             Time3g_Afternoon = dataForStan$Time3g_Afternoon, 
             Time3g_Evening = dataForStan$Time3g_Evening, 
             YesPudding = dataForStan$YesPudding)


fit0 <- stan(file = "runStanProgram/NDNSRP.stan", data = data, 
             pars = c("b", "b_P", "s_P", "q", "OR_Eve_vs_Morn", "OR_Eve_vs_After"), seed = 1234)
