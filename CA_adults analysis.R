##%######################################################%##
#                                                          #
####            load the data set prepared              ####
#               2019-08-06                                 #
##%######################################################%##

load("Food1_9_adlt.Rdata")
library(plyr)
library(tidyverse)
library(dplyr)
library(readr)
library(naniar)

##%######################################################%##
#                                                          #
####          calculate the health FSA-points           ####
####                of each diary record                ####
#                                                          #
##%######################################################%##


# 1. Work out total A points 
# A maximum of ten points can be awarded for each nutrients.
# total 'A' points = (points for energy) + (points for saturated fat) 
#                    + (points for sugars) + (points for sodium)

Food1_9_adlt <- Food1_9_adlt %>% 
  rowwise() %>% 
  mutate(A_energy = min(10, round(EnergykJ*100/TotalGrams/335)),
         A_satfat = min(10, round(Saturatedfattyacidsg*100/TotalGrams)), 
         A_sugar  = min(10, round(Totalsugarsg*100/TotalGrams/4.5)), 
         A_sodium = min(10, round(Sodiummg*100/TotalGrams/90))) %>% 
  mutate(A_points = A_energy + A_satfat + A_sugar + A_sodium)

# 2. Work out total C points
# A maximum of 5 points can be awarded for each nutrient/food component. 
# Total C points = F(ood)V(egetable)N(ut) [points for % fruit, vegetable & nut content] + 
# (points for fibre[either NSP(non-starch polysaccharides) or
# AOAC (association of analytical chemist)])  + (points for protein)

# if a food or drink scores 11 or more A points then it cannot score 
# points for protein, unless it also scores 5 points for fruit, vegetables and nuts. 
# Before the C points are calculated, we need to make an adjustment in weights when 
# we have dried fruits, dried and Purred Fruit and Vegetables. 

Food1_9_adlt <- Food1_9_adlt %>% 
  rowwise() %>% 
  mutate(fruitadjg  = Fruitg + 2*(DriedFruitg + Tomatoesg), 
         totaladjg  = TotalGrams + DriedFruitg + TomatoPureeg) %>% 
  mutate(FVN_pc     = 100*(fruitadjg + Tomatoesg + YellowRedGreeng + 
                     Brassicaceaeg + Beansg + Nutsg + OtherVegg)/totaladjg) %>% 
  mutate(C_FrVegNut = if_else(FVN_pc <= 40, 0, 
                        if_else((FVN_pc > 40) & (FVN_pc <= 60), 1, 
                          if_else((FVN_pc > 60) & (FVN_pc <= 80), 2, 
                           if_else(FVN_pc > 80, 5, 999)))))

Food1_9_adlt <- Food1_9_adlt %>% 
  rowwise() %>% 
  mutate(C_fibre   = min(5, round(Englystfibreg*100/totaladjg/0.7)), 
         C_protein = min(5, round(Proteing*100/totaladjg/1.6))) 

Food1_9_adlt$C_protein[(Food1_9_adlt$A_points > 10) & (Food1_9_adlt$C_FrVegNut < 5)] <- 0

Food1_9_adlt <- Food1_9_adlt %>% 
  mutate(C_points = C_protein + C_FrVegNut + C_fibre) %>% 
  mutate(H_points = A_points - C_points)


samp <- sample(1:dim(Food1_9_adlt)[1], 5000)
samp_dat <- Food1_9_adlt[samp, ]

haven::write_dta(samp_dat, path = "../../Downloads/Food1_9_adlt.dta", version = 14)

save(Food1_9_adlt, file = "Food1_9_adlt_Hpoints.Rdata")

##%######################################################%##
#                                                          #
####           end of Health H points coding            ####
#                                                          #
##%######################################################%##

load("Food1_9_adlt_Hpoints.Rdata")
library(tidyverse)
# new recode the location/time variable to an integer for CA

Food1_9_adlt %>% 
  group_by(Where) %>% 
  summarise(n = n()) %>% 
  mutate(rel.freq = paste0(round(100 * n/sum(n), 2), "%"))  %>% 
  print(n=Inf)


Food1_9_adlt %>% 
  group_by(MealTimeSlot) %>% 
  summarise(n = n()) %>% 
  mutate(rel.freq = paste0(round(100 * n/sum(n), 2), "%"))  %>% 
  print(n=Inf)



Food1_9_adlt$Time3g <-  fct_collapse(Food1_9_adlt$MealTimeSlot, 
                       Morning = c("6am to 8:59am", "9am to 11:59am"), 
                       Afternoon = c("12 noon to 1:59pm", "2pm to 4:59pm", 
                                     "5pm to 7:59pm"), 
                       Evening = c("8pm to 9:59pm", "10pm to 5:59am"))

fct_count(Food1_9_adlt$Time3g)


Food1_9_adlt <- Food1_9_adlt %>% 
  mutate(Locat = if_else(grepl("Home", Where, fixed = TRUE), "Home", 
                  if_else(grepl("School", Where), "School", 
                    if_else(grepl("Work", Where), "Work", 
                     if_else(grepl("Friend", Where), "Friend", 
                      if_else(grepl("Bus|Street", Where), "Move", 
                if_else(grepl("Coffee|Community|Rest|Sport|Public|Place|Leis|Fast|Holiday", Where),
                        "Leis", "Other")))))))

fct_count(Food1_9_adlt$Locat)
