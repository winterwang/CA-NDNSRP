##%######################################################%##
#                                                          #
####            load the data set prepared              ####
#               2019-09-07                                 #
##%######################################################%##

load("Food1_9_teen.Rdata")
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

Food1_9_teen <- Food1_9_teen %>% 
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

Food1_9_teen <- Food1_9_teen %>% 
  rowwise() %>% 
  mutate(fruitadjg  = Fruitg + 2*(DriedFruitg + Tomatoesg), 
         totaladjg  = TotalGrams + DriedFruitg + TomatoPureeg) %>% 
  mutate(FVN_pc     = 100*(fruitadjg + Tomatoesg + YellowRedGreeng + 
                             Brassicaceaeg + Beansg + Nutsg + OtherVegg)/totaladjg) %>% 
  mutate(C_FrVegNut = if_else(FVN_pc <= 40, 0, 
                              if_else((FVN_pc > 40) & (FVN_pc <= 60), 1, 
                                      if_else((FVN_pc > 60) & (FVN_pc <= 80), 2, 
                                              if_else(FVN_pc > 80, 5, 999)))))


Food1_9_teen <- Food1_9_teen %>% 
  rowwise() %>% 
  mutate(C_fibre   = min(5, round(Englystfibreg*100/totaladjg/0.7)), 
         C_protein = min(5, round(Proteing*100/totaladjg/1.6))) 


Food1_9_teen$C_protein[(Food1_9_teen$A_points > 10) & (Food1_9_teen$C_FrVegNut < 5)] <- 0

Food1_9_teen <- Food1_9_teen %>% 
  mutate(C_points = C_protein + C_FrVegNut + C_fibre) %>% 
  mutate(H_points = A_points - C_points)


save(Food1_9_teen, file = "Food1_9_teen_Hpoints.Rdata")



##%######################################################%##
#                                                          #
####           end of Health H points coding            ####
#                                                          #
##%######################################################%##



load("Food1_9_teen_Hpoints.Rdata")
library(tidyverse)

Food1_9_teen <- Food1_9_teen %>% 
  mutate(MealTimeSlot = factor(MealTimeDescription, 
                               levels = c("6am to 8:59am", 
                                          "9am to 11:59am",
                                          "12 noon to 1:59pm",
                                          "2pm to 4:59pm",
                                          "5pm to 7:59pm",
                                          "8pm to 9:59pm",
                                          "10pm to 5:59am"))) %>% 
  mutate(DayW = factor(DayofWeek, 
                       levels = c("Monday", "Tuesday",
                                  "Wednesday", "Thursday", 
                                  "Friday", "Saturday", 
                                  "Sunday")))


Food1_9_teen %>% 
  ungroup() %>% 
  group_by(Where) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  mutate(rel.freq = paste0(round(100 * n/sum(n), 3), "%"))  %>% 
  mutate(relfreq = n/sum(n)) %>% 
  mutate(cumprop = paste0(round(100 * cumsum(relfreq), 3), "%")) %>% 
  select(-relfreq) %>% 
  print(n=Inf)





TableFoogGroup <- Food1_9_teen %>% 
  ungroup() %>% 
  group_by(mfgLab) %>% 
  summarise(n = n(), meanHpoint = mean(H_points, na.rm = T), mfgCalories = sum(Energykcal)) %>% 
  arrange(-mfgCalories) %>% 
  mutate(n.freq = paste0(round(100 * n/sum(n), 2), "%"))  %>% 
  mutate(cal.Prop = paste0(round(100 * mfgCalories/sum(mfgCalories), 2), "%"))  %>% 
  mutate(calprop = mfgCalories/sum(mfgCalories)) %>% 
  mutate(calcumprop = paste0(round(100 * cumsum(calprop), 3), "%")) %>% 
  print(n=Inf)




Food1_9_teen %>% 
  ungroup() %>% 
  group_by(MealTimeSlot) %>% 
  summarise(n = n()) %>% 
  mutate(rel.freq = paste0(round(100 * n/sum(n), 2), "%"))  %>% 
  print(n=Inf)



Food1_9_teen <- Food1_9_teen %>% 
  mutate(Locat = if_else(grepl("Home", Where, fixed = TRUE), "Home", 
                         if_else(grepl("School", Where), "School", 
                                 if_else(grepl("Work", Where), "Work", 
                                         if_else(grepl("Friend", Where), "Friend", 
                                                 if_else(grepl("Bus|Street", Where), "Move", 
                                                         if_else(grepl("Coffee|Community|Rest|Sport|Public|Place|Leis|Fast|Holiday", Where),
                                                                 "Leis", "Other")))))))


Food1_9_teen %>% 
  ungroup() %>% 
  group_by(Locat) %>% 
  summarise(n = n()) %>% 
  mutate(rel.freq = paste0(round(100 * n/sum(n), 2), "%"))  %>% 
  print(n=Inf)


Food1_9_teen$Locat_type <- fct_collapse(Food1_9_teen$Locat, 
                                        Home = c("Home"), 
                                        School_work = c("School", "Work"), 
                                        Others = c("Friend", "Leis", "Move", "Other"))
fct_count(Food1_9_teen$Locat_type)



# Define labels for main food groups

Food1_9_teen %>% 
  ungroup() %>% 
  group_by(MainFoodGroupCode) %>% 
  summarise(n = n()) %>% 
  mutate(rel.freq = paste0(round(100 * n/sum(n), 2), "%"))  %>% 
  print(n=Inf)

Food1_9_teen <- Food1_9_teen %>% 
  mutate(mfgLab = factor(MainFoodGroupCode)) %>% 
  mutate(mfgLab = fct_recode(mfgLab, 
                             "Pasta & Rice" 　= "1",
                             "White Bread"    = "2",
                             "WMeal Bread"    = "3",
                             "Oth Bread"      = "4",
                             "HiFi cereals"   = "5",
                             "LoFi cereals"   = "6",
                             "Biscuits"       = "7",
                             "Cakes & Pastries" = "8",
                             "Puddings"        = "9",  
                             "Whole Milk"      = "10",
                             "2% milk"         = "11",
                             "Skimmed Milk"    = "12",
                             "Other Milk Cream"= "13",
                             "Cheese"          = "14",
                             "Yogurt"          = "15",
                             "Eggs"            = "16",
                             "Butter"          = "17",
                             "Polyunsatu margarine" = "18",
                             "LowFat Spreads"   = "19",
                             "Margarine"        = "20",
                             "Spreads less-fat" = "21",
                             "Bacon and ham"    = "22",
                             "Beef"             = "23",
                             "Lamb"             = "24",
                             "Pork"             = "25",
                             "Coated Chicken"   = "26",
                             "Chicken/turkey"   = "27",
                             "Liver"            = "28",
                             "Burgers/kebabs"   = "29",
                             "Sausages"         = "30",
                             "Meat pastries"    = "31",
                             "Other meat"       = "32",
                             "White fish, shellfish" = "33", 
                             "Other white fish"     = "34",
                             "Oily fish"            = "35",
                             "Salad and raw veg"    = "36",
                             "Veg not raw"          = "37",
                             "Chips"                = "38",
                             "Potatos other"        = "39",
                             "Fruit"                = "40",
                             "JamsSpreads"          = "41",
                             "Crisps"               = "42",
                             "Sugar confectionery"  = "43",
                             "Chocolate"            = "44",
                             "Fruit juice"          = "45",
                             "Spirits and liqueurs" = "47",
                             "Wine"                 = "48",
                             "Beer lager"           = "49",
                             "Misc./Vending"        = "50",
                             "Tea/Coffee/Water"     = "51",
                             "Commercial toddlers foods" = "52",
                             "Ice cream"            = "53",
                             "Dietary supplements"  = "54",
                             "Artificial Sweeteners"= "55",
                             "Nuts and seeds"       = "56",
                             "Reg soft drinks"      = "57",
                             "Diet soft drinks"     = "58",
                             "Brown Bread"          = "59",
                             "1% milk"              = "60",
                             "Smoothies"            = "61"))




TableFoogGroup <- Food1_9_teen %>% 
  ungroup() %>% 
  group_by(mfgLab) %>% 
  summarise(n = n(), meanHpoint = mean(H_points, na.rm = T), mfgCalories = sum(Energykcal)) %>% 
  arrange(-mfgCalories) %>% 
  mutate(n.freq = paste0(round(100 * n/sum(n), 2), "%"))  %>% 
  mutate(cal.Prop = paste0(round(100 * mfgCalories/sum(mfgCalories), 2), "%"))  %>% 
  mutate(calprop = mfgCalories/sum(mfgCalories)) %>% 
  mutate(calcumprop = paste0(round(100 * cumsum(calprop), 3), "%")) %>% 
  print(n=Inf)

save(Food1_9_teen, file = "Food1_9_teen_Hpoints.Rdata")


##%######################################################%##
#                                                          #
####         extrate names of P50 and P80 food          ####
####              groups contributing the               ####
####           majority of calories consumed            ####
#                                                          #
##%######################################################%##


P50foodnames <- TableFoogGroup$mfgLab[cumsum(TableFoogGroup$calprop) < 0.51]
P80foodnames <- TableFoogGroup$mfgLab[cumsum(TableFoogGroup$calprop) < 0.81]


##%######################################################%##
#                                                          #
####       healthy unhealth food group definition       ####
#                                                          #
##%######################################################%##



TableFoogGroup <- TableFoogGroup %>% 
  mutate(healthy     = meanHpoint < -2, 
         lesshealthy = meanHpoint > 4, 
         neutral     = (meanHpoint <= 4) & (meanHpoint >= -2))

TableFoogGroup %>% 
  filter(healthy)


HealthyName <- TableFoogGroup$mfgLab[TableFoogGroup$healthy]

TableFoogGroup %>% 
  filter(neutral)

NeutralName <- TableFoogGroup$mfgLab[TableFoogGroup$neutral]

TableFoogGroup %>% 
  filter(lesshealthy)

LessHealthyName <- TableFoogGroup$mfgLab[TableFoogGroup$lesshealthy]



Food1_9_teen <- Food1_9_teen %>% 
  mutate(P50 = if_else(mfgLab %in% P50foodnames, TRUE, FALSE), 
         P80 = if_else(mfgLab %in% P80foodnames, TRUE, FALSE), 
         HealthFoodGr = if_else(mfgLab %in% HealthyName, "Healthy", 
                                if_else(mfgLab %in% NeutralName, "Neutral", 
                                        if_else(mfgLab %in% LessHealthyName, 
                                                "LessHealthy", "None"))))



##%######################################################%##
#                                                          #
####         Now split the food diaries in two          ####
####   datasets (just by food recordings) # HFood is    ####
####   allfood with HT=0 for hypothesis generation #    ####
#### TFood is allfood with HT=1 for hypothesis testing  ####
#                                                          #
##%######################################################%##



set.seed(1701745)
Food1_9_teen$rand <-  runif(dim(Food1_9_teen)[1], min = 0, max = 1)
Food1_9_teen$HT <- Food1_9_teen$rand > 0.5


H_teenFood <- Food1_9_teen %>% 
  filter(!HT)

T_teenFood <- Food1_9_teen %>% 
  filter(HT)

save(H_teenFood, file = "H_teenFood.Rdata")

save(T_teenFood, file = "T_teenFood.Rdata")



##%######################################################%##
#                                                          #
####   Use HFood to perform Correspondence analysis:    ####
#               2019-09-07                                 #
##%######################################################%##



# n of food records by day of week 
ggplot(H_teenFood, aes(x=factor(DayofWeek)))+
  geom_bar(fill="steelblue")+
  theme_minimal()

##%######################################################%##
#                                                          #
####                   Healthy foods                    ####
#                                                          #
##%######################################################%##



with(H_teenFood[H_teenFood$HealthFoodGr == "Healthy", ], epiDisplay::tabpct(mfgLab, Locat_type))
with(H_teenFood[H_teenFood$HealthFoodGr == "Healthy", ], epiDisplay::tabpct(mfgLab, MealTimeDescription))

freqtab <- xtabs(~H_teenFood$mfgLab + H_teenFood$Locat_type)
freqtab



library(FactoMineR)
library(factoextra)
library(gplots)



chisq <- chisq.test(freqtab)
chisq
