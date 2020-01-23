table(Indiv1_9_adlt$Years)
3450+1288+1417

table(Indiv1_9_adlt$Diabetes)
table(Indiv1_9_adlt$DM4cat)

Ind1_9_DM <- Indiv1_9_adlt %>% 
  filter(Years %in% c("1-4", "5-6", "7-8")) %>% 
  select(seriali, Diabetes, DM4cat) %>% 
  rename(ID = seriali)

haven::write_dta(Ind1_9_DM, "../LSHTMproject/Rcode/Ind1_9_DM.dta")
