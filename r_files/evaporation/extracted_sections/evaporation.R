## ----tidy-evaporation-effects-on-biomass-------------------------------------------------------------------------------

#Columns: exchange & evaporation
ds_for_evaporation = gather(ds_for_evaporation, 
                            key = exchange, 
                            value = evaporation, 
                            water_add_after_t2:water_add_after_t6)
ds_for_evaporation[ds_for_evaporation == "water_add_after_t2"] = "2"
ds_for_evaporation[ds_for_evaporation == "water_add_after_t3"] = "3"
ds_for_evaporation[ds_for_evaporation == "water_add_after_t4"] = "4"
ds_for_evaporation[ds_for_evaporation == "water_add_after_t5"] = "5"
ds_for_evaporation[ds_for_evaporation == "water_add_after_t6"] = "6"
ds_for_evaporation$evaporation[ds_for_evaporation$exchange == 2] = ds_for_evaporation$evaporation[ds_for_evaporation$exchange == 2] / 2 #This is because exchange contained the topping up of two exchanges
ds_for_evaporation$evaporation[ds_for_evaporation$exchange == 2] = ds_for_evaporation$evaporation[ds_for_evaporation$exchange == 2] + 2 #We need to add 2 ml to the evaporation that happened at the exchange events 1 and 2. This is because we already added 1 ml of water at exchange 1 and 1 ml of water at exchange 2. 

#Column: nr_of_tubes_in_rack
ds_for_evaporation$nr_of_tubes_in_rack[ds_for_evaporation$exchange == 1] = 15
ds_for_evaporation$nr_of_tubes_in_rack[ds_for_evaporation$exchange == 2] = 15
ds_for_evaporation$nr_of_tubes_in_rack[ds_for_evaporation$exchange == 3] = 15
ds_for_evaporation$nr_of_tubes_in_rack[ds_for_evaporation$exchange == 4] = 4
ds_for_evaporation$nr_of_tubes_in_rack[ds_for_evaporation$exchange == 5] = 4
ds_for_evaporation$nr_of_tubes_in_rack[ds_for_evaporation$exchange == 6] = 4


## ----plot, warning = FALSE---------------------------------------------------------------------------------------------
ds_for_evaporation %>%
  filter(disturbance == disturbance) %>%
  ggplot(aes(x = as.character(nr_of_tubes_in_rack),
             y = evaporation)) + 
  geom_boxplot() + 
  labs(x = "Number of tubes in rack", 
       y = "Evaporation (ml)")

ds_for_evaporation %>%
  filter(disturbance == disturbance) %>%
  ggplot(aes(x = as.character(patch_size),
             y = evaporation)) + 
  geom_boxplot() + 
  labs(x = "Patch size", 
       y = "Evaporation (ml)")

ds_for_evaporation %>%
  filter(disturbance == disturbance) %>%
  ggplot(aes(x = as.character(day),
             y = evaporation)) + 
  geom_boxplot() + 
  labs(x = "Day", 
       y = "Evaporation (ml)")

ds_for_evaporation %>%
  filter(disturbance == disturbance) %>%
  ggplot(aes(x = disturbance,
             y = evaporation)) + 
  geom_boxplot() + 
  labs(x = "Disturbance", 
       y = "Evaporation (ml)")


## ----mixed model, eval = FALSE-----------------------------------------------------------------------------------------
## 
## mixed.model = lmer(evaporation  ~
##                      patch_size * disturbance  * exchange +
##                      (exchange | culture_ID),
##                    data = ds_for_evaporation,
##                    REML = FALSE,
##                    control = lmerControl (optimizer = "Nelder_Mead"))
## 
## null.model = lm(evaporation ~
##                   1,
##                 data = ds_for_evaporation)
## 
## anova(mixed.model, null.model)

