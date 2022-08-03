## ----environment set-up, message=FALSE, echo=FALSE-----------------------------------------------------------------------------------------------------------

rm( list = ls(  ) )
cat( "\014" )
library("tidyverse")
library("gridExtra")
library("lme4")
library("here")



## ----import--------------------------------------------------------------------------------------------------------------------------------------------------

ds = read.csv(here("data", "PatchSizePilot_culture_info.csv"), header = TRUE)

head(ds)



## ----tidy----------------------------------------------------------------------------------------------------------------------------------------------------

ds = gather(ds, exchange, evaporation, water_add_after_t2:water_add_after_t6)
ds[ds == "water_add_after_t2"] = "2"
ds[ds == "water_add_after_t3"] = "3"
ds[ds == "water_add_after_t4"] = "4"
ds[ds == "water_add_after_t5"] = "5"
ds[ds == "water_add_after_t6"] = "6"


#Divide by two the evaporation after the second resource exchange, as we retopped the water that evaporated also from exchange number 1. 
ds$evaporation[ds$exchange == 2] = ds$evaporation[ds$exchange == 2] / 2

#Add how many tubes were in each rack
ds$nr_of_tubes_in_rack[ds$exchange == 1] = 15
ds$nr_of_tubes_in_rack[ds$exchange == 2] = 15
ds$nr_of_tubes_in_rack[ds$exchange == 3] = 15
ds$nr_of_tubes_in_rack[ds$exchange == 4] = 4
ds$nr_of_tubes_in_rack[ds$exchange == 5] = 4
ds$nr_of_tubes_in_rack[ds$exchange == 6] = 4

#We need to add 2 ml to the evaporation that happened at the exchange events 1 and 2. This is because we already added 1 ml of water at exchange 1 and 1 ml of water at exchange 2. 
ds$evaporation[ds$exchange == 2] = ds$evaporation[ds$exchange == 2] + 2

#We need to take off tubes 106-110 at the second excahnge. This is because we boiled them more and then refilled them with 3.15 ml.



## ----plot----------------------------------------------------------------------------------------------------------------------------------------------------

ds %>%
  ggplot(aes(x = exchange,
             y = evaporation)) +
  geom_boxplot() + 
  labs(x = "Exchange number", y = "Evaporation (ml)")

ds %>%
  ggplot(aes(x = as.character(nr_of_tubes_in_rack),
             y = evaporation)) + 
  geom_boxplot() +
  labs(x = "nr of tubes microwaved in a rack", y = "evaporation (ml)")

ds %>%
  ggplot(aes(x = exchange,
             y = evaporation,
             fill = patch_size,
             color = patch_size)) +
  geom_boxplot() + 
  labs(x = "Exchange number", y = "Evaporation (ml)", fill = "Patch size", color = "Patch size")

ds %>%
  ggplot(aes(x = exchange,
             y = evaporation,
             fill = disturbance,
             color = disturbance)) +
  geom_boxplot() +
  labs(x = "Exchange number", y = "Evaporation (ml)", fill = "Disturbance", color = "Disturbance")

ds %>%
  ggplot(aes(x = patch_size,
             y = evaporation,
             fill = disturbance,
             color = disturbance)) +
  geom_boxplot() +
  labs(x = "Patch size", y = "Evaporation (ml)", fill = "Disturbance", color = "Disturbance")



## ----mixed model, eval = FALSE-------------------------------------------------------------------------------------------------------------------------------
## 
## mixed.model = lmer(evaporation  ~ exchange + patch_size + disturbance  + exchange*patch_size + exchange*disturbance + patch_size*disturbance + exchange*patch_size*disturbance +
##                      (exchange||id) + (patch_size||id) + (disturbance||id) + (exchange*patch_size||id) + (exchange*disturbance||id) + (patch_size*disturbance||id) +
##                      (exchange*patch_size*disturbance||id), data=ds)
## 


## ----difference between rack treatments----------------------------------------------------------------------------------------------------------------------

evaporation_exchange_1_2 = ds %>%
  filter(!is.na(evaporation), exchange == 2) %>%
  summarise(mean_evaporation = mean(evaporation), sd_evaporation = sd(evaporation))
mean_evaporation_exchange_1_2 = round(evaporation_exchange_1_2$mean_evaporation, digits = 2)
sd_evaporation_exchange_1_2 = round(evaporation_exchange_1_2$sd_evaporation, digits = 2)





mean_evaporation = ds %>%
  filter(!is.na(evaporation)) %>%
  group_by(nr_of_tubes_in_rack) %>%
  summarise(mean_evaporation = mean(evaporation), sd_evaporation = sd(evaporation))

mean_evaporation_15 = round(mean_evaporation$mean_evaporation[mean_evaporation$nr_of_tubes_in_rack == 15], digits = 2)
mean_evaporation_4 = round(mean_evaporation$mean_evaporation[mean_evaporation$nr_of_tubes_in_rack == 4], digits = 2)
sd_evaporation_15 = round(mean_evaporation$sd_evaporation[mean_evaporation$nr_of_tubes_in_rack == 15], digits = 2)
sd_evaporation_4 = round(mean_evaporation$sd_evaporation[mean_evaporation$nr_of_tubes_in_rack == 4], digits = 2)



