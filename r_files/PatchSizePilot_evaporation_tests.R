library("tidyverse")
rm(list = ls())
cat( "\014" )

### --- MICROWAVING OF 15 TUBES FOR 3 MINUTES --- ##############

ds = read.csv("/Users/ema/github/PatchSizePilot/data/evaporation_test/evaporation_test_right.csv", header = TRUE)

ds$rack = as.character(ds$rack)
ds$water_pipetted = as.character(ds$water_pipetted)

ds %>%
  ggplot(aes (x = water_pipetted,
                y = weight_water_evaporated,
                group = interaction(water_pipetted, rack),
                fill = rack,
                color = rack)) +
  geom_boxplot() +
  labs(x = "Water volume (ml)" , y = "Evaporation (g)", fill = "Rack replicate", color = "Rack replicate")

### --- MICROWAVING OF 5 TUBES WITH OTHER 10 TUBES THAT WERE FILLED OR EMPTY --- ##############

ds = read.csv("/Users/ema/github/PatchSizePilot/data/evaporation_test/evaporation_test_fill_nofill.csv", header = TRUE)

ds$all_tubes_water[ds$all_tubes_water == "yes"] = "6.75"
ds$all_tubes_water[ds$all_tubes_water == "no"] = "0"


ds %>%
  ggplot(aes (x = all_tubes_water,
              y = weight_water_evaporated)) +
  geom_boxplot() +
  labs(x = "Water in the other tubes (ml)" , y = "Evaporation (g)", caption = "I'm not sure about the 6.75 ml.
       I need to check in my lab book.")

### --- HOW MUCH I TOPPED UP TUBES --- ##############

ds = read.csv("/Users/ema/github/PatchSizePilot/data/PatchSizePilot_dataset.csv", header = TRUE)

ds %>%
  ggplot(aes(x = ,
             y = )) +
  geom_boxplot()