## ---- class.source = 'fold-hide'---------------------------------------------------------------------------------------------------------
evaporation.test = read.csv(here("data", "evaporation_test","evaporation_test_right.csv"), header = TRUE)

evaporation.test %>%
  ggplot(aes (x = as.character(water_pipetted),
                y = weight_water_evaporated,
                group = interaction(water_pipetted, as.character(rack)),
                fill = as.character(rack))) +
  geom_boxplot() +
  labs(x = "Water volume (ml)" , 
       y = "Evaporation (g)", 
       fill = "Rack replicate")


## ---- class.source = 'fold-hide'---------------------------------------------------------------------------------------------------------
evaporation.test = read.csv(here("data", "evaporation_test", "evaporation_test_fill_nofill.csv"), header = TRUE)

evaporation.test %>%
  ggplot(aes (x = all_tubes_water,
              y = weight_water_evaporated)) +
  geom_boxplot() +
  labs(x = "Water in the other 10 tubes" , 
  y = "Evaporation (g)", 
  caption = "When all tubes were filled, they were filled with 6.75 ml of deionised water.")

