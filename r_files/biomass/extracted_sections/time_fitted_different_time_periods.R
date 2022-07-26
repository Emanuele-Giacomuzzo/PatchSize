## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------

R2 = NULL

for (last_point in 3:7) {
  
  filtered_dataset = ds_regional_predicted_shrunk_type %>%
  filter(time_point <= last_point)
  
  full_model = lm(regional_mean_bioarea ~ 
                            disturbance +
                            metaecosystem_type,
                        data = filtered_dataset)
  
  no_metaeco_type_model = lm(regional_mean_bioarea ~ 
                            disturbance,
                        data = filtered_dataset)
  
  anova(full_model, no_metaeco_type_model)
  
  summary(full_model)
  summary(no_metaeco_type_model)
  R2[[last_point]] = summary(full_model)$adj.r.squared - summary(no_metaeco_type_model)$adj.r.squared
  R2[[last_point]] = round(R2[[last_point]], digits = 3)
  
}

