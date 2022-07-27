## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------

R2 = NULL

for (last_point in 3:7) {
  
  filtered_dataset = ds_regional_shrunk_type_n_day %>%
  filter(time_point <= last_point)
  
  full_model = lm(regional_mean_bioarea ~ 
                            disturbance +
                            metaecosystem_type +
                            (1 | day),
                        data = filtered_dataset)
  
  no_metaeco_type_model = lm(regional_mean_bioarea ~ 
                            disturbance +
                            (1 | day),
                        data = filtered_dataset)
  
  anova(full_model, no_metaeco_type_model)
  
  R2_full = r.squaredGLMM(full_model)[1,1]
  R2_null = r.squaredGLMM(no_metaeco_type_model)[1,1]
  
  R2[[last_point]] =  R2_full - R2_null
  R2[[last_point]] = round(R2[[last_point]], digits = 3)
  
}

