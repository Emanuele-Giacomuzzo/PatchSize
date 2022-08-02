#Different time points single points 
#R2 = NULL
#for (t in 2:7) {
#  
#  time_point_input = t
#  filtered_dataset = ds_regional_shrunk_type_n_day %>%
#    filter(time_point == time_point_input)
#  model_1 = lm(regional_mean_bioarea ~ 
#                            disturbance +
#                            metaecosystem_type,
#                        data = filtered_dataset)
#  model_2 = lm(regional_mean_bioarea ~ 
#                            disturbance,
#                        data = filtered_dataset)
#  R2[[t]] = summary(model_1)$adj.r.squared - summary(model_2)$adj.r.squared
#  R2[[t]] = round(R2[[t]], digits = 3)
#  
#}