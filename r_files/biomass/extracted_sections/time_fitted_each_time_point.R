## ----day-8--------------------------------------------------------------------------------------------------------------------------------------------------------------

time_point_input = 2

filtered_dataset = ds_regional_predicted_shrunk_type %>%
  filter(time_point == time_point_input)

model_1 = lm(regional_mean_bioarea ~ 
                            disturbance +
                            metaecosystem_type,
                        data = filtered_dataset)

model_2 = lm(regional_mean_bioarea ~ 
                            disturbance,
                        data = filtered_dataset)

anova(model_1, model_2)

summary(model_1)
summary(model_2)
R2_day_8 = summary(model_1)$adj.r.squared - summary(model_2)$adj.r.squared
R2_day_8 = round(R2_day_8, digits = 3)


time_point_input = 3

filtered_dataset = ds_regional_predicted_shrunk_type %>%
  filter(time_point == time_point_input)

model_1 = lm(regional_mean_bioarea ~ 
                            disturbance +
                            metaecosystem_type,
                        data = filtered_dataset)

model_2 = lm(regional_mean_bioarea ~ 
                            disturbance,
                        data = filtered_dataset)

anova(model_1, model_2)

summary(model_1)
summary(model_2)
R2_day_12 = summary(model_1)$adj.r.squared - summary(model_2)$adj.r.squared
R2_day_12 = round(R2_day_12, digits = 3)


time_point_input = 4

filtered_dataset = ds_regional_predicted_shrunk_type %>%
  filter(time_point == time_point_input)

model_1 = lm(regional_mean_bioarea ~ 
                            disturbance +
                            metaecosystem_type,
                        data = filtered_dataset)

model_2 = lm(regional_mean_bioarea ~ 
                            disturbance,
                        data = filtered_dataset)

anova(model_1, model_2)

summary(model_1)
summary(model_2)
R2_day_16 = summary(model_1)$adj.r.squared - summary(model_2)$adj.r.squared
R2_day_16 = round(R2_day_16, digits = 3)




time_point_input = 5

filtered_dataset = ds_regional_predicted_shrunk_type %>%
  filter(time_point == time_point_input)

model_1 = lm(regional_mean_bioarea ~ 
                            disturbance +
                            metaecosystem_type,
                        data = filtered_dataset)

model_2 = lm(regional_mean_bioarea ~ 
                            disturbance,
                        data = filtered_dataset)

anova(model_1, model_2)

summary(model_1)
summary(model_2)
R2_day_20 = summary(model_1)$adj.r.squared - summary(model_2)$adj.r.squared
R2_day_20 = round(R2_day_20, digits = 3)



time_point_input = 6

filtered_dataset = ds_regional_predicted_shrunk_type %>%
  filter(time_point == time_point_input)

model_1 = lm(regional_mean_bioarea ~ 
                            disturbance +
                            metaecosystem_type,
                        data = filtered_dataset)

model_2 = lm(regional_mean_bioarea ~ 
                            disturbance,
                        data = filtered_dataset)

anova(model_1, model_2)

summary(model_1)
summary(model_2)
R2_day_24 = summary(model_1)$adj.r.squared - summary(model_2)$adj.r.squared
R2_day_24 = round(R2_day_24, digits = 3)



time_point_input = 7

filtered_dataset = ds_regional_predicted_shrunk_type %>%
  filter(time_point == time_point_input)

model_1 = lm(regional_mean_bioarea ~ 
                            disturbance +
                            metaecosystem_type,
                        data = filtered_dataset)

model_2 = lm(regional_mean_bioarea ~ 
                            disturbance,
                        data = filtered_dataset)

anova(model_1, model_2)

summary(model_1)
summary(model_2)
R2_day_28 = summary(model_1)$adj.r.squared - summary(model_2)$adj.r.squared
R2_day_28 = round(R2_day_28, digits = 3)

