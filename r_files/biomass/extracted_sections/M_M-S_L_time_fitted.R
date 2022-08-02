## ------------------------------------------------------------------------------------------------------------------------------------------------------------
ds_regional_predicted_shrunk_type_n_day = ds_regional_predicted_shrunk_type %>%
  filter(time_point >= 2)


## ----eval = FALSE--------------------------------------------------------------------------------------------------------------------------------------------
## model_slopes_correlated = lmer(regional_mean_bioarea ~
##                             predicted_from_time +
##                             metaecosystem_type  +
##                             disturbance +
##                             predicted_from_time * metaecosystem_type +
##                             predicted_from_time * disturbance +
##                             metaecosystem_type * disturbance +
##                             predicted_from_time * metaecosystem_type * disturbance +
##                             (predicted_from_time | system_nr),
##                         data = ds_regional_predicted_shrunk_type_n_day,
##                         REML = FALSE)
## 
## model_slopes_uncorrelated = lmer(regional_mean_bioarea ~
##                             predicted_from_time +
##                             metaecosystem_type  +
##                             disturbance +
##                             predicted_from_time * metaecosystem_type +
##                             predicted_from_time * disturbance +
##                             metaecosystem_type * disturbance +
##                             predicted_from_time * metaecosystem_type * disturbance +
##                             (predicted_from_time || system_nr),
##                         data = ds_regional_predicted_shrunk_type_n_day,
##                         REML = FALSE)
## 
## anova(model_slopes_uncorrelated, model_slopes_correlated)


## ----time-fitted-random-effects------------------------------------------------------------------------------------------------------------------------------
model_1 = lmer(regional_mean_bioarea ~ 
                            predicted_from_time + 
                            metaecosystem_type  + 
                            disturbance + 
                            predicted_from_time * metaecosystem_type +
                            predicted_from_time * disturbance +
                            metaecosystem_type * disturbance +
                            predicted_from_time * metaecosystem_type * disturbance +
                            (predicted_from_time || system_nr),
               data = ds_regional_predicted_shrunk_type_n_day,
               REML = FALSE,
               control = lmerControl (optimizer = "Nelder_Mead")
               )

model_2 = lmer(regional_mean_bioarea ~ 
                            predicted_from_time + 
                            metaecosystem_type  + 
                            disturbance + 
                            predicted_from_time * disturbance +
                            metaecosystem_type * disturbance +
                            predicted_from_time * metaecosystem_type * disturbance +
                            (predicted_from_time || system_nr),
               data = ds_regional_predicted_shrunk_type_n_day,
               REML = FALSE,
               control = lmerControl (optimizer = "Nelder_Mead")
               )

anova(model_1, model_2)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
model_3 = lmer(regional_mean_bioarea ~ 
                            predicted_from_time + 
                            metaecosystem_type  + 
                            disturbance + 
                            metaecosystem_type * disturbance +
                            predicted_from_time * metaecosystem_type * disturbance +
                            (predicted_from_time || system_nr),
               data = ds_regional_predicted_shrunk_type_n_day,
               REML = FALSE,
               control = lmerControl (optimizer = "Nelder_Mead"))

anova(model_2, model_3)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
model_4 = lmer(regional_mean_bioarea ~ 
                            predicted_from_time + 
                            metaecosystem_type  + 
                            disturbance + 
                            predicted_from_time * metaecosystem_type * disturbance +
                            (predicted_from_time || system_nr),
               data = ds_regional_predicted_shrunk_type_n_day,
               REML = FALSE,
               control = lmerControl (optimizer = "Nelder_Mead"))

anova(model_3, model_4)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
model_5 = lmer(regional_mean_bioarea ~ 
                            predicted_from_time + 
                            metaecosystem_type  + 
                            disturbance + 
                            (predicted_from_time || system_nr),
               data = ds_regional_predicted_shrunk_type_n_day,
               REML = FALSE,
               control = lmerControl (optimizer = "Nelder_Mead"))

anova(model_4, model_5)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------

#Take off disturbance slopes
model_8 = lmer(regional_mean_bioarea ~ 
                            predicted_from_time + 
                            metaecosystem_type  + 
                            disturbance + 
                            (1 | system_nr),
               data = ds_regional_predicted_shrunk_type_n_day,
               REML = FALSE,
               control = lmerControl (optimizer = "Nelder_Mead"))

anova(model_5, model_8)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------

#Take off random effects
model_9 = lm(regional_mean_bioarea ~ 
                            predicted_from_time + 
                            metaecosystem_type  + 
                            disturbance,
                        data = ds_regional_predicted_shrunk_type_n_day)

anova(model_8, model_9)


## ----echo = FALSE, warning = FALSE---------------------------------------------------------------------------------------------------------------------------
#Create a table in which time is a fixed effect. 

### --- INITIALISE TABLE --- ###

columns = c("model", "time_point", "AIC", "BIC", "mixed_R2", "fixed_R2", "mixed_R2_metaeco", "fixed_R2_metaeco")
fitted_time_table = data.frame(matrix(ncol = length(columns),
                                     nrow = 0))
colnames(fitted_time_table) = columns

### --- T + M + D --- ###

for (last_point in 3:7) {
  
  full_model = lm(regional_mean_bioarea ~ 
                    predicted_from_time +
                    metaecosystem_type +
                    disturbance,
                    data = filter(ds_regional_predicted_shrunk_type_n_day, 
                                  time_point <= last_point))
  
  null_model = lm(regional_mean_bioarea ~ 
                    1 , 
                  data = filter(ds_regional_predicted_shrunk_type_n_day, 
                                time_point <= last_point))
  
  metaeco_null_model = lm(regional_mean_bioarea ~  
                            predicted_from_time +
                            disturbance,
                                 data = filter(ds_regional_predicted_shrunk_type_n_day, 
                                               time_point <= last_point))
  
  fitted_time_table = update_all_models_table("T + M + D",
                                             fitted_time_table, 
                                             full_model, 
                                             null_model,
                                             metaeco_null_model,
                                             "fixed")
}

### --- T + M + D + T*M*D --- ###

for (last_point in 3:7) {
  
  full_model = lm(regional_mean_bioarea ~ 
                    predicted_from_time +
                    metaecosystem_type +
                    disturbance +
                    predicted_from_time * metaecosystem_type * disturbance,
                    data = filter(ds_regional_predicted_shrunk_type_n_day, 
                                  time_point <= last_point))
  
  null_model = lm(regional_mean_bioarea ~ 
                    1 , 
                  data = filter(ds_regional_predicted_shrunk_type_n_day, 
                                time_point <= last_point))
  
  metaeco_null_model = lm(regional_mean_bioarea ~  
                                 predicted_from_time +
                                 disturbance +
                                 predicted_from_time * disturbance,
                                 data = filter(ds_regional_predicted_shrunk_type_n_day, 
                                               time_point <= last_point))
  
  fitted_time_table = update_all_models_table("T + M + D + T*M*D",
                                             fitted_time_table, 
                                             full_model, 
                                             null_model,
                                             metaeco_null_model,
                                             "fixed")
}

### --- T + M + D + T*M*D + (T|ID) --- ### PROBLEM!!!

for (last_point in 3:7) {
  
  full_model = lmer(regional_mean_bioarea ~ 
                      predicted_from_time +
                      metaecosystem_type +
                      disturbance +
                      predicted_from_time * metaecosystem_type * disturbance + 
                      (predicted_from_time || system_nr),
                    data = filter(ds_regional_predicted_shrunk_type_n_day, time_point <= last_point),
                    REML = FALSE,
                    control = lmerControl(optimizer ="Nelder_Mead"))
  
  null_model = lm(regional_mean_bioarea ~ 
                    1 , 
                  data = filter(ds_regional_predicted_shrunk_type_n_day, time_point <= last_point))
   
  metaeco_null = lmer(regional_mean_bioarea ~  
                        predicted_from_time +
                        disturbance +
                        predicted_from_time * disturbance + 
                        (predicted_from_time || system_nr),
                      data = filter(ds_regional_predicted_shrunk_type_n_day, 
                                               time_point <= last_point),
                      REML = FALSE, 
                      control = lmerControl(optimizer ="Nelder_Mead"))
  
  fitted_time_table = update_all_models_table("T + M + D + T*M*D + (T|ID)",
                                             fitted_time_table, 
                                             full_model, 
                                             null_model,
                                             metaeco_null,
                                             "mixed")
}

### --- T + M + D + (T|ID) --- ###

for (last_point in 3:7) {
  
  full_model = lmer(regional_mean_bioarea ~ 
                      predicted_from_time +
                      metaecosystem_type +
                      disturbance +
                      (predicted_from_time || system_nr),
                    data = filter(ds_regional_predicted_shrunk_type_n_day, time_point <= last_point),
                    REML = FALSE,
                    control = lmerControl(optimizer ="Nelder_Mead"))
  
  null_model = lm(regional_mean_bioarea ~ 
                    1 , 
                  data = filter(ds_regional_predicted_shrunk_type_n_day, time_point <= last_point))
   
  metaeco_null = lmer(regional_mean_bioarea ~  
                        predicted_from_time +
                        disturbance +
                        (predicted_from_time || system_nr),
                      data = filter(ds_regional_predicted_shrunk_type_n_day, 
                                               time_point <= last_point),
                      REML = FALSE, 
                      control = lmerControl(optimizer ="Nelder_Mead"))
  
  fitted_time_table = update_all_models_table("T + M + D + (T|ID)",
                                             fitted_time_table, 
                                             full_model, 
                                             null_model,
                                             metaeco_null,
                                             "mixed")
}


