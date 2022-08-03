## ------------------------------------------------------------------------------------------------------------------------------------------------------------
ds_regional_predicted_shrunk_type_n_day = ds_regional_predicted_shrunk_type %>%
  filter(time_point >= 2)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
model_slopes_correlated = lmer(regional_mean_bioarea ~ 
                            predicted_from_time + 
                            metaecosystem_type  + 
                            disturbance + 
                            predicted_from_time * metaecosystem_type +
                            predicted_from_time * disturbance +
                            metaecosystem_type * disturbance +
                            predicted_from_time * metaecosystem_type * disturbance +
                            (predicted_from_time | system_nr),
                        data = ds_regional_predicted_shrunk_type_n_day,
                        REML = FALSE)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
model_slopes_uncorrelated = lmer(regional_mean_bioarea ~ 
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
                        control = lmerControl(optimizer = "Nelder_Mead"))

anova(model_slopes_uncorrelated, model_slopes_correlated)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
model_no_TM = lmer(regional_mean_bioarea ~ 
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

anova(model_slopes_uncorrelated, model_no_TM)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
model_no_TD = lmer(regional_mean_bioarea ~ 
                            predicted_from_time + 
                            metaecosystem_type  + 
                            disturbance + 
                            metaecosystem_type * disturbance +
                            predicted_from_time * metaecosystem_type * disturbance +
                            (predicted_from_time || system_nr),
               data = ds_regional_predicted_shrunk_type_n_day,
               REML = FALSE,
               control = lmerControl (optimizer = "Nelder_Mead"))

anova(model_no_TM, model_no_TD)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
model_no_MD = lmer(regional_mean_bioarea ~ 
                            predicted_from_time + 
                            metaecosystem_type  + 
                            disturbance + 
                            predicted_from_time * metaecosystem_type * disturbance +
                            (predicted_from_time || system_nr),
               data = ds_regional_predicted_shrunk_type_n_day,
               REML = FALSE,
               control = lmerControl (optimizer = "Nelder_Mead"))

anova(model_no_TD, model_no_MD)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
model_no_TMD = lmer(regional_mean_bioarea ~ 
                            predicted_from_time + 
                            metaecosystem_type  + 
                            disturbance + 
                            (predicted_from_time || system_nr),
               data = ds_regional_predicted_shrunk_type_n_day,
               REML = FALSE,
               control = lmerControl (optimizer = "Nelder_Mead"))

anova(model_no_MD, model_no_TMD)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
model_no_random_slope = lmer(regional_mean_bioarea ~ 
                            predicted_from_time + 
                            metaecosystem_type  + 
                            disturbance + 
                            (1 | system_nr),
               data = ds_regional_predicted_shrunk_type_n_day,
               REML = FALSE,
               control = lmerControl (optimizer = "Nelder_Mead"))

anova(model_no_MD, model_no_random_slope)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
model_no_MD_correlated = lmer(regional_mean_bioarea ~ 
                            predicted_from_time + 
                            metaecosystem_type  + 
                            disturbance + 
                            predicted_from_time * metaecosystem_type * disturbance +
                            (predicted_from_time | system_nr),
               data = ds_regional_predicted_shrunk_type_n_day,
               REML = FALSE,
               control = lmerControl (optimizer = "Nelder_Mead"))

anova(model_no_MD, model_no_MD_correlated)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
fixed_model = lm(regional_mean_bioarea ~ 
                            predicted_from_time + 
                            metaecosystem_type  + 
                            disturbance,
               data = ds_regional_predicted_shrunk_type_n_day)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
anova(model_slopes_correlated, model_slopes_uncorrelated, model_no_random_slope, fixed_model) #Only with models df > 0 

#anova(model_slopes_correlated, model_slopes_uncorrelated, model_no_TM, model_no_TD, model_no_MD, model_no_TMD, model_no_random_slope, fixed_model) #With all the models


## ----echo = FALSE, warning = FALSE---------------------------------------------------------------------------------------------------------------------------
#Create a table in which time is a fixed effect. 

### --- INITIALISE TABLE --- ###

columns = c("model", "time_point", "AIC", "BIC", "R2", "R2_fixed", "R2_meta_mixed", "R2_meta_fixed") #"R2_mixed"
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

### --- T + M + D + T*M*D + (T||ID) --- ###

for (last_point in 3:7) {
  
  full_model = lmer(regional_mean_bioarea ~ 
                      predicted_from_time +
                      metaecosystem_type +
                      disturbance +
                      predicted_from_time * metaecosystem_type * disturbance + 
                      (predicted_from_time || system_nr),
                    data = filter(ds_regional_predicted_shrunk_type_n_day, 
                                  time_point <= last_point),
                    REML = FALSE,
                    control = lmerControl(optimizer ="Nelder_Mead"))
  
  null_model = lm(regional_mean_bioarea ~ 
                    1 , 
                  data = filter(ds_regional_predicted_shrunk_type_n_day, 
                                time_point <= last_point))
   
  metaeco_null = lmer(regional_mean_bioarea ~  
                        predicted_from_time +
                        disturbance +
                        predicted_from_time * disturbance + 
                        (predicted_from_time || system_nr),
                      data = filter(ds_regional_predicted_shrunk_type_n_day, 
                                               time_point <= last_point),
                      REML = FALSE, 
                      control = lmerControl(optimizer ="Nelder_Mead"))
  
  fitted_time_table = update_all_models_table("T + M + D + T*M*D + (T||ID)",
                                             fitted_time_table, 
                                             full_model, 
                                             null_model,
                                             metaeco_null,
                                             "mixed")
}

# for (last_point in 3:7) {
#   
#   full_model = lmer(regional_mean_bioarea ~ 
#                             predicted_from_time + 
#                             metaecosystem_type  + 
#                             disturbance + 
#                             predicted_from_time * metaecosystem_type * disturbance +
#                             (predicted_from_time | system_nr),
#                data = filter(ds_regional_predicted_shrunk_type_n_day, 
#                                   time_point <= last_point),
#                REML = FALSE,
#                control = lmerControl (optimizer = "Nelder_Mead"))
#   
#   null_model = lm(regional_mean_bioarea ~ 
#                     1 , 
#                   data = filter(ds_regional_predicted_shrunk_type_n_day, 
#                                 time_point <= last_point))
#    
#   metaeco_null = lmer(regional_mean_bioarea ~  
#                         predicted_from_time +
#                         disturbance +
#                         predicted_from_time * disturbance + 
#                         (predicted_from_time | system_nr),
#                       data = filter(ds_regional_predicted_shrunk_type_n_day, 
#                                                time_point <= last_point),
#                       REML = FALSE, 
#                       control = lmerControl(optimizer ="Nelder_Mead"))
#   
#   fitted_time_table = update_all_models_table("T + M + D + T*M*D + (T|ID)",
#                                              fitted_time_table, 
#                                              full_model, 
#                                              null_model,
#                                              metaeco_null,
#                                              "mixed")
# }

