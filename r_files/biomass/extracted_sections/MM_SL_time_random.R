## ------------------------------------------------------------------------------------------------------------------------------------------------------------
ds_regional_MM_SL_t2t7 = ds_regional %>%
    filter (metaecosystem_type == "M_M" | metaecosystem_type == "S_L", 
            time_point >= 2)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
full_model_correlated = lmer(regional_mean_bioarea ~ 
                             metaecosystem_type  + 
                             disturbance + 
                             metaecosystem_type * disturbance + 
                             (metaecosystem_type | day) + 
                             (disturbance | day) + 
                             (metaecosystem_type*disturbance  | day) +
                             (1 | system_nr) , 
                             data = ds_regional_MM_SL_t2t7, 
                             REML = FALSE)

full_model_uncorrelated = lmer(regional_mean_bioarea ~ 
                    metaecosystem_type  + 
                    disturbance + 
                    metaecosystem_type * disturbance + 
                    (metaecosystem_type || day) + 
                    (disturbance || day) + 
                    (metaecosystem_type*disturbance  || day) +
                    (1 | system_nr) ,
                  data = ds_regional_MM_SL_t2t7, 
                  REML = FALSE)

anova(full_model_uncorrelated, full_model_correlated)


## ---- message=FALSE------------------------------------------------------------------------------------------------------------------------------------------
no_interaction_model = lmer(regional_mean_bioarea ~ 
                              metaecosystem_type + 
                              disturbance  + 
                              (metaecosystem_type | day) + 
                              (disturbance | day) +
                              (1 | system_nr), 
                            data = ds_regional_MM_SL_t2t7, 
                            REML = FALSE)

anova(full_model_correlated, no_interaction_model)


## ---- message=FALSE------------------------------------------------------------------------------------------------------------------------------------------
no_metaeco_slopes_model = lmer(regional_mean_bioarea ~ 
                                 metaecosystem_type + 
                                 disturbance  + 
                                 (disturbance | day) +
                                 (1 | system_nr), 
                               data = ds_regional_MM_SL_t2t7, 
                               REML = FALSE)

anova(no_interaction_model, no_metaeco_slopes_model)


## ---- message=FALSE------------------------------------------------------------------------------------------------------------------------------------------
no_disturbance_slopes_model = lmer(regional_mean_bioarea ~ 
                                     metaecosystem_type + 
                                     disturbance  + 
                                     (metaecosystem_type | day) +
                                     (1 | system_nr), 
                                   data = ds_regional_MM_SL_t2t7, 
                                   REML = FALSE)

anova(no_interaction_model, no_disturbance_slopes_model)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
no_system_nr = lmer(regional_mean_bioarea ~ 
                      metaecosystem_type + 
                      disturbance  + 
                      (metaecosystem_type | day), 
                    data = ds_regional_MM_SL_t2t7, 
                    REML = FALSE)

anova(no_disturbance_slopes_model, no_system_nr)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
anova(full_model_correlated, full_model_uncorrelated, no_interaction_model, no_metaeco_slopes_model, no_disturbance_slopes_model, no_system_nr)


## ---- echo = FALSE, warning = FALSE, message = FALSE---------------------------------------------------------------------------------------------------------
#Create a table with all the models in which time is a random effect. 

### --- INITIALISE TABLE --- ###

columns = c("model", "time_point", "AIC", "BIC", "R2", "R2_fixed", "R2_meta_mixed", "R2_meta_fixed") # "R2_mixed"
random_time_table = data.frame(matrix(ncol = length(columns),
                                     nrow = 0))
colnames(random_time_table) = columns

### --- M + D + (M | t) + (1 | ID) --- ###

for (last_point in 3:7) {
  
  full_model = lmer(regional_mean_bioarea ~ 
                      disturbance +
                      metaecosystem_type +
                      (metaecosystem_type | day) +
                      (1 | system_nr),
                    data = filter(ds_regional_MM_SL_t2t7, 
                                  time_point <= last_point),
                    REML = FALSE,
                    control = lmerControl(optimizer ="Nelder_Mead"))
  
  null_model = lm(regional_mean_bioarea ~ 
                    1 , 
                  data = filter(ds_regional_MM_SL_t2t7, time_point <= last_point))
   
  metaeco_null = lmer(regional_mean_bioarea ~  
                        disturbance  + 
                        (1 | day) +
                        (1 | system_nr),
                                 data = filter(ds_regional_MM_SL_t2t7, 
                                               time_point <= last_point),
                                 REML = FALSE, 
                                 control = lmerControl(optimizer ="Nelder_Mead"))
  
  random_time_table = update_all_models_table("M + D + (M|t) + (1|ID)",
                                             random_time_table, 
                                             full_model, 
                                             null_model,
                                             metaeco_null,
                                             "mixed")
  
}

### --- M + D + (M | t) + (D | t) + (1 | ID) --- ###

for (last_point in 3:7) {
  
  full_model = lmer(regional_mean_bioarea ~ 
                      disturbance +
                      metaecosystem_type +
                      (metaecosystem_type | day) +
                      (disturbance | day) +
                      (1 | system_nr),
                    data = filter(ds_regional_MM_SL_t2t7, 
                                  time_point <= last_point),
                    REML = FALSE,
                    control = lmerControl(optimizer ="Nelder_Mead"))
  
  null_model = lm(regional_mean_bioarea ~ 
                    1 , 
                  data = filter(ds_regional_MM_SL_t2t7, 
                                time_point <= last_point))
  
  metaeco_null = lmer(regional_mean_bioarea ~  
                        disturbance +
                        (disturbance | day) +
                        (1 | system_nr),
                                 data = filter(ds_regional_MM_SL_t2t7, 
                                               time_point <= last_point),
                                 REML = FALSE, 
                                 control = lmerControl(optimizer ="Nelder_Mead"))
  
  random_time_table = update_all_models_table("M + D + (M|t) + (D|t) + (1|ID)",
                                             random_time_table, 
                                             full_model, 
                                             null_model,
                                             metaeco_null,
                                             "mixed")
  
}

### --- M + D + (M  | t) --- ###

for (last_point in 3:7) {
  
  full_model = lmer(regional_mean_bioarea ~ 
                      metaecosystem_type +
                      disturbance +
                      (metaecosystem_type | day),
                    data = filter(ds_regional_MM_SL_t2t7, 
                                  time_point <= last_point),
                    REML = FALSE,
                    control = lmerControl(optimizer ="Nelder_Mead"))
  
  null_model = lm(regional_mean_bioarea ~ 
                    1 , 
                  data = filter(ds_regional_MM_SL_t2t7, time_point <= last_point))
   
  metaeco_null = lm(regional_mean_bioarea ~  
                        disturbance,
                      data = filter(ds_regional_MM_SL_t2t7, 
                                    time_point <= last_point))
  
  random_time_table = update_all_models_table("M + D + (M|t)",
                                             random_time_table, 
                                             full_model, 
                                             null_model,
                                             metaeco_null,
                                             "mixed")
  
}

### --- M + D --- ###

for (last_point in 3:7) {
  
  full_model = lm(regional_mean_bioarea ~ 
                      metaecosystem_type +
                      disturbance,
                    data = filter(ds_regional_MM_SL_t2t7, 
                                  time_point <= last_point))

  null_model = lm(regional_mean_bioarea ~ 
                    1 , 
                  data = filter(ds_regional_MM_SL_t2t7, time_point <= last_point))
   
  metaeco_null = lm(regional_mean_bioarea ~  
                        disturbance,
                      data = filter(ds_regional_MM_SL_t2t7, 
                                    time_point <= last_point))
  
  random_time_table = update_all_models_table("M + D",
                                             random_time_table, 
                                             full_model, 
                                             null_model,
                                             metaeco_null,
                                             "fixed")
  
}


