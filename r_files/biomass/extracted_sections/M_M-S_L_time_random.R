## ----------------------------------------------------------------------------------------------------------
ds_regional_MM_SL_t2t7 = ds_regional %>%
    filter (metaecosystem_type == "M_M" | metaecosystem_type == "S_L", 
            time_point >= 2)


## ----full-models-------------------------------------------------------------------------------------------
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


## ----------------------------------------------------------------------------------------------------------
best_model = full_model_correlated


## ----no_interaction_model, message=FALSE-------------------------------------------------------------------
no_interaction_model = lmer(regional_mean_bioarea ~ 
                              metaecosystem_type + 
                              disturbance  + 
                              (metaecosystem_type | day) + 
                              (disturbance | day) +
                              (1 | system_nr), 
                            data = ds_regional_MM_SL_t2t7, 
                            REML = FALSE)

anova(best_model, no_interaction_model)


## ----------------------------------------------------------------------------------------------------------
best_model = no_interaction_model


## ----no_slopes_model, message=FALSE------------------------------------------------------------------------
no_slopes_model = lmer(regional_mean_bioarea ~ 
                         metaecosystem_type + 
                         disturbance  + 
                         (1 | day) +
                         (1 | system_nr), 
                       data = ds_regional_MM_SL_t2t7, 
                       REML = FALSE)

anova(best_model, no_slopes_model)


## ----best-model--------------------------------------------------------------------------------------------
best_model = no_slopes_model


## ----------------------------------------------------------------------------------------------------------
no_system_nr = lmer(regional_mean_bioarea ~ 
                         metaecosystem_type + 
                         disturbance  + 
                         (1 | day), 
                       data = ds_regional_MM_SL_t2t7, 
                       REML = FALSE)

anova(best_model, no_system_nr)


## ----------------------------------------------------------------------------------------------------------
best_model_without_system_nr = no_system_nr
best_model_with_system_nr = no_interaction_model


## ----------------------------------------------------------------------------------------------------------

### --- INITIALISE TABLE --- ###

columns = c("model", "time_point", "AIC", "BIC", "mixed_R2", "mixed_R2_metaeco", "fixed_R2", "fixed_R2_metaeco")
random_time_table = data.frame(matrix(ncol = length(columns),
                                     nrow = 0))
colnames(random_time_table) = columns

### --- "M + D + (1 | t)" --- ###

for (last_point in 3:7) {
  
  full_model = lmer(regional_mean_bioarea ~ 
                      disturbance +
                      metaecosystem_type +
                      (1 | day),
                    data = filter(ds_regional_MM_SL_t2t7, time_point <= last_point),
                    REML = FALSE,
                    control = lmerControl(optimizer ="Nelder_Mead"))
  
  null_model = lm(regional_mean_bioarea ~ 
                    1 , 
                  data = filter(ds_regional_MM_SL_t2t7, time_point <= last_point))
   
  metaeco_null = lmer(regional_mean_bioarea ~  
                                   disturbance  + 
                                   (1 | day),
                                 data = filter(ds_regional_MM_SL_t2t7, 
                                               time_point <= last_point),
                                 REML = FALSE, 
                                 control = lmerControl(optimizer ="Nelder_Mead"))
  
  random_time_table = update_all_models_table("M + D + (1 | t)",
                                             random_time_table, 
                                             full_model, 
                                             null_model,
                                             metaeco_null,
                                             "mixed")
  
}

### --- M + D + (1 | t) + (1 |ID) --- ###

for (last_point in 3:7) {
  
  full_model = lmer(regional_mean_bioarea ~ 
                      disturbance +
                      metaecosystem_type +
                      (1 | day) +
                      (1 | system_nr),
                    data = filter(ds_regional_MM_SL_t2t7, time_point <= last_point),
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
  
  random_time_table = update_all_models_table("M + D + (1 | t) + (1 | ID)",
                                             random_time_table, 
                                             full_model, 
                                             null_model,
                                             metaeco_null,
                                             "mixed")
  
}


## ----echo = FALSE------------------------------------------------------------------------------------------
datatable(random_time_table, caption = "M = Meta-ecosystem type, D = disturbance, (1 | t) = random effect of time on the intercept, (1 | ID) = random effect of meta-ecosystem ID on the intercept, mixed_R2 = r squared when considering both fixed and random effects (conditional r squared), fixed_R2 = r squared when considering only the fixed effects (marginal r squared)")

