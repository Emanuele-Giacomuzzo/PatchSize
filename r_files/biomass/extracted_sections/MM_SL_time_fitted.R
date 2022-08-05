## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ds_regional_predicted_shrunk_type_n_day = ds_regional_predicted_shrunk_type %>%
  filter(time_point >= 2)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full = lmer(log10(regional_mean_bioarea + 1) ~
              predicted_from_time * metaecosystem_type * disturbance +
              (predicted_from_time | system_nr),
            data = ds_regional_predicted_shrunk_type_n_day,
            REML = FALSE,
            control = lmerControl(optimizer = "Nelder_Mead"))


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
no_correlation = lmer(log10(regional_mean_bioarea + 1) ~
              predicted_from_time * metaecosystem_type * disturbance +
              (predicted_from_time || system_nr),
            data = ds_regional_predicted_shrunk_type_n_day,
            REML = FALSE,
            control = lmerControl(optimizer = "Nelder_Mead"))

anova(full, no_correlation)
#Keep full


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
no_TMD = lmer(log10(regional_mean_bioarea + 1) ~
              predicted_from_time +
              metaecosystem_type +
              disturbance +
              predicted_from_time : metaecosystem_type +
              predicted_from_time : disturbance +
              metaecosystem_type : disturbance +
              (predicted_from_time | system_nr),
            data = ds_regional_predicted_shrunk_type_n_day,
            REML = FALSE,
            control = lmerControl(optimizer = "Nelder_Mead"))

anova(full, no_TMD)
#Keep no_TMD


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
no_TM = lmer(log10(regional_mean_bioarea + 1) ~
              predicted_from_time +
              metaecosystem_type +
              disturbance +
              predicted_from_time : disturbance +
              metaecosystem_type : disturbance +
              (predicted_from_time | system_nr),
            data = ds_regional_predicted_shrunk_type_n_day,
            REML = FALSE,
            control = lmerControl(optimizer = "Nelder_Mead"))

anova(no_TMD, no_TM)
#Keep no_TM


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
no_TD = lmer(log10(regional_mean_bioarea + 1) ~
              predicted_from_time +
              metaecosystem_type +
              disturbance +
              metaecosystem_type : disturbance +
              (predicted_from_time | system_nr),
            data = ds_regional_predicted_shrunk_type_n_day,
            REML = FALSE,
            control = lmerControl(optimizer = "Nelder_Mead"))

anova(no_TM, no_TD)
#Keep no_TM


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
no_MD = lmer(log10(regional_mean_bioarea + 1) ~
              predicted_from_time +
              metaecosystem_type +
              disturbance +
              predicted_from_time : disturbance +
              (predicted_from_time | system_nr),
            data = ds_regional_predicted_shrunk_type_n_day,
            REML = FALSE,
            control = lmerControl(optimizer = "Nelder_Mead"))

anova(no_TM, no_MD)
#Keep no_MD


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
no_random_slopes = lmer(log10(regional_mean_bioarea + 1) ~
              predicted_from_time +
              metaecosystem_type +
              disturbance +
              predicted_from_time : disturbance +
              (1 | system_nr),
            data = ds_regional_predicted_shrunk_type_n_day,
            REML = FALSE,
            control = lmerControl(optimizer = "Nelder_Mead"))

anova(no_MD, no_random_slopes)
#Keep no_MD


## ---- warning = FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------
#Create a table in which time is a fixed effect. 

### --- INITIALISE TABLE --- ###
columns = c("model", "time_point", "AIC", "BIC", "R2_mixed", "R2_fixed", "R2_mixed_M", "R2_fixed_M")
fitted_time_table = data.frame(matrix(ncol = length(columns), nrow = 0))
colnames(fitted_time_table) = columns

### --- POPULATE TABLE --- ###

for (last_point in 4:7) {
  
  full_model = lmer(log10(regional_mean_bioarea + 1) ~
                     predicted_from_time +
                     metaecosystem_type +
                     disturbance +
                     predicted_from_time : disturbance +
                     (predicted_from_time | system_nr),
                     data = ds_regional_predicted_shrunk_type_n_day %>%
                            filter(time_point <= last_point),
                    REML = FALSE,
                    control = lmerControl(optimizer = "Nelder_Mead"))

  null_model = lm(regional_mean_bioarea ~ 
                    1 , 
                  data = ds_regional_predicted_shrunk_type_n_day %>%
                            filter(time_point <= last_point))
  
  metaeco_null_model = lmer(log10(regional_mean_bioarea + 1) ~
                              predicted_from_time +
                              disturbance +
                              predicted_from_time : disturbance +
                              (predicted_from_time | system_nr),
                            data = ds_regional_predicted_shrunk_type_n_day %>%
                              filter(time_point <= last_point),
                            REML = FALSE,
                            control = lmerControl(optimizer = "Nelder_Mead"))
  
  fitted_time_table = update_all_models_table("Tp + M + D + Tp * D + (Tp | system_nr)",
                                             fitted_time_table, 
                                             full_model, 
                                             null_model,
                                             metaeco_null_model,
                                             "mixed")
}

datatable(fitted_time_table, 
          rownames = FALSE,
          options = list(pageLength = 100,
                         scrollX = TRUE,
                         autoWidth = TRUE,
                         columnDefs = list(list(targets=c(0),visible=TRUE, width='160'),
                                           list(targets=c(1), visible=TRUE, width='10'),
                                           list(targets=c(2), visible=TRUE, width='10'),
                                           list(targets=c(3), visible=TRUE, width='10'),
                                           list(targets=c(4), visible=TRUE, width='10'),
                                           list(targets=c(5), visible=TRUE, width='10'),
                                           list(targets=c(6), visible=TRUE, width='10'),
                                           list(targets=c(7), visible=TRUE, width='10'),
                                           list(targets='_all', visible=FALSE))),
          caption = "
          Tp = predicted from time,
          M = Meta-ecosystem type, 
          D = disturbance, 
          (1 | t) = random effect of time on the intercept,
          (1 | ID) = random effect of meta-ecosystem ID on the intercept, 
          || = no correlation between intercept and slope,
          | = correlation between intercept and slope,
          R2 = r squared of the whole model,
          R2_fixed = fixed part of the mixed model,
          mixed_R2 = r squared when considering both fixed and random effects (conditional r squared), 
          fixed_R2 = r squared when considering only the fixed effects (marginal r squared)")

