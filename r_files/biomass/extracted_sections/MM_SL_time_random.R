## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ds_regional_MM_SL_t2t7 = ds_regional %>%
    filter (metaecosystem_type == "M_M" | metaecosystem_type == "S_L", 
            time_point >= 2)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full_model = lmer(regional_mean_bioarea ~ 
                             metaecosystem_type  + 
                             disturbance + 
                             metaecosystem_type : disturbance + 
                             (metaecosystem_type | day) + 
                             (disturbance | day) + 
                             (metaecosystem_type : disturbance  | day) +
                             (1 | system_nr) , 
                             data = ds_regional_MM_SL_t2t7, 
                             REML = FALSE)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
no_MD = lmer(regional_mean_bioarea ~ 
                             metaecosystem_type  + 
                             disturbance + 
                             (metaecosystem_type | day) + 
                             (disturbance | day) + 
                             (1 | system_nr) , 
                             data = ds_regional_MM_SL_t2t7, 
                             REML = FALSE)

anova(full_model, no_MD)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
no_M_day_slope = lmer(regional_mean_bioarea ~ 
                             metaecosystem_type  + 
                             disturbance + 
                             (disturbance | day) + 
                             (1 | system_nr) , 
                             data = ds_regional_MM_SL_t2t7, 
                             REML = FALSE)

anova(no_MD, no_M_day_slope)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
no_M_day_correlation = lmer(regional_mean_bioarea ~ 
                             metaecosystem_type  + 
                             disturbance + 
                             (metaecosystem_type || day) + 
                             (disturbance | day) + 
                             (1 | system_nr) , 
                             data = ds_regional_MM_SL_t2t7, 
                             REML = FALSE)

anova(no_MD, no_M_day_correlation)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
no_D_day_slope = lmer(regional_mean_bioarea ~ 
                             metaecosystem_type  + 
                             disturbance + 
                             (metaecosystem_type | day) + 
                             (1 | system_nr) , 
                             data = ds_regional_MM_SL_t2t7, 
                             REML = FALSE)

anova(no_MD, no_M_day_slope)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
no_D_day_correlation = lmer(regional_mean_bioarea ~ 
                             metaecosystem_type  + 
                             disturbance + 
                             (metaecosystem_type | day) + 
                             (disturbance || day) + 
                             (1 | system_nr) , 
                             data = ds_regional_MM_SL_t2t7, 
                             REML = FALSE)

anova(no_MD, no_D_day_correlation)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
no_system_nr = lmer(regional_mean_bioarea ~ 
                             metaecosystem_type  + 
                             disturbance + 
                             (metaecosystem_type | day) + 
                             (disturbance || day), 
                             data = ds_regional_MM_SL_t2t7, 
                             REML = FALSE)

anova(no_D_day_correlation, no_system_nr)


## ----message = FALSE, warning = FALSE----------------------------------------------------------------------------------------------------------------------------------------------------
#Create a table with all the models in which time is a random effect. 

### --- INITIALISE TABLE --- ###

columns = c("model", "time_point", "AIC", "BIC", "R2_mixed", "R2_fixed", "R2_mixed_M", "R2_fixed_M")
random_time_table = data.frame(matrix(ncol = length(columns),
                                     nrow = 0))
colnames(random_time_table) = columns

### --- M + D + (M | t) + (D || t) --- ###

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
  
  random_time_table = update_all_models_table("M+D+(M|t)+(D||t)",
                                             random_time_table, 
                                             full_model, 
                                             null_model,
                                             metaeco_null,
                                             "mixed")
}

datatable(random_time_table, 
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
          M = Meta-ecosystem type, 
          D = disturbance, 
          t = time,
          (M | t) = random effect of time on the intercept and slope of M,
          (D || t) = random effect of time on the intercept and slope of D, 
          || = no correlation between intercept and slope,
          | = correlation between intercept and slope,
          R2_mixed = r squared of the model,
          R2_fixed = r squared of the model when considering only fixed effects,
          R2_mixed_M = r squared of meta-ecosystem type,
          R2_fixed_M = r squared of meta-ecosystem type when considerin only fixed effects")

