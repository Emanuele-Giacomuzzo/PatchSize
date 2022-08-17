## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ds_regional %>%
  filter(time_point >= 2) %>%
  ggplot(aes(x = day,
             y = regional_mean_bioarea,
             group = day)) +
  geom_boxplot() +
  labs(title = "Without log transformation",
       x = "Day",
       y = "Regional bioarea (something/µl)")


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
linear_model = lm(regional_mean_bioarea ~ 
                    day, 
                  data = ds_regional %>% 
                            filter(time_point >= 2) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"))

par(mfrow=c(2,3))
plot(linear_model, which = 1:5)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ds_regional %>%
  filter(time_point >= 2) %>%
  ggplot(aes(x = day,
             y = log(regional_mean_bioarea + 1),
             group = day)) +
  geom_boxplot() +
  labs(title = "With log transformation",
       x = "Day",
       y = "Log (regional bioarea + 1) (something/µl)")


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
log_linear_model = lm(log10(regional_mean_bioarea + 1) ~ 
                    day, 
                  data = ds_regional %>% 
                            filter(time_point >= 2) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"))

par(mfrow=c(2,3))
plot(log_linear_model, which = 1:5)
par(mfrow=c(1,1))


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full = lmer(log10(regional_mean_bioarea + 1) ~
                     day * metaecosystem_type * disturbance +
                     (day | system_nr),
                     data = ds_regional %>%
                            filter(time_point >= 2) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"),
                   REML = FALSE,
                   control = lmerControl(optimizer = "Nelder_Mead"))


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
no_correlation = lmer(log10(regional_mean_bioarea + 1) ~
                     day * metaecosystem_type * disturbance +
                     (day | system_nr),
                     data = ds_regional %>%
                            filter(time_point >= 2) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"),
                   REML = FALSE,
                   control = lmerControl(optimizer = "Nelder_Mead"))

anova(full, no_correlation)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
no_threeway = lmer(log10(regional_mean_bioarea + 1) ~
                     day +
                     metaecosystem_type +
                     disturbance +
                     day : metaecosystem_type + 
                     day : disturbance +
                     metaecosystem_type : disturbance + 
                     (day | system_nr),
                     data = ds_regional %>%
                            filter(time_point >= 2) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"),
                   REML = FALSE,
                   control = lmerControl(optimizer = 'optimx', 
                                         optCtrl = list(method = 'L-BFGS-B')))

anova(full, no_threeway)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
no_TM = lmer(log10(regional_mean_bioarea + 1) ~
                     day +
                     metaecosystem_type +
                     disturbance +
                     day : disturbance +
                     metaecosystem_type : disturbance + 
                     (day | system_nr),
                     data = ds_regional %>%
                            filter(time_point >= 2) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"),
                   REML = FALSE,
                   control = lmerControl(optimizer = "Nelder_Mead"))

anova(no_threeway,no_TM)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
no_TD = lmer(log10(regional_mean_bioarea + 1) ~
                     day +
                     metaecosystem_type +
                     disturbance +
                     metaecosystem_type : disturbance + 
                     (day | system_nr),
                     data = ds_regional %>%
                            filter(time_point >= 2) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"),
                   REML = FALSE,
                   control = lmerControl(optimizer = "Nelder_Mead"))
anova(no_TM, no_TD)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
no_MD = lmer(log10(regional_mean_bioarea + 1) ~
                     day +
                     metaecosystem_type +
                     disturbance +
                     day : disturbance +
                     (day | system_nr),
                     data = ds_regional %>%
                            filter(time_point >= 2) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"),
                   REML = FALSE,
                   control = lmerControl(optimizer = "Nelder_Mead"))

anova(no_TM, no_MD)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
no_random_slopes = lmer(log10(regional_mean_bioarea + 1) ~
                     day +
                     metaecosystem_type +
                     disturbance +
                     day : disturbance +
                     (1 | system_nr),
                     data = ds_regional %>%
                            filter(time_point >= 2) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"),
                   REML = FALSE,
                   control = lmerControl(optimizer = "Nelder_Mead"))

anova(no_MD, no_random_slopes)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Create a table in which the regional biomass has been log transformed. 

### --- INITIALISE TABLE --- ###

columns = c("model", "time_point", "AIC", "BIC", "R2_mixed", "R2_fixed", "R2_mixed_M", "R2_fixed_M")
log_time_table = data.frame(matrix(ncol = length(columns), nrow = 0))
colnames(log_time_table) = columns

### --- POPULATE THE TABLE --- ###

for (last_point in 4:7) {
  
  full_model = lmer(log10(regional_mean_bioarea + 1) ~
                     day +
                     metaecosystem_type +
                     disturbance +
                     day : disturbance +
                     (day | system_nr),
                     data = ds_regional %>%
                            filter(time_point >= 2) %>%
                            filter(time_point <= last_point) %>%
                            filter(metaecosystem_type == "M_M" | 
                                   metaecosystem_type == "S_L"),
                    REML = FALSE,
                    control = lmerControl(optimizer = "Nelder_Mead"))

  
  null_model = lm(regional_mean_bioarea ~ 
                    1 , 
                  data = ds_regional %>%
                            filter(time_point >= 2) %>%
                            filter(time_point <= last_point) %>%
                            filter(metaecosystem_type == "M_M" | 
                                   metaecosystem_type == "S_L"))
  
  metaeco_null_model = lmer(log10(regional_mean_bioarea + 1) ~
                              day +
                              disturbance +
                              day : disturbance +
                              (day | system_nr),
                            data = ds_regional %>%
                              filter(time_point >= 2) %>%
                              filter(time_point <= last_point) %>%
                              filter(metaecosystem_type == "M_M" | 
                                     metaecosystem_type == "S_L"),
                            REML = FALSE,
                            control = lmerControl(optimizer = "Nelder_Mead"))
  
  log_time_table = update_all_models_table("t + M + D + t * M * D + (t || system_nr)",
                                             log_time_table, 
                                             full_model, 
                                             null_model,
                                             metaeco_null_model,
                                             "mixed")
}

datatable(log_time_table, 
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
          (1 | t) = random effect of time on the intercept,
          (1 | ID) = random effect of meta-ecosystem ID on the intercept, 
          || = no correlation between intercept and slope,
          | = correlation between intercept and slope,
          R2 = r squared of the whole model,
          R2_fixed = fixed part of the mixed model,
          mixed_R2 = r squared when considering both fixed and random effects (conditional r squared), 
          fixed_R2 = r squared when considering only the fixed effects (marginal r squared)")

