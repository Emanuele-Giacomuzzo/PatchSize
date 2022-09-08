## ---- echo = FALSE-------------------------------------------------------------------------------------------------------------------------------------------
ds_regional %>%
  filter(time_point >= 2) %>%
  ggplot(aes(x = day,
             y = regional_mean_bioarea,
             group = day)) +
  geom_boxplot() +
  labs(title = "Without log transformation",
       x = "Day",
       y = "Regional bioarea (something/µl)")


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
linear_model = lm(regional_mean_bioarea ~ 
                    day, 
                  data = ds_regional %>% 
                            filter(time_point >= 2) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"))

summary(linear_model)
par(mfrow=c(2,3))
plot(linear_model, which = 1:5)


## ---- echo = FALSE-------------------------------------------------------------------------------------------------------------------------------------------
ds_regional %>%
  filter(time_point >= 2) %>%
  ggplot(aes(x = day,
             y = log(regional_mean_bioarea + 1),
             group = day)) +
  geom_boxplot() +
  labs(title = "With log transformation",
       x = "Day",
       y = "Log (regional bioarea + 1) (something/µl)")


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
log_linear_model = lm(log10(regional_mean_bioarea + 1) ~ 
                    day, 
                  data = ds_regional %>% 
                            filter(time_point >= 2) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"))

summary(log_linear_model)
par(mfrow=c(2,3))
plot(log_linear_model, which = 1:5)
par(mfrow=c(1,1))


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
full_model = lmer(log10(regional_mean_bioarea + 1) ~
                     day +
                     metaecosystem_type +
                     disturbance +
                     day * metaecosystem_type +
                     day * disturbance +
                     metaecosystem_type * disturbance + 
                     day * metaecosystem_type * disturbance +
                     (day | system_nr),
                     data = ds_regional %>%
                            filter(time_point >= 2) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"),
                   REML = FALSE,
                   control = lmerControl(optimizer = "Nelder_Mead"))

summary(full_model)
#plot(full_model) 
#qqnorm(resid(full_model))


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
full = lmer(log10(regional_mean_bioarea + 1) ~
                     day * metaecosystem_type * disturbance +
                     (day || system_nr),
                     data = ds_regional %>%
                            filter(time_point >= 2) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"),
                   REML = FALSE,
                   control = lmerControl(optimizer = "Nelder_Mead"))


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
no_threeway = lmer(log10(regional_mean_bioarea + 1) ~
                     day +
                     metaecosystem_type +
                     disturbance +
                     day : metaecosystem_type + 
                     day : disturbance +
                     metaecosystem_type : disturbance + 
                     (day || system_nr),
                     data = ds_regional %>%
                            filter(time_point >= 2) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"),
                   REML = FALSE,
                   control = lmerControl(optimizer = "Nelder_Mead"))

anova(full, no_threeway)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
no_TM = lmer(log10(regional_mean_bioarea + 1) ~
                     day +
                     metaecosystem_type +
                     disturbance +
                     day : disturbance +
                     metaecosystem_type : disturbance + 
                     (day || system_nr),
                     data = ds_regional %>%
                            filter(time_point >= 2) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"),
                   REML = FALSE,
                   control = lmerControl(optimizer = "Nelder_Mead"))

anova(no_threeway,no_TM)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
no_TD = lmer(log10(regional_mean_bioarea + 1) ~
                     day +
                     metaecosystem_type +
                     disturbance +
                     metaecosystem_type : disturbance + 
                     (day || system_nr),
                     data = ds_regional %>%
                            filter(time_point >= 2) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"),
                   REML = FALSE,
                   control = lmerControl(optimizer = "Nelder_Mead"))
anova(no_TM, no_TD)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
no_MD = lmer(log10(regional_mean_bioarea + 1) ~
                     day +
                     metaecosystem_type +
                     disturbance +
                     day : disturbance +
                     (day || system_nr),
                     data = ds_regional %>%
                            filter(time_point >= 2) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"),
                   REML = FALSE,
                   control = lmerControl(optimizer = "Nelder_Mead"))

anova(no_TM, no_MD)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
with_correlation = lmer(log10(regional_mean_bioarea + 1) ~
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

anova(no_MD, with_correlation)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
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


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
best_model = no_MD
R2_full_model = r.squaredGLMM(best_model)
round(R2_full_model, digits = 3)

best_model_no_metaecosystem_type = lmer(log10(regional_mean_bioarea + 1) ~
                                          day +
                                          disturbance +
                                          day : disturbance +
                                          (day || system_nr),
                                        data = ds_regional %>%
                                          filter(time_point >= 2) %>%
                                          filter(metaecosystem_type == "M_M" |
                                                   metaecosystem_type == "S_L"),
                                        REML = FALSE,
                                        control = lmerControl(optimizer = "Nelder_Mead"))
R2_no_metaecosystem_type = r.squaredGLMM(best_model_no_metaecosystem_type)
round(R2_no_metaecosystem_type, digits = 3)

R2_metaecosystem_type = R2_full_model - R2_no_metaecosystem_type
round(R2_metaecosystem_type, digits = 3)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
















#t5

best_model = lmer(log10(regional_mean_bioarea + 1) ~
                     day +
                     metaecosystem_type +
                     disturbance +
                     day : disturbance +
                     (day || system_nr),
                     data = ds_regional %>%
                            filter(time_point >= 2) %>%
                            filter(time_point <= 5) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"),
                   REML = FALSE,
                   control = lmerControl(optimizer = "Nelder_Mead"))

R2_full_model = r.squaredGLMM(best_model)
round(R2_full_model, digits = 3)

best_model_no_metaecosystem_type = lmer(log10(regional_mean_bioarea + 1) ~
                                          day +
                                          disturbance +
                                          day : disturbance +
                                          (day || system_nr),
                                        data = ds_regional %>%
                                          filter(time_point >= 2) %>%
                                          filter(time_point <= 5) %>%
                                          filter(metaecosystem_type == "M_M" |
                                                   metaecosystem_type == "S_L"),
                                        REML = FALSE,
                                        control = lmerControl(optimizer = "Nelder_Mead"))
R2_no_metaecosystem_type = r.squaredGLMM(best_model_no_metaecosystem_type)
round(R2_no_metaecosystem_type, digits = 3)

R2_metaecosystem_type = R2_full_model - R2_no_metaecosystem_type
round(R2_metaecosystem_type, digits = 3)


## ----echo = FALSE--------------------------------------------------------------------------------------------------------------------------------------------
#Create a table in which the regional biomass has been log transformed. 

### --- INITIALISE TABLE --- ###

columns = c("model", "time_point", "AIC", "BIC", "R2", "R2_fixed", "R2_meta_mixed", "R2_meta_fixed") #"R2_mixed"
log_time_table = data.frame(matrix(ncol = length(columns),
                                     nrow = 0))
colnames(log_time_table) = columns

### --- t + M + D + t * M * D + (t || system_nr) --- ###

for (last_point in 3:7) {
  
  full_model = lmer(log10(regional_mean_bioarea + 1) ~
                     day +
                     metaecosystem_type +
                     disturbance +
                     day * metaecosystem_type * disturbance +
                     (day || system_nr),
                    data = ds_regional %>%
                            filter(time_point >= 2) %>%
                            filter(metaecosystem_type == "M_M" | 
                                     metaecosystem_type == "S_L"),
                    REML = FALSE,
                    control = lmerControl(optimizer = "Nelder_Mead"))

  
  null_model = lm(regional_mean_bioarea ~ 
                    1 , 
                  data = ds_regional %>%
                            filter(time_point >= 2) %>%
                            filter(metaecosystem_type == "M_M" | 
                                     metaecosystem_type == "S_L"))
  
  metaeco_null_model = lmer(log10(regional_mean_bioarea + 1) ~
                              day +
                              disturbance +
                              day * disturbance +
                              (day || system_nr),
                            data = ds_regional %>%
                              filter(time_point >= 2) %>%
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

