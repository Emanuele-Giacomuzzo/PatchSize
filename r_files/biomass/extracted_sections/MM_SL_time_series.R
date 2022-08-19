## -----------------------------------------------------------------------------------------------------------------------------------------------------------
ds_regional %>%
  filter(time_point >= 2) %>%
  ggplot(aes(x = day,
             y = regional_mean_bioarea,
             group = day)) +
  geom_boxplot() +
  labs(title = "Without log transformation",
       x = "Day",
       y = "Regional bioarea (something/µl)")


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
linear_model = lm(regional_mean_bioarea ~ 
                    day, 
                  data = ds_regional %>% 
                            filter(time_point >= 2) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"))

par(mfrow=c(2,3))
plot(linear_model, which = 1:5)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
ds_regional %>%
  filter(time_point >= 2) %>%
  ggplot(aes(x = day,
             y = log(regional_mean_bioarea + 1),
             group = day)) +
  geom_boxplot() +
  labs(title = "With log transformation",
       x = "Day",
       y = "Log (regional bioarea + 1) (something/µl)")


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
log_linear_model = lm(log10(regional_mean_bioarea + 1) ~ 
                    day, 
                  data = ds_regional %>% 
                            filter(time_point >= 2) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"))

par(mfrow=c(2,3))
plot(log_linear_model, which = 1:5)
par(mfrow=c(1,1))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
full = lmer(log10(regional_mean_bioarea + 1) ~
                     day * metaecosystem_type * disturbance +
                     (day | system_nr),
                     data = ds_regional %>%
                            filter(time_point >= 2) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"),
                   REML = FALSE,
                   control = lmerControl(optimizer = "Nelder_Mead"))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
no_correlation = lmer(log10(regional_mean_bioarea + 1) ~
                     day * metaecosystem_type * disturbance +
                     (day | system_nr),
                     data = ds_regional %>%
                            filter(time_point >= 2) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"),
                   REML = FALSE,
                   control = lmerControl(optimizer = "Nelder_Mead"))

anova(full, no_correlation)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
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


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
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


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
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


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
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


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
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


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------
best_model = no_MD

R2_marginal = r.squaredGLMM(best_model)[1]
R2_marginal = round(R2_marginal, digits = 2)
R2_conditional = r.squaredGLMM(best_model)[2]
R2_conditional = round(R2_conditional, digits = 2)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
t2_t5 = lmer(log10(regional_mean_bioarea + 1) ~
                     day +
                     metaecosystem_type +
                     disturbance +
                     day : disturbance +
                     (day | system_nr),
                     data = ds_regional %>%
                            filter(time_point >= 2) %>%
                            filter(time_point <= 5) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"),
                   REML = FALSE,
                   control = lmerControl(optimizer = "Nelder_Mead"))

R2_marginal = r.squaredGLMM(t2_t5)[1]
R2_marginal = round(R2_marginal, digits = 2)
R2_conditional = r.squaredGLMM(t2_t5)[2]
R2_conditional = round(R2_conditional, digits = 2)

