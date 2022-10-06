## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
ds_regional_biomass %>%
  filter(time_point >= 2) %>%
  filter(!metaecosystem_type == "S_L_from_isolated") %>%
  ggplot(aes(x = day,
             y = total_regional_bioarea,
             group = day)) +
  geom_boxplot() +
  labs(x = "Day",
       y = "Regional bioarea (µm²)")


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
linear_model = lm(total_regional_bioarea ~ 
                    day, 
                  data = ds_regional_biomass %>% 
                            filter(time_point >= 2) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"))

par(mfrow=c(2,3))
plot(linear_model, which = 1:5)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
full = lmer(total_regional_bioarea ~
                     day * metaecosystem_type * disturbance +
                     (day | system_nr),
                     data = ds_regional_biomass %>%
                            filter(time_point >= 2) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"),
                   REML = FALSE,
                   control = lmerControl(optimizer = "Nelder_Mead"))


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
no_correlation = lmer(total_regional_bioarea ~
                     day * metaecosystem_type * disturbance +
                     (day || system_nr),
                     data = ds_regional_biomass %>%
                            filter(time_point >= 2) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"),
                   REML = FALSE,
                   control = lmerControl(optimizer = "Nelder_Mead"))

anova(full, no_correlation)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
no_random_slopes = lmer(total_regional_bioarea ~
                     day * metaecosystem_type * disturbance +
                     (1 | system_nr),
                     data = ds_regional_biomass %>%
                            filter(time_point >= 2) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"),
                   REML = FALSE,
                   control = lmerControl(optimizer = "Nelder_Mead"))

anova(no_correlation, no_random_slopes)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
no_threeway = lmer(total_regional_bioarea ~
                     day +
                     metaecosystem_type +
                     disturbance +
                     day : metaecosystem_type + 
                     day : disturbance +
                     metaecosystem_type : disturbance + 
                     (1 | system_nr),
                     data = ds_regional_biomass %>%
                            filter(time_point >= 2) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"),
                   REML = FALSE,
                   control = lmerControl(optimizer = 'optimx', 
                                         optCtrl = list(method = 'L-BFGS-B')))

anova(no_random_slopes, no_threeway)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
no_TM = lmer(total_regional_bioarea ~
                     day +
                     metaecosystem_type +
                     disturbance +
                     day : disturbance +
                     metaecosystem_type : disturbance + 
                     (1 | system_nr),
                     data = ds_regional_biomass %>%
                            filter(time_point >= 2) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"),
                   REML = FALSE,
                   control = lmerControl(optimizer = "Nelder_Mead"))

anova(no_threeway,no_TM)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
no_TD = lmer(total_regional_bioarea ~
                     day +
                     metaecosystem_type +
                     disturbance +
                     day : metaecosystem_type + 
                     metaecosystem_type : disturbance + 
                     (1 | system_nr),
                     data = ds_regional_biomass %>%
                            filter(time_point >= 2) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"),
                   REML = FALSE,
                   control = lmerControl(optimizer = "Nelder_Mead"))

anova(no_threeway, no_TD)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
no_MD = lmer(total_regional_bioarea ~
                     day +
                     metaecosystem_type +
                     disturbance +
                     day : metaecosystem_type + 
                     (1 | system_nr),
                     data = ds_regional_biomass %>%
                            filter(time_point >= 2) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"),
                   REML = FALSE,
                   control = lmerControl(optimizer = "Nelder_Mead"))

anova(no_TD, no_MD)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
best_model = no_MD


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot(best_model)
qqnorm(resid(best_model))


## ----warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------
R2_marginal = r.squaredGLMM(best_model)[1]
R2_marginal = round(R2_marginal, digits = 2)
R2_conditional = r.squaredGLMM(best_model)[2]
R2_conditional = round(R2_conditional, digits = 2)


## ----eval = FALSE--------------------------------------------------------------------------------------------------------------------------------------------------
## R2_regional = partR2(best_model,
##        partvars = c("day",
##                     "metaecosystem_type",
##                     "disturbance"),
##        R2_type = "conditional",
##        nboot = 1000,
##        CI = 0.95)
## saveRDS(R2_regional, file = here("results", "biomass", "R2_regional_t2t7.RData"))


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
R2_regional = readRDS(here("results", "biomass", "R2_regional_t2t7.RData"))
R2_regional$IR2


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
t2_t5 = lmer(total_regional_bioarea ~
                     day +
                     metaecosystem_type +
                     disturbance +
                     day : metaecosystem_type + 
                     (1 | system_nr),
                     data = ds_regional_biomass %>%
                            filter(time_point >= 2) %>%
                            filter(time_point <= 5) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"),
                   REML = FALSE,
                   control = lmerControl(optimizer = "Nelder_Mead"))

plot(t2_t5)
qqnorm(resid(t2_t5))

R2_marginal = r.squaredGLMM(t2_t5)[1]
R2_marginal = round(R2_marginal, digits = 2)
R2_conditional = r.squaredGLMM(t2_t5)[2]
R2_conditional = round(R2_conditional, digits = 2)


## ----eval = FALSE--------------------------------------------------------------------------------------------------------------------------------------------------
## R2_regional = partR2(t2_t5,
##        partvars = c("day",
##                     "metaecosystem_type",
##                     "disturbance"),
##        R2_type = "conditional",
##        nboot = 1000,
##        CI = 0.95)
## saveRDS(R2_regional, file = here("results", "biomass", "R2_regional_t2t5.RData"))


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
R2_regional = readRDS(here("results", "biomass", "R2_regional_t2t5.RData"))
R2_regional$IR2

