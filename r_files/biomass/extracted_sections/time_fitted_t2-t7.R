## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
ds_regional_predicted_shrunk_type_n_day = ds_regional_predicted_shrunk_type %>%
  filter(time_point >= 2)


## ----eval = FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------
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


## ----time-fitted-random-effects-----------------------------------------------------------------------------------------------------------------------------------------
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
                        REML = FALSE)

model_2 = lmer(regional_mean_bioarea ~ 
                            predicted_from_time + 
                            metaecosystem_type  + 
                            disturbance + 
                            predicted_from_time * disturbance +
                            metaecosystem_type * disturbance +
                            predicted_from_time * metaecosystem_type * disturbance +
                            (predicted_from_time || system_nr),
                        data = ds_regional_predicted_shrunk_type_n_day,
                        REML = FALSE)

anova(model_1, model_2)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
model_3 = lmer(regional_mean_bioarea ~ 
                            predicted_from_time + 
                            metaecosystem_type  + 
                            disturbance + 
                            metaecosystem_type * disturbance +
                            predicted_from_time * metaecosystem_type * disturbance +
                            (predicted_from_time || system_nr),
                        data = ds_regional_predicted_shrunk_type_n_day,
                        REML = FALSE)

anova(model_2, model_3)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
model_4 = lmer(regional_mean_bioarea ~ 
                            predicted_from_time + 
                            metaecosystem_type  + 
                            disturbance + 
                            predicted_from_time * metaecosystem_type * disturbance +
                            (predicted_from_time || system_nr),
                        data = ds_regional_predicted_shrunk_type_n_day,
                        REML = FALSE)

anova(model_3, model_4)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
model_5 = lmer(regional_mean_bioarea ~ 
                            predicted_from_time + 
                            metaecosystem_type  + 
                            disturbance + 
                            (predicted_from_time || system_nr),
                        data = ds_regional_predicted_shrunk_type_n_day,
                        REML = FALSE)

anova(model_4, model_5)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Take off disturbance slopes
model_8 = lmer(regional_mean_bioarea ~ 
                            predicted_from_time + 
                            metaecosystem_type  + 
                            disturbance + 
                            (1 | system_nr),
                        data = ds_regional_predicted_shrunk_type_n_day,
                        REML = FALSE)

anova(model_5, model_8)



## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Take off random effects
model_9 = lm(regional_mean_bioarea ~ 
                            predicted_from_time + 
                            metaecosystem_type  + 
                            disturbance,
                        data = ds_regional_predicted_shrunk_type_n_day)

anova(model_8, model_9)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------

best_model = model_9

summary(best_model)
#R squared: 0.795

#Take off metaecosystem type
best_model_without_metaeco_type = lm(regional_mean_bioarea ~ 
                            predicted_from_time + 
                            disturbance,
                        data = ds_regional_predicted_shrunk_type_n_day)

anova(best_model, best_model_without_metaeco_type)
#they are different.
summary(best_model_without_metaeco_type)
#R squared: 0.718

R2_metaecosystem = summary(best_model)$adj.r.squared - summary(best_model_without_metaeco_type)$adj.r.squared
R2_metaecosystem = round(R2_metaecosystem, digits = 3)

