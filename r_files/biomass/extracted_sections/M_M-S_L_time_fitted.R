## ----regional biomass plot, echo = FALSE-------------------------------------------------------------------
ds_regional %>%
  filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L") %>%
  ggplot(aes(x = day,
             y = regional_mean_bioarea,
             group = day)) + 
  geom_boxplot() +
  labs(x = "day", y = "Regional bioarea (something/microlitres)")


## ----time-function-----------------------------------------------------------------------------------------

a1 = -0.1
a4 = 1200
a5 = -1

day = seq(0, 30, 0.01)
biomass = a4*(day-a5) * exp(a1*(day-a5))
plot(biomass ~ day)


## ----parameterise-time-function, results = FALSE, echo = FALSE---------------------------------------------

ds_regional_shrunk_type = ds_regional %>%
    filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L")

model = nls(regional_mean_bioarea ~ a4 * (day-a5) * exp(a1 * (day-a5)), 
            start = list(a1 = -0.1, a4 = 1200, a5 = -1),
            trace = T,
            data = ds_regional_shrunk_type)

a1 = as.numeric(model$m$getPars()[1])
a4 = as.numeric(model$m$getPars()[2])
a5 = as.numeric(model$m$getPars()[3])


## ----show-fitted-parameters--------------------------------------------------------------------------------
model$m$getPars()


## ----plot-parameterised-time-function----------------------------------------------------------------------

day = seq(0,30,1)
predicted = a4*(day-a5)*exp(a1*(day-a5))
data_fitted=data.frame(day=day,regional_mean_bioarea=predicted)

ds_regional_shrunk_type%>%
  ggplot(aes(x = day,
             y = regional_mean_bioarea,
             group = day)) + 
  geom_boxplot() +
  labs(x = "day", y = "regional bioarea") +
  geom_line(data=data_fitted,aes(x = day, y=regional_mean_bioarea),color="red", group = 1)


## ----predicted-ds------------------------------------------------------------------------------------------

ds_regional_predicted_shrunk_type = ds_regional %>%
  mutate(predicted_from_time = a4*(day-a5)*exp(a1*(day-a5))) %>%
  filter(metaecosystem_type == "S_L" | metaecosystem_type == "M_M")


## ----------------------------------------------------------------------------------------------------------
ds_regional_predicted_shrunk_type_n_day = ds_regional_predicted_shrunk_type %>%
  filter(time_point >= 2)


## ----eval = FALSE------------------------------------------------------------------------------------------
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


## ----time-fitted-random-effects----------------------------------------------------------------------------
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
               REML = FALSE,
               control = lmerControl (optimizer = "Nelder_Mead")
               )

model_2 = lmer(regional_mean_bioarea ~ 
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

anova(model_1, model_2)


## ----------------------------------------------------------------------------------------------------------
model_3 = lmer(regional_mean_bioarea ~ 
                            predicted_from_time + 
                            metaecosystem_type  + 
                            disturbance + 
                            metaecosystem_type * disturbance +
                            predicted_from_time * metaecosystem_type * disturbance +
                            (predicted_from_time || system_nr),
               data = ds_regional_predicted_shrunk_type_n_day,
               REML = FALSE,
               control = lmerControl (optimizer = "Nelder_Mead"))

anova(model_2, model_3)


## ----------------------------------------------------------------------------------------------------------
model_4 = lmer(regional_mean_bioarea ~ 
                            predicted_from_time + 
                            metaecosystem_type  + 
                            disturbance + 
                            predicted_from_time * metaecosystem_type * disturbance +
                            (predicted_from_time || system_nr),
               data = ds_regional_predicted_shrunk_type_n_day,
               REML = FALSE,
               control = lmerControl (optimizer = "Nelder_Mead"))

anova(model_3, model_4)


## ----------------------------------------------------------------------------------------------------------
model_5 = lmer(regional_mean_bioarea ~ 
                            predicted_from_time + 
                            metaecosystem_type  + 
                            disturbance + 
                            (predicted_from_time || system_nr),
               data = ds_regional_predicted_shrunk_type_n_day,
               REML = FALSE,
               control = lmerControl (optimizer = "Nelder_Mead"))

anova(model_4, model_5)


## ----------------------------------------------------------------------------------------------------------

#Take off disturbance slopes
model_8 = lmer(regional_mean_bioarea ~ 
                            predicted_from_time + 
                            metaecosystem_type  + 
                            disturbance + 
                            (1 | system_nr),
               data = ds_regional_predicted_shrunk_type_n_day,
               REML = FALSE,
               control = lmerControl (optimizer = "Nelder_Mead"))

anova(model_5, model_8)


## ----------------------------------------------------------------------------------------------------------

#Take off random effects
model_9 = lm(regional_mean_bioarea ~ 
                            predicted_from_time + 
                            metaecosystem_type  + 
                            disturbance,
                        data = ds_regional_predicted_shrunk_type_n_day)

anova(model_8, model_9)


## ----------------------------------------------------------------------------------------------------------

### --- INITIALISE TABLE --- ###

columns = c("model", "time_point", "AIC", "BIC", "mixed_r2", "mixed_r2_metaeco", "fixed_r2", "fixed_r2_metaeco")
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

### --- T + M + D + T*M*D --- ###

for (last_point in 3:7) {
  
  full_model = lm(regional_mean_bioarea ~ 
                    predicted_from_time +
                    metaecosystem_type +
                    disturbance +
                    predicted_from_time * metaecosystem_type * disturbance,
                    data = filter(ds_regional_predicted_shrunk_type_n_day, 
                                  time_point <= last_point))
  
  null_model = lm(regional_mean_bioarea ~ 
                    1 , 
                  data = filter(ds_regional_predicted_shrunk_type_n_day, 
                                time_point <= last_point))
  
  metaeco_null_model = lm(regional_mean_bioarea ~  
                                 predicted_from_time +
                                 disturbance +
                                 predicted_from_time * disturbance,
                                 data = filter(ds_regional_predicted_shrunk_type_n_day, 
                                               time_point <= last_point))
  
  fitted_time_table = update_all_models_table("T + M + D + T*M*D",
                                             fitted_time_table, 
                                             full_model, 
                                             null_model,
                                             metaeco_null_model,
                                             "fixed")
}

### --- T + M + D + T*M*D + (T|ID) --- ### PROBLEM!!!

for (last_point in 3:7) {
  
  full_model = lmer(regional_mean_bioarea ~ 
                      predicted_from_time +
                      metaecosystem_type +
                      disturbance +
                      predicted_from_time * metaecosystem_type * disturbance + 
                      (predicted_from_time || system_nr),
                    data = filter(ds_regional_predicted_shrunk_type_n_day, time_point <= last_point),
                    REML = FALSE,
                    control = lmerControl(optimizer ="Nelder_Mead"))
  
  null_model = lm(regional_mean_bioarea ~ 
                    1 , 
                  data = filter(ds_regional_predicted_shrunk_type_n_day, time_point <= last_point))
   
  metaeco_null = lmer(regional_mean_bioarea ~  
                        predicted_from_time +
                        disturbance +
                        predicted_from_time * disturbance + 
                        (predicted_from_time || system_nr),
                      data = filter(ds_regional_predicted_shrunk_type_n_day, 
                                               time_point <= last_point),
                      REML = FALSE, 
                      control = lmerControl(optimizer ="Nelder_Mead"))
  
  fitted_time_table = update_all_models_table("T + M + D + T*M*D + (T|ID)",
                                             random_time_table, 
                                             full_model, 
                                             null_model,
                                             metaeco_null,
                                             "mixed")
}

### --- T + M + D + (T|ID) --- ###

for (last_point in 3:7) {
  
  full_model = lmer(regional_mean_bioarea ~ 
                      predicted_from_time +
                      metaecosystem_type +
                      disturbance +
                      (predicted_from_time || system_nr),
                    data = filter(ds_regional_predicted_shrunk_type_n_day, time_point <= last_point),
                    REML = FALSE,
                    control = lmerControl(optimizer ="Nelder_Mead"))
  
  null_model = lm(regional_mean_bioarea ~ 
                    1 , 
                  data = filter(ds_regional_predicted_shrunk_type_n_day, time_point <= last_point))
   
  metaeco_null = lmer(regional_mean_bioarea ~  
                        predicted_from_time +
                        disturbance +
                        (predicted_from_time || system_nr),
                      data = filter(ds_regional_predicted_shrunk_type_n_day, 
                                               time_point <= last_point),
                      REML = FALSE, 
                      control = lmerControl(optimizer ="Nelder_Mead"))
  
  fitted_time_table = update_all_models_table("T + M + D + (T|ID)",
                                             random_time_table, 
                                             full_model, 
                                             null_model,
                                             metaeco_null,
                                             "mixed")
}



## ----echo = FALSE------------------------------------------------------------------------------------------
datatable(fitted_time_table, caption = "M = Meta-ecosystem type, D = disturbance, (1 | t) = random effect of time on the intercept, (1 | ID) = random effect of meta-ecosystem ID on the intercept, mixed_r2 = r squared when considering both fixed and random effects (conditional r squared), fixed_r2 = r squared when considering only the fixed effects (marginal r squared)")

