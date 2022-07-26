## ----full-models---------------------------------------------------------------------------------------------------------------------------------------------------------
#Uncorrelated intercepts and slopes
full_model = lmer(regional_mean_bioarea ~ 
                    metaecosystem_type  + 
                    disturbance + 
                    metaecosystem_type * disturbance + 
                    (metaecosystem_type || day) + 
                    (disturbance || day) + 
                    (metaecosystem_type*disturbance  || day),
                  data = ds_regional_shrunk_type_n_day, 
                  REML = FALSE)

#Correlated intercepts and slopes
full_model_correlated = lmer(regional_mean_bioarea ~ 
                               metaecosystem_type  + 
                               disturbance + 
                               metaecosystem_type * disturbance + 
                               (metaecosystem_type | day) + 
                               (disturbance | day) + 
                               (metaecosystem_type*disturbance  | day), 
                             data = ds_regional_shrunk_type_n_day, 
                             REML = FALSE)

anova(full_model, full_model_correlated)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
best_model = full_model


## ----fixed_effects_model-------------------------------------------------------------------------------------------------------------------------------------------------
fixed_effects_model = lm(regional_mean_bioarea ~ 
                           metaecosystem_type  + 
                           disturbance +
                           metaecosystem_type * disturbance, 
                         data = ds_regional_shrunk_type_n_day)

anova(best_model, fixed_effects_model)


## ----no_interaction_model, message=FALSE---------------------------------------------------------------------------------------------------------------------------------
no_interaction_model = lmer(regional_mean_bioarea ~ 
                              metaecosystem_type + 
                              disturbance  + 
                              (metaecosystem_type || day) + 
                              (disturbance || day), 
                            data = ds_regional_shrunk_type_n_day, 
                            REML = FALSE)

anova(best_model, no_interaction_model)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
best_model = no_interaction_model


## ----no_slopes_model, message=FALSE--------------------------------------------------------------------------------------------------------------------------------------
no_slopes_model = lmer(regional_mean_bioarea ~ 
                         metaecosystem_type + 
                         disturbance  + 
                         (1 | day), 
                       data = ds_regional_shrunk_type_n_day, 
                       REML = FALSE)

anova(best_model, no_slopes_model)


## ----best-model----------------------------------------------------------------------------------------------------------------------------------------------------------
best_model = no_slopes_model


## ----r2-p-best-model-----------------------------------------------------------------------------------------------------------------------------------------------------

model.null = lm(regional_mean_bioarea ~ 1 , 
                  data = ds_regional_shrunk_type_n_day)

r2_best = r.squaredGLMM(best_model)
r2_best = round(r2_best, digits = 3)

anova = anova(best_model, model.null)
p_best = anova$`Pr(>Chisq)`[2]
p_best = round(p_best, digits = 5)

if (p_best < 0.00001) {
  p_best = "< 0.00001"
}



## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
metaecosystem_type_null = lmer(regional_mean_bioarea ~  
                                 disturbance  + 
                                 (1 | day),
                               data = ds_regional_shrunk_type_n_day, 
                               REML = FALSE, 
                               control = lmerControl(optimizer ="Nelder_Mead"))

r2_no_metaecosystem_type = r.squaredGLMM(metaecosystem_type_null)
r2_no_metaecosystem_type = round(r2_no_metaecosystem_type, digits = 3)

anova = anova(best_model, metaecosystem_type_null)
p_no_metaecosystem_type = anova$`Pr(>Chisq)`[2]
p_no_metaecosystem_type = round(p_no_metaecosystem_type, digits = 5)

if (p_no_metaecosystem_type < 0.00001) {
  p_no_metaecosystem_type = "< 0.00001"
}



## ----p-r2-disturbance----------------------------------------------------------------------------------------------------------------------------------------------------

disturbance_null = lmer(regional_mean_bioarea ~ 
                          metaecosystem_type  + 
                          (1 | day), 
                        data = ds_regional_shrunk_type_n_day, 
                        REML = FALSE,
                        control = lmerControl(optimizer ='optimx', 
                                                     optCtrl=list(method='L-BFGS-B')))

r2_no_disturbance = r.squaredGLMM(disturbance_null)
r2_no_disturbance = round(r2_no_disturbance, digits = 3)

anova = anova(best_model, disturbance_null)
p_no_disturbance = anova$`Pr(>Chisq)`[2]
p_no_disturbance = round(p_no_disturbance, digits = 5)

if (p_no_disturbance < 0.00001) {
  p_no_disturbance = "< 0.00001"
}


