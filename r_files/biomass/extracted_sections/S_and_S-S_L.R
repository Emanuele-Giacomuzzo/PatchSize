## ---- echo = FALSE-------------------------------------------------------------------------------------------------------------------------------------------
ds %>%
  filter (eco_metaeco_type == "S" | 
            eco_metaeco_type == "S (S_L)") %>%
  filter(disturbance == "low") %>%
  ggplot(aes(x = day,
             y = bioarea_per_volume,
             group= interaction(day, eco_metaeco_type),
             fill = eco_metaeco_type,
             color = eco_metaeco_type)) +
  geom_boxplot() +
  labs(x = "Day", 
       y = "Local biomass (bioarea/µl)", 
       color = '', 
       fill = '',
       title = "Disturbance = low") +
  scale_fill_discrete(labels = c("Isolated", 
                                 "Connected to larger patch")) +
  scale_color_discrete(labels = c("Isolated", 
                                  "Connected to larger patch")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))

ds %>%
  filter (eco_metaeco_type == "S" | 
            eco_metaeco_type == "S (S_L)") %>%
  filter(disturbance == "high") %>%
  ggplot(aes(x = day,
             y = bioarea_per_volume,
             group= interaction(day, eco_metaeco_type),
             fill = eco_metaeco_type,
             color = eco_metaeco_type)) +
  geom_boxplot() +
  labs(x = "Day", 
       y = "Local biomass (bioarea/µl)", 
       color = '', 
       fill = '',
       title = "Disturbance = high") +
  scale_fill_discrete(labels = c("Isolated", 
                                 "Connected to larger patch")) +
  scale_color_discrete(labels = c("Isolated", 
                                  "Connected to larger patch")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))




## ------------------------------------------------------------------------------------------------------------------------------------------------------------
ds_local_S_t2_t7 = ds %>%
  filter (eco_metaeco_type == "S" | 
          eco_metaeco_type == "S (S_L)") %>%
  filter(time_point >= 2) 


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
full_model = lmer(bioarea_per_volume ~ 
                    metaecosystem_type  + 
                    disturbance + 
                    metaecosystem_type * disturbance + 
                    (metaecosystem_type || day) + 
                    (disturbance || day) + 
                    (metaecosystem_type*disturbance  || day),
                  data = ds_local_S_t2_t7, 
                  REML = FALSE)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
fixed_effects_model = lm(bioarea_per_volume ~ 
                           metaecosystem_type  + 
                           disturbance +
                           metaecosystem_type * disturbance, 
                         data = ds_local_S_t2_t7)

anova(full_model, fixed_effects_model)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
best_model = full_model


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
no_interaction_model = lmer(bioarea_per_volume ~ 
                              metaecosystem_type + 
                              disturbance  + 
                              (metaecosystem_type || day) + 
                              (disturbance || day), 
                            data = ds_local_S_t2_t7, 
                            REML = FALSE)

anova(best_model, no_interaction_model)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
best_model = no_interaction_model


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
no_metaeco_slopes_model = lmer(bioarea_per_volume ~ 
                         metaecosystem_type + 
                         disturbance  + 
                         (disturbance || day), 
                       data = ds_local_S_t2_t7, 
                       REML = FALSE,
                       control = lmerControl (optimizer = "Nelder_Mead")
                       )

anova(best_model, no_metaeco_slopes_model)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
no_disturbance_slopes_model = lmer(bioarea_per_volume ~ 
                         metaecosystem_type + 
                         disturbance  + 
                         (1 | day), 
                       data = ds_local_S_t2_t7, 
                       REML = FALSE,
                       control = lmerControl (optimizer = "Nelder_Mead")
                       )

anova(best_model, no_disturbance_slopes_model)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
best_model = no_disturbance_slopes_model
summary(best_model)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
model.null = lm(bioarea_per_volume ~ 
                  1, 
                  data = ds_local_S_t2_t7)

r2_best = r.squaredGLMM(best_model)
r2_best = round(r2_best, digits = 3)

anova = anova(best_model, model.null)
p_best = anova$`Pr(>Chisq)`[2]
p_best = round(p_best, digits = 5)

if (p_best < 0.00001) {
  p_best = "< 0.00001"
}


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
metaecosystem_type_null = lmer(bioarea_per_volume ~  
                                 disturbance  + 
                                 (1 | day),
                               data = ds_local_S_t2_t7, 
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


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
disturbance_null = lmer(bioarea_per_volume ~ 
                          metaecosystem_type  + 
                          (1 | day), 
                        data = ds_local_S_t2_t7, 
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


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
R2 = NULL
for (last_point in 3:7) {
  
  filtered_dataset = ds_local_S_t2_t7 %>%
  filter(time_point <= last_point)
  
  full_model = lmer(bioarea_per_volume ~ 
                      disturbance +
                      metaecosystem_type + 
                      (1 | day),
                    data = filtered_dataset,
                    REML = FALSE)
  
  no_metaeco_type_model = lmer(bioarea_per_volume ~ 
                               disturbance +
                               (1 | day),
                             data = filtered_dataset,
                             REML = FALSE)
  
  anova(full_model, no_metaeco_type_model)
  
  R2_full = r.squaredGLMM(full_model)[1,1]
  R2_null = r.squaredGLMM(no_metaeco_type_model)[1,1]
  
  R2[[last_point]] =  R2_full - R2_null
  R2[[last_point]] = round(R2[[last_point]], digits = 3)
  
}

