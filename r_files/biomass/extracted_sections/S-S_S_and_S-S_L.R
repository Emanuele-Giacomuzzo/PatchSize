## ---- echo = FALSE-----------------------------------------------------------------------------------------
### --- BOXPLOTS --- ###
local_low_boxplots = ds %>%
  filter (eco_metaeco_type == "S (S_S)" |
            eco_metaeco_type == "S (S_L)") %>%
  filter(disturbance == "low") %>%
  ggplot(aes(x = day,
             y = bioarea_per_volume,
             group = interaction(day, eco_metaeco_type),
             fill = eco_metaeco_type,
             color = eco_metaeco_type)) +
  geom_boxplot() + 
  labs(x = "Day", 
       y = "Local bioarea (something/µl)", 
       color = '', 
       fill = '',
       title = "Disturbance = low") +
  scale_fill_manual(values=c("#1b9e77", "#d95f02"),
                    labels = c("Connected to same size", "Connected to larger size")) +
  scale_color_manual(values=c("#1b9e77", "#d95f02"),
                     labels = c("Connected to same size", "Connected to larger size")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))
local_low_boxplots

local_high_boxplots = ds %>%
  filter (eco_metaeco_type == "S (S_S)" |
            eco_metaeco_type == "S (S_L)") %>%
  filter(disturbance == "high") %>%
  ggplot(aes(x = day,
             y = bioarea_per_volume,
             group = interaction(day, eco_metaeco_type),
             fill = eco_metaeco_type,
             color = eco_metaeco_type)) +
  geom_boxplot() + 
  labs(x = "Day", 
       y = "Local bioarea (something/µl)", 
       color = '', 
       fill = '',
       title = "Disturbance = high") +
  scale_fill_manual(values=c("#1b9e77", "#d95f02"),
                    labels = c("Connected to same size", "Connected to larger size")) +
  scale_color_manual(values=c("#1b9e77", "#d95f02"),
                     labels = c("Connected to same size", "Connected to larger size")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))
local_high_boxplots

### --- MEAN --- ###

local_low_mean = ds %>%
  group_by(eco_metaeco_type, disturbance, day) %>%
  summarise(mean_bioarea_per_volume = mean(bioarea_per_volume), 
            sd_bioarea_per_volume = sd(bioarea_per_volume)) %>%
  filter (eco_metaeco_type == "S (S_S)" |
            #eco_metaeco_type == "S" | 
            eco_metaeco_type == "S (S_L)") %>%
  filter(disturbance == "low") %>%
  ggplot(aes(x = day,
             y = mean_bioarea_per_volume,
             fill = eco_metaeco_type,
             color = eco_metaeco_type)) +
  geom_line (stat = "summary", 
             fun = "mean", 
             position=position_dodge(width=0.5)) +
  geom_pointrange(aes(ymin = mean_bioarea_per_volume - sd_bioarea_per_volume, 
                      ymax = mean_bioarea_per_volume + sd_bioarea_per_volume),
                  position=position_dodge(width=0.5)) +
  labs(x = "Day", 
       y = "Local bioarea (something/µl)", 
       color = '', 
       fill = '',
       title = "Disturbance = low") +
  scale_fill_manual(values=c("#1b9e77", "#d95f02"),
                    labels = c("Connected to same size", "Connected to larger size")) +
  scale_color_manual(values=c("#1b9e77", "#d95f02"),
                     labels = c("Connected to same size", "Connected to larger size")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))
local_low_mean

local_high_mean = ds %>%
  group_by(eco_metaeco_type, disturbance, day) %>%
  summarise(mean_bioarea_per_volume = mean(bioarea_per_volume), 
            sd_bioarea_per_volume = sd(bioarea_per_volume)) %>%
  filter (eco_metaeco_type == "S (S_S)" |
            #eco_metaeco_type == "S" | 
            eco_metaeco_type == "S (S_L)") %>%
  filter(disturbance == "high") %>%
  ggplot(aes(x = day,
             y = mean_bioarea_per_volume,
             fill = eco_metaeco_type,
             color = eco_metaeco_type)) +
  geom_line (stat = "summary", 
             fun = "mean", 
             position=position_dodge(width=0.5)) +
  geom_pointrange(aes(ymin = mean_bioarea_per_volume - sd_bioarea_per_volume, 
                      ymax = mean_bioarea_per_volume + sd_bioarea_per_volume),
                  position=position_dodge(width=0.5)) +
  labs(x = "Day", 
       y = "Local bioarea (something/µl)", 
       color = '', 
       fill = '',
       title = "Disturbance = low") +
  scale_fill_manual(values=c("#1b9e77", "#d95f02"),
                    labels = c("Connected to same size", "Connected to larger size")) +
  scale_color_manual(values=c("#1b9e77", "#d95f02"),
                     labels = c("Connected to same size", "Connected to larger size")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))
local_high_mean


## ----------------------------------------------------------------------------------------------------------
ds_local_S_t2_t7 = ds %>%
  filter (eco_metaeco_type == "S (S_S)" | 
          eco_metaeco_type == "S (S_L)") %>%
  filter(time_point >= 2) 


## ----------------------------------------------------------------------------------------------------------
full_model = lmer(bioarea_per_volume ~ 
                    metaecosystem_type  + 
                    disturbance + 
                    metaecosystem_type * disturbance + 
                    (metaecosystem_type || day) + 
                    (disturbance || day) + 
                    (metaecosystem_type*disturbance  || day),
                  data = ds_local_S_t2_t7, 
                  REML = FALSE)


## ----------------------------------------------------------------------------------------------------------
fixed_effects_model = lm(bioarea_per_volume ~ 
                           metaecosystem_type  + 
                           disturbance +
                           metaecosystem_type * disturbance, 
                         data = ds_local_S_t2_t7)

anova(full_model, fixed_effects_model)


## ----------------------------------------------------------------------------------------------------------
best_model = full_model


## ----------------------------------------------------------------------------------------------------------
no_interaction_model = lmer(bioarea_per_volume ~ 
                              metaecosystem_type + 
                              disturbance  + 
                              (metaecosystem_type || day) + 
                              (disturbance || day), 
                            data = ds_local_S_t2_t7, 
                            REML = FALSE)

anova(best_model, no_interaction_model)


## ----------------------------------------------------------------------------------------------------------
best_model = no_interaction_model


## ----------------------------------------------------------------------------------------------------------
no_metaeco_slopes_model = lmer(bioarea_per_volume ~ 
                         metaecosystem_type + 
                         disturbance  + 
                         (disturbance || day), 
                       data = ds_local_S_t2_t7, 
                       REML = FALSE,
                       control = lmerControl (optimizer = "Nelder_Mead")
                       )

anova(best_model, no_metaeco_slopes_model)


## ----------------------------------------------------------------------------------------------------------
no_disturbance_slopes_model = lmer(bioarea_per_volume ~ 
                         metaecosystem_type + 
                         disturbance  + 
                         (1 | day), 
                       data = ds_local_S_t2_t7, 
                       REML = FALSE,
                       control = lmerControl (optimizer = "Nelder_Mead")
                       )

anova(best_model, no_disturbance_slopes_model)


## ----------------------------------------------------------------------------------------------------------
best_model = no_disturbance_slopes_model
summary(best_model)


## ----------------------------------------------------------------------------------------------------------
#Effect size of the whole model
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

#Effect size and significance of meta-ecosystem type
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

#Effect size of disturbance 
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


## ----------------------------------------------------------------------------------------------------------
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


## ----------------------------------------------------------------------------------------------------------

full = lmer(bioarea_per_volume ~ 
                    metaecosystem_type  + 
                    disturbance + 
                    metaecosystem_type * disturbance + #1
                    (metaecosystem_type || day) + #2
                    (disturbance || day) + #3
                    (metaecosystem_type * disturbance  || day), #4
            data = ds_local_S_t2_t7, 
            REML = FALSE,
            control = lmerControl(optimizer = 'optimx', optCtrl = list(method = 'L-BFGS-B')))

m1 = lmer(bioarea_per_volume ~ 
                    metaecosystem_type  + 
                    disturbance + 
                    (metaecosystem_type || day) + #2
                    (disturbance || day) + #3
                    (metaecosystem_type*disturbance  || day), #4
          data = ds_local_S_t2_t7, 
          REML = FALSE,
          control = lmerControl(optimizer = 'optimx', optCtrl = list(method = 'L-BFGS-B')))

m2 = lmer(bioarea_per_volume ~ 
                    metaecosystem_type  + 
                    disturbance + 
                    metaecosystem_type * disturbance + #1
                    (disturbance || day) + #3
                    (metaecosystem_type*disturbance  || day), #4
          data = ds_local_S_t2_t7, 
          REML = FALSE,
          control = lmerControl(optimizer = 'optimx', optCtrl = list(method = 'L-BFGS-B')))

m3 = lmer(bioarea_per_volume ~ 
                    metaecosystem_type  + 
                    disturbance + 
                    metaecosystem_type * disturbance + #1
                    (metaecosystem_type || day) + #2
                    (metaecosystem_type*disturbance  || day), #4
          data = ds_local_S_t2_t7, 
          REML = FALSE,
          control = lmerControl(optimizer = 'optimx', optCtrl = list(method = 'L-BFGS-B')))

m4 = lmer(bioarea_per_volume ~ 
                    metaecosystem_type  + 
                    disturbance + 
                    metaecosystem_type * disturbance + #1
                    (metaecosystem_type || day) + #2
                    (disturbance || day), #3
            data = ds_local_S_t2_t7, 
            REML = FALSE,
            control = lmerControl(optimizer = 'optimx', optCtrl = list(method = 'L-BFGS-B')))

m5 = lmer(bioarea_per_volume ~ 
                    metaecosystem_type  + 
                    disturbance + 
                    (disturbance || day), #3
            data = ds_local_S_t2_t7, 
            REML = FALSE,
            control = lmerControl (optimizer = "Nelder_Mead"))

m6 = lmer(bioarea_per_volume ~ 
                    metaecosystem_type  + 
                    disturbance + 
                    (metaecosystem_type || day), #2
            data = ds_local_S_t2_t7, 
            REML = FALSE,
            control = lmerControl(optimizer = 'optimx', optCtrl = list(method = 'L-BFGS-B')))

m7 = lmer(bioarea_per_volume ~ 
                    metaecosystem_type  + 
                    disturbance + 
                    (metaecosystem_type || day) + #2
                    (disturbance || day), #3
            data = ds_local_S_t2_t7, 
            REML = FALSE,
            control = lmerControl(optimizer = 'optimx', optCtrl = list(method = 'L-BFGS-B')))

m8 = lmer(bioarea_per_volume ~ 
                    metaecosystem_type  + 
                    disturbance + 
                    metaecosystem_type * disturbance + #1
                    (metaecosystem_type*disturbance  || day), #4
            data = ds_local_S_t2_t7, 
            REML = FALSE,
            control = lmerControl(optimizer = 'optimx', optCtrl = list(method = 'L-BFGS-B')))

m9 = lmer(bioarea_per_volume ~ 
                    metaecosystem_type  + 
                    disturbance + 
                    metaecosystem_type * disturbance + #1
                    (disturbance || day), #3
            data = ds_local_S_t2_t7, 
            REML = FALSE,
            control = lmerControl(optimizer = 'optimx', optCtrl = list(method = 'nlminb')))

m10 = lmer(bioarea_per_volume ~ 
                    metaecosystem_type  + 
                    disturbance + 
                    metaecosystem_type * disturbance + #1
                    (metaecosystem_type || day), #2
            data = ds_local_S_t2_t7, 
            REML = FALSE,
            control = lmerControl(optimizer = 'optimx', optCtrl = list(method = 'L-BFGS-B')))

m11 = lm(bioarea_per_volume ~ 
                    metaecosystem_type  + 
                    disturbance +
                    metaecosystem_type * disturbance, #1
            data = ds_local_S_t2_t7
)

m12 = lmer(bioarea_per_volume ~ 
                    metaecosystem_type  + 
                    disturbance + 
                    (metaecosystem_type || day), #2
            data = ds_local_S_t2_t7, 
            REML = FALSE,
            control = lmerControl(optimizer = 'optimx', optCtrl = list(method = 'L-BFGS-B')))

m13 = lmer(bioarea_per_volume ~ 
                    metaecosystem_type  + 
                    disturbance + 
                    (disturbance || day), #3
            data = ds_local_S_t2_t7, 
            REML = FALSE,
            control = lmerControl (optimizer = "Nelder_Mead"))

m14 = lm(bioarea_per_volume ~ 
                    metaecosystem_type  + 
                    disturbance,
            data = ds_local_S_t2_t7
)

anova(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, full)

