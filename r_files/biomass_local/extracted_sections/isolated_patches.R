## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
ds_biomass_abund %>%
  filter ( disturbance == "low") %>%
  filter(metaecosystem == "no") %>%
  group_by (system_nr, day, patch_size) %>%
  summarise(mean_bioarea_per_volume_across_videos = mean(bioarea_per_volume)) %>%
  ggplot (aes(x = day,
                y = mean_bioarea_per_volume_across_videos,
                group = system_nr,
                fill = system_nr,
              color = system_nr,
                linetype = patch_size)) +
    geom_line () +
    labs(x = "Day", 
         y = "Regional bioarea (µm²/µl)",
         title = "Disturbance = low",
         fill = "System nr",
         linetype = "") +
    scale_y_continuous(limits = c(0, 6250)) +
    scale_x_continuous(limits = c(-2, 30)) +
  scale_colour_continuous(guide = "none") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.position = c(.95, .95),
          legend.justification = c("right", "top"),
          legend.box.just = "right",
          legend.margin = margin(6, 6, 6, 6)) +
  scale_linetype_discrete(labels = c("large isolated",
                                     "medium isolated",
                                     "small isolated"))

ds_biomass_abund %>%
  filter ( disturbance == "high") %>%
  filter(metaecosystem == "no") %>%
  group_by (system_nr, day, patch_size) %>%
  summarise(mean_bioarea_per_volume_across_videos = mean(bioarea_per_volume)) %>%
  ggplot (aes(x = day,
                y = mean_bioarea_per_volume_across_videos,
                group = system_nr,
                fill = system_nr,
              color = system_nr,
                linetype = patch_size)) +
    geom_line () +
    labs(x = "Day", 
         y = "Regional bioarea (µm²/µl)",
         title = "Disturbance = low",
         fill = "System nr",
         linetype = "") +
    scale_y_continuous(limits = c(0, 6250)) +
    scale_x_continuous(limits = c(-2, 30)) +
  scale_colour_continuous(guide = "none") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.position = c(.95, .95),
          legend.justification = c("right", "top"),
          legend.box.just = "right",
          legend.margin = margin(6, 6, 6, 6)) +
  scale_linetype_discrete(labels = c("large isolated",
                                     "medium isolated",
                                     "small isolated"))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
ds_biomass_abund %>%
  filter(disturbance == "low") %>%
  filter(metaecosystem == "no") %>%
  ggplot(aes(x = day,
             y = bioarea_per_volume,
             group = interaction(day, patch_size),
             fill = patch_size)) +
  geom_boxplot() + 
  labs(title = "Disturbance = low",
       x = "Day",
       y = "Local bioarea (µm²/μl)",
       fill = "") + 
  scale_fill_discrete(labels = c("isolated large", "isolated medium", "isolated small")) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.position = c(.95, .95),
          legend.justification = c("right", "top"),
          legend.box.just = "right",
          legend.margin = margin(6, 6, 6, 6))

ds_biomass_abund %>%
  filter(disturbance == "high") %>%
  filter(metaecosystem == "no") %>%
  ggplot(aes(x = day,
             y = bioarea_per_volume,
             group = interaction(day, patch_size),
             fill = patch_size)) +
  geom_boxplot() + 
  labs(title = "Disturbance = high",
       x = "Day",
       y = "Local bioarea (µm²/μl)",
       fill = "") + 
  scale_fill_discrete(labels = c("isolated large", "isolated medium", "isolated small")) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.position = c(.95, .95),
          legend.justification = c("right", "top"),
          legend.box.just = "right",
          legend.margin = margin(6, 6, 6, 6))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
ds_biomass_abund %>%
  filter(time_point >= 2) %>%
  ggplot(aes(x = day,
             y = bioarea_per_volume,
             group = day)) +
  geom_boxplot() +
  labs(x = "Day",
       y = "Regional bioarea (µm²)")


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
linear_model = lm(bioarea_per_volume ~ 
                    day, 
                  data = ds_biomass_abund %>% 
                            filter(time_point >= 2) %>%
                            filter(metaecosystem == "no"))

par(mfrow=c(2,3))
plot(linear_model, which = 1:5)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
full = lmer(bioarea_per_volume ~
                     day * patch_size * disturbance +
                     (day | system_nr),
                     data = ds_biomass_abund %>%
                            filter(time_point >= 2) %>%
                            filter(metaecosystem == "no"),
                   REML = FALSE,
                   control = lmerControl(optimizer = "Nelder_Mead"))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
no_correlation = lmer(bioarea_per_volume ~
                     day * patch_size * disturbance +
                     (day || system_nr),
                     data = ds_biomass_abund %>%
                            filter(time_point >= 2) %>%
                            filter(metaecosystem == "no"),
                   REML = FALSE,
                   control = lmerControl(optimizer = "Nelder_Mead"))

anova(full, no_correlation)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
no_random_slopes = lmer(bioarea_per_volume ~
                     day * patch_size * disturbance +
                     (1 | system_nr),
                     data = ds_biomass_abund %>%
                            filter(time_point >= 2) %>%
                            filter(metaecosystem == "no"),
                   REML = FALSE,
                   control = lmerControl(optimizer = "Nelder_Mead"))

anova(no_correlation, no_random_slopes)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
no_threeway = lmer(bioarea_per_volume ~
                     day +
                     patch_size +
                     disturbance +
                     day : patch_size + 
                     day : disturbance +
                     patch_size : disturbance + 
                     (1 | system_nr),
                     data = ds_biomass_abund %>%
                            filter(time_point >= 2) %>%
                            filter(metaecosystem == "no"),
                   REML = FALSE,
                   control = lmerControl(optimizer = 'optimx', 
                                         optCtrl = list(method = 'L-BFGS-B')))

anova(no_random_slopes, no_threeway)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
no_TM = lmer(bioarea_per_volume ~
                     day +
                     patch_size +
                     disturbance +
                     day : disturbance +
                     patch_size : disturbance + 
                     (1 | system_nr),
                     data = ds_biomass_abund %>%
                            filter(time_point >= 2) %>%
                            filter(metaecosystem == "no"),
                   REML = FALSE,
                   control = lmerControl(optimizer = "Nelder_Mead"))

anova(no_threeway,no_TM)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
no_TD = lmer(bioarea_per_volume ~
                     day +
                     patch_size +
                     disturbance +
                     day : patch_size + 
                     patch_size : disturbance + 
                     (1 | system_nr),
                     data = ds_biomass_abund %>%
                            filter(time_point >= 2) %>%
                            filter(metaecosystem == "no"),
                   REML = FALSE,
                   control = lmerControl(optimizer = "Nelder_Mead"))

anova(no_threeway, no_TD)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
no_MD = lmer(bioarea_per_volume ~
                     day +
                     patch_size +
                     disturbance +
                     day : patch_size + 
                     (1 | system_nr),
                     data = ds_biomass_abund %>%
                            filter(time_point >= 2) %>%
                            filter(metaecosystem == "no"),
                   REML = FALSE,
                   control = lmerControl(optimizer = "Nelder_Mead"))

anova(no_TD, no_MD)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
best_model = no_MD


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
plot(best_model)
qqnorm(resid(best_model))


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
R2_marginal = r.squaredGLMM(best_model)[1]
R2_marginal = round(R2_marginal, digits = 2)
R2_conditional = r.squaredGLMM(best_model)[2]
R2_conditional = round(R2_conditional, digits = 2)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
t2_t5 = lmer(bioarea_per_volume ~
                     day +
                     patch_size +
                     disturbance +
                     day : patch_size + 
                     (1 | system_nr),
                     data = ds_biomass_abund %>%
                            filter(time_point >= 2) %>%
                            filter(time_point <= 5) %>%
                            filter(metaecosystem == "no"),
                   REML = FALSE,
                   control = lmerControl(optimizer = "Nelder_Mead"))

plot(t2_t5)
qqnorm(resid(t2_t5))

R2_marginal = r.squaredGLMM(t2_t5)[1]
R2_marginal = round(R2_marginal, digits = 2)
R2_conditional = r.squaredGLMM(t2_t5)[2]
R2_conditional = round(R2_conditional, digits = 2)


## ----eval = FALSE-------------------------------------------------------------------------------------------------------------------------------------------------
## 
## ### --- Work in progress: calculating R2 of M --- ###
## 
## R2_regional = partR2(best_model,
##        partvars = c("day",
##                     "patch_size",
##                     "disturbance"),
##        R2_type = "marginal",
##        nboot = 1000,
##        CI = 0.95)
## saveRDS(R2_regional, file = here("results", "biomass", "R2_regional.RData"))


## ----eval = FALSE-------------------------------------------------------------------------------------------------------------------------------------------------
## readRDS(here("results", "biomass", "R2_regional.RData"))

