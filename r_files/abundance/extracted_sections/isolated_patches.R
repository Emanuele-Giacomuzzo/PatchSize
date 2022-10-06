## ----abundance-isolated-patches-single-ecosystem-plots-------------------------------------------------------------------------------------------------------------
for (disturbance_input in c("low", "high")){
  
  print(ds_biomass_abund %>%
  filter ( disturbance == disturbance_input) %>%
  filter(metaecosystem == "no") %>%
  group_by (system_nr, day, patch_size) %>%
  summarise(mean_indiv_per_volume_across_videos = mean(indiv_per_volume)) %>%
  ggplot (aes(x = day,
                y = mean_indiv_per_volume_across_videos,
                group = system_nr,
                fill = system_nr,
              color = system_nr,
                linetype = patch_size)) +
    geom_line () +
    labs(title = paste("Disturbance =", disturbance_input),
         x = "Day", 
         y = "Community density (individuals/µl)",
         fill = "System nr",
         linetype = "") +
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
                                     "small isolated")))}


## ----abundance-isolated-patches-boxplots---------------------------------------------------------------------------------------------------------------------------
for (disturbance_input in c("low", "high")){
print(ds_biomass_abund %>%
  filter(disturbance == disturbance_input) %>%
  filter(metaecosystem == "no") %>%
  ggplot(aes(x = day,
             y = indiv_per_volume,
             group = interaction(day, patch_size),
             fill = patch_size)) +
  geom_boxplot() + 
  labs(title = paste("Disturbance =", disturbance_input),
       x = "Day",
       y = "Community density (individuals/μl)",
       fill = "") + 
  scale_fill_discrete(labels = c("isolated large", 
                                 "isolated medium", 
                                 "isolated small")) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.position = c(.95, .95),
          legend.justification = c("right", "top"),
          legend.box.just = "right",
          legend.margin = margin(6, 6, 6, 6)))}


## ----abundance-isolated-patches-plot-linearity---------------------------------------------------------------------------------------------------------------------
ds_biomass_abund %>%
  filter(time_point >= 2) %>%
  ggplot(aes(x = day,
             y = indiv_per_volume,
             group = day)) +
  geom_boxplot() +
  labs(x = "Day",
       y = "Regional bioarea (µm²)")


## ----abundance-isolated-patches-linearity-test---------------------------------------------------------------------------------------------------------------------
linear_model = lm(indiv_per_volume ~ 
                    day, 
                  data = ds_biomass_abund %>% 
                            filter(time_point >= 2) %>%
                            filter(metaecosystem == "no"))

par(mfrow=c(2,3))
plot(linear_model, which = 1:5)


## ----abundance-isolated-patches-model-full-------------------------------------------------------------------------------------------------------------------------
full = lmer(indiv_per_volume ~
                     day * patch_size_volume * disturbance +
                     (day | system_nr),
                     data = ds_biomass_abund %>%
                            filter(time_point >= 2) %>%
                            filter(metaecosystem == "no"),
                   REML = FALSE,
                   control = lmerControl(optimizer = "Nelder_Mead"))


## ----abundance-isolated-patches-model-no-correlation---------------------------------------------------------------------------------------------------------------
no_correlation = lmer(indiv_per_volume ~
                     day * patch_size_volume * disturbance +
                     (day || system_nr),
                     data = ds_biomass_abund %>%
                            filter(time_point >= 2) %>%
                            filter(metaecosystem == "no"),
                   REML = FALSE,
                   control = lmerControl(optimizer = "Nelder_Mead"))

anova(full, no_correlation)


## ----abundance-isolated-patches-model-no-random-slopes-------------------------------------------------------------------------------------------------------------
no_random_slopes = lmer(indiv_per_volume ~
                     day * patch_size_volume * disturbance +
                     (1 | system_nr),
                     data = ds_biomass_abund %>%
                            filter(time_point >= 2) %>%
                            filter(metaecosystem == "no"),
                   REML = FALSE,
                   control = lmerControl(optimizer = "Nelder_Mead"))

anova(no_correlation, no_random_slopes)


## ----abundance-isolated-patches-model-no-threeway------------------------------------------------------------------------------------------------------------------
no_threeway = lmer(indiv_per_volume ~
                     day +
                     patch_size_volume +
                     disturbance +
                     day : patch_size_volume + 
                     day : disturbance +
                     patch_size_volume : disturbance + 
                     (1 | system_nr),
                     data = ds_biomass_abund %>%
                            filter(time_point >= 2) %>%
                            filter(metaecosystem == "no"),
                   REML = FALSE,
                   control = lmerControl(optimizer = 'optimx', 
                                         optCtrl = list(method = 'L-BFGS-B')))

anova(no_random_slopes, no_threeway)


## ----abundance-isolated-patches-model-no-tm------------------------------------------------------------------------------------------------------------------------
no_TM = lmer(indiv_per_volume ~
                     day +
                     patch_size_volume +
                     disturbance +
                     day : disturbance +
                     patch_size_volume : disturbance + 
                     (1 | system_nr),
                     data = ds_biomass_abund %>%
                            filter(time_point >= 2) %>%
                            filter(metaecosystem == "no"),
                   REML = FALSE,
                   control = lmerControl(optimizer = "Nelder_Mead"))

anova(no_threeway,no_TM)


## ----abundance-isolated-patches-model-no-td------------------------------------------------------------------------------------------------------------------------
no_TD = lmer(indiv_per_volume ~
                     day +
                     patch_size_volume +
                     disturbance +
                     day : patch_size_volume + 
                     patch_size_volume : disturbance + 
                     (1 | system_nr),
                     data = ds_biomass_abund %>%
                            filter(time_point >= 2) %>%
                            filter(metaecosystem == "no"),
                   REML = FALSE,
                   control = lmerControl(optimizer = "Nelder_Mead"))

anova(no_threeway, no_TD)


## ----abundance-isolated-patches-model-no-md------------------------------------------------------------------------------------------------------------------------
no_MD = lmer(indiv_per_volume ~
                     day +
                     patch_size_volume +
                     disturbance +
                     day : patch_size_volume + 
                     (1 | system_nr),
                     data = ds_biomass_abund %>%
                            filter(time_point >= 2) %>%
                            filter(metaecosystem == "no"),
                   REML = FALSE,
                   control = lmerControl(optimizer = "Nelder_Mead"))

anova(no_TD, no_MD)


## ----abundance-isolated-patches-best-model-------------------------------------------------------------------------------------------------------------------------
best_model = no_MD


## ----abundance-isolated-patches-best-model-residuals---------------------------------------------------------------------------------------------------------------
plot(best_model)
qqnorm(resid(best_model))


## ----warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------
R2_marginal = r.squaredGLMM(best_model)[1]
R2_marginal = round(R2_marginal, digits = 2)
R2_conditional = r.squaredGLMM(best_model)[2]
R2_conditional = round(R2_conditional, digits = 2)


## ----eval = recompute_analyses-------------------------------------------------------------------------------------------------------------------------------------
## R2_isolated_patches = partR2(best_model,
##                              partvars = c("day",
##                                           "patch_size_volume",
##                                           "disturbance"),
##                              R2_type = "conditional",
##                              nboot = 1000,
##                              CI = 0.95)
## saveRDS(R2_isolated_patches, file = here("results", "biomass", "R2_isolated_patches.RData"))


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
R2_regional = readRDS(here("results", "biomass", "R2_isolated_patches.RData"))
R2_regional$IR2


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
t2_t5 = lmer(indiv_per_volume ~
                     day +
                     patch_size_volume +
                     disturbance +
                     day : patch_size_volume + 
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


## ----eval = recompute_analyses-------------------------------------------------------------------------------------------------------------------------------------
## R2_isolated_patches = partR2(t2_t5,
##                              partvars = c("day",
##                                           "patch_size_volume",
##                                           "disturbance"),
##                              R2_type = "conditional",
##                              nboot = 1000,
##                              CI = 0.95)
## saveRDS(R2_isolated_patches, file = here("results", "R2_isolated_patches_t2t5.RData"))


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
R2_regional = readRDS(here("results", "R2_isolated_patches_t2t5.RData"))
R2_regional$IR2

