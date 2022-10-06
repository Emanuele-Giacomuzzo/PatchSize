## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
for (disturbance_input in c("low", "high")){
  
  print(ds_median_body_size %>%
        filter(disturbance == disturbance_input) %>%
        filter(metaecosystem == "no") %>%
        group_by(disturbance, 
                 patch_size, 
                 eco_metaeco_type, 
                 culture_ID, 
                 time_point,
                 day) %>%
        summarise(median_body_size = mean(median_body_size)) %>%
        ggplot(aes(x = day,
                   y = median_body_size,
                   group = interaction(day, eco_metaeco_type),
                   fill = eco_metaeco_type)) +
        geom_boxplot()+
        labs(title = paste("Disturbance = ", disturbance_input),
             x = "Day",
             y = "Median body size (µm²)",
             fill = "") +
        scale_fill_discrete(labels = c("large isolated", 
                                       "medium isolated",
                                       "small isolated")) +
        theme_bw() +
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              legend.position = c(.40, .95),
              legend.justification = c("right", "top"),
              legend.box.just = "right",
              legend.margin = margin(6, 6, 6, 6)) +
        geom_vline(xintercept = first_perturbation_day + 0.7, 
                   linetype="dotdash", 
                   color = "grey", 
                   size = 0.7) +
        labs(caption = "Vertical grey line: first perturbation"))}


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
ds_median_body_size %>%
  filter(time_point >= 2) %>%
  ggplot(aes(x = day,
             y = median_body_size,
             group = day)) +
  geom_boxplot() +
  labs(x = "Day",
       y = "Regional bioarea (µm²)")


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
linear_model = lm(median_body_size ~ 
                    day, 
                  data = ds_median_body_size %>% 
                            filter(time_point >= 2) %>%
                            filter(metaecosystem == "no"))

par(mfrow=c(2,3))
plot(linear_model, which = 1:5)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
full = lmer(median_body_size ~
              day * patch_size_volume * disturbance +
              (day | culture_ID),
            data = ds_median_body_size %>%
                            filter(metaecosystem == "no") %>%
                            filter(time_point >= 2),
            REML = FALSE,
            control = lmerControl(optimizer = "Nelder_Mead"))


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
no_correlation = lmer(median_body_size ~
                        day * patch_size_volume * disturbance +
                        (day || culture_ID),
                      data = ds_median_body_size %>%
                            filter(metaecosystem == "no") %>%
                            filter(time_point >= 2),
                      REML = FALSE,
                      control = lmerControl(optimizer = "Nelder_Mead"))

anova(full, no_correlation)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
no_random_slopes = lmer(median_body_size ~
                     day * patch_size_volume * disturbance +
                     (1 | culture_ID),
                     data = ds_median_body_size %>%
                            filter(metaecosystem == "no") %>%
                            filter(time_point >= 2),
                   REML = FALSE,
                   control = lmerControl(optimizer = "Nelder_Mead"))

anova(no_correlation, no_random_slopes)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
no_threeway = lmer(median_body_size ~
                     day +
                     patch_size_volume +
                     disturbance +
                     day : patch_size_volume + 
                     day : disturbance +
                     patch_size_volume : disturbance + 
                     (1 | culture_ID),
                     data = ds_median_body_size %>%
                            filter(metaecosystem == "no") %>%
                            filter(time_point >= 2),
                   REML = FALSE,
                   control = lmerControl(optimizer = 'optimx', 
                                         optCtrl = list(method = 'L-BFGS-B')))

anova(no_random_slopes, no_threeway)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
no_TM = lmer(median_body_size ~
                     day +
                     patch_size_volume +
                     disturbance +
                     day : disturbance +
                     patch_size_volume : disturbance + 
                     (1 | culture_ID),
                     data = ds_median_body_size %>%
                            filter(metaecosystem == "no") %>%
                            filter(time_point >= 2),
                   REML = FALSE,
                   control = lmerControl(optimizer = "Nelder_Mead"))

anova(no_threeway,no_TM)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
no_TD = lmer(median_body_size ~
                     day +
                     patch_size_volume +
                     disturbance +
                     day : patch_size_volume + 
                     patch_size_volume : disturbance + 
                     (1 | culture_ID),
                     data = ds_median_body_size %>%
                            filter(metaecosystem == "no") %>%
                            filter(time_point >= 2),
                   REML = FALSE,
                   control = lmerControl(optimizer = "Nelder_Mead"))

anova(no_threeway, no_TD)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
no_MD = lmer(median_body_size ~
                     day +
                     patch_size_volume +
                     disturbance +
                     day : patch_size_volume + 
                     (1 | culture_ID),
                     data = ds_median_body_size %>%
                            filter(metaecosystem == "no") %>%
                            filter(time_point >= 2),
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


## ----eval = recompute_analyses-------------------------------------------------------------------------------------------------------------------------------------
## R2_isolated_patches = partR2(best_model,
##                              partvars = c("day",
##                                           "patch_size_volume",
##                                           "disturbance"),
##                              R2_type = "conditional",
##                              nboot = 1000,
##                              CI = 0.95)
## saveRDS(R2_isolated_patches, file = here("results", "R2_median_body_size_isolated_patches.RData"))


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
R2_isolated_patches = readRDS(here("results", "R2_median_body_size_isolated_patches.RData"))
R2_isolated_patches$IR2


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
t2_t5 = lmer(median_body_size ~
                     day +
                     patch_size_volume +
                     disturbance +
                     day : patch_size_volume + 
                     (1 | culture_ID),
                     data = ds_median_body_size %>%
                            filter(metaecosystem == "no") %>%
                            filter(time_point >= 2) %>%
                            filter(time_point <= 5),
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
## saveRDS(R2_isolated_patches, file = here("results", "R2_median_body_size_isolated_patches_t2t5.RData"))


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
R2_isolated_patches = readRDS(here("results", "R2_median_body_size_isolated_patches_t2t5.RData"))
R2_isolated_patches$IR2

