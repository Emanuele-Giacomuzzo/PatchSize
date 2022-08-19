## ----small-patches-single-ecosystems-plots------------------------------------------------------------------------------------------------------------------
ds_biomass %>%
  filter(disturbance == "low") %>%
  filter(patch_size == "S") %>%
  ggplot(aes(x = day,
             y = bioarea_per_volume,
             group = culture_ID,
             fill = culture_ID,
             color = culture_ID,
             linetype = eco_metaeco_type)) +
  geom_line(stat = "summary", fun = "mean") +
  labs(x = "Day",
       y = "Local bioarea (something/μl)",
       title = "Disturbance = low",
       linetype = "") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
  scale_linetype_discrete(labels = c("small isolated",
                                     "small connected to small",
                                     "small connected to large"))  +
  geom_vline(xintercept = first_perturbation_day,
             linetype="dotdash",
             color = "grey",
             size=0.7) +
  labs(caption = "Vertical grey line: first perturbation")

ds_biomass %>%
  filter(disturbance == "high") %>%
  filter(patch_size == "S") %>%
  ggplot(aes(x = day,
             y = bioarea_per_volume,
             group = culture_ID,
             fill = system_nr,
             color = system_nr,
             linetype = eco_metaeco_type)) +
  geom_line(stat = "summary", fun = "mean") +
  labs(title = "Disturbance = high",
       x = "Day",
       y = "Local bioarea (something/μl)",
       linetype = "") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
  scale_linetype_discrete(labels = c("small isolated",
                                     "small connected to small",
                                     "small connected to large"))  +
  geom_vline(xintercept = first_perturbation_day,
             linetype="dotdash",
             color = "grey",
             size=0.7) +
  labs(caption = "Vertical grey line: first perturbation")


## ----small-patches-boxplots---------------------------------------------------------------------------------------------------------------------------------
ds_biomass %>%
  filter(disturbance == "low") %>%
  filter(patch_size == "S") %>%
  ggplot(aes(x = day,
             y = bioarea_per_volume,
             group = interaction(day,eco_metaeco_type),
             fill = eco_metaeco_type)) +
  geom_boxplot() +
  labs(x = "Day",
       y = "Local bioarea (something/μl)",
       title = "Disturbance = low",
       fill = "") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
  scale_fill_discrete(labels = c("small isolated",
                                 "small connected to small",
                                 "small connected to large")) +
  geom_vline(xintercept = first_perturbation_day + 0.7,
             linetype="dotdash",
             color = "grey",
             size=0.7) +
  labs(caption = "Vertical grey line: first perturbation")

ds_biomass %>%
  filter(disturbance == "high") %>%
  filter(patch_size == "S") %>%
  ggplot(aes(x = day,
             y = bioarea_per_volume,
             group = interaction(day,eco_metaeco_type),
             fill = eco_metaeco_type)) +
  geom_boxplot() +
  labs(title = "Disturbance = high",
       x = "Day",
       y = "Local bioarea (something/μl)",
       fill = "") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
  scale_fill_discrete(labels = c("small isolated",
                                 "small connected to small",
                                 "small connected to large")) +
  geom_vline(xintercept = first_perturbation_day + 0.7,
             linetype="dotdash",
             color = "grey",
             size=0.7) +
  labs(caption = "Vertical grey line: first perturbation")


## ----small-patches-lnRR-plots-------------------------------------------------------------------------------------------------------------------------------
small_patches_publication = ds_biomass_averaged_across_videos %>%
  filter(disturbance == "low") %>%
  filter(eco_metaeco_type == "S (S_L)" | eco_metaeco_type == "S (S_S)") %>%
  ggplot(aes(x = day,
             y = lnRR_bioarea_per_volume,
             group = interaction(day, eco_metaeco_type),
             fill = eco_metaeco_type)) +
  geom_boxplot() +
  labs(title = "Disturbance = low",
       x = "Day",
       y = "LnRR Local Bioarea (something/μl)",
       fill = "") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(.3, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
  scale_fill_discrete(labels = c("small connected to small",
                                 "small connected to large")) +
  geom_vline(xintercept = first_perturbation_day + 0.7,
             linetype="dotdash",
             color = "grey",
             size=0.7) +
  labs(caption = "Vertical grey line: first perturbation, LnRR Local Bioarea: Bioarea + 1 / Mean bioarea small isolated")
small_patches_publication

ds_biomass_averaged_across_videos %>%
  filter(disturbance == "high") %>%
  filter(eco_metaeco_type == "S (S_L)" | eco_metaeco_type == "S (S_S)") %>%
  ggplot(aes(x = day,
             y = lnRR_bioarea_per_volume,
             group = interaction(day, eco_metaeco_type),
             fill = eco_metaeco_type)) +
  geom_boxplot() +
  labs(title = "Disturbance = high",
       x = "Day",
       y = "LnRR Local Bioarea (something/μl)",
       fill = "") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(.3, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
  scale_fill_discrete(labels = c("small connected to small",
                                 "small connected to large")) +
  geom_vline(xintercept = first_perturbation_day + 0.7,
             linetype="dotdash",
             color = "grey",
             size=0.7) +
  labs(caption = "Vertical grey line: first perturbation, LnRR Local Bioarea: Bioarea/Mean bioarea small isolated")


## ----choose-time-points-------------------------------------------------------------------------------------------------------------------------------------
first_time_point = 2
last_time_point = 7


## ----small-patches-full-model-------------------------------------------------------------------------------------------------------------------------------
full_model = lmer(lnRR_bioarea_per_volume ~
                    day * eco_metaeco_type * disturbance +
                    (day | culture_ID),
                  data = ds_biomass_averaged_across_videos %>%
                         filter(time_point >= first_time_point) %>%
                         filter(time_point <= last_time_point) %>%
                         filter(eco_metaeco_type== "S (S_S)" | 
                                eco_metaeco_type == "S (S_L)"),
                  REML = FALSE)


## ----small-patches-no-interaction---------------------------------------------------------------------------------------------------------------------------
no_interaction = lmer(lnRR_bioarea_per_volume ~
                    day * eco_metaeco_type * disturbance +
                    (day || culture_ID),
                  data = ds_biomass_averaged_across_videos %>%
                         filter(time_point >= first_time_point) %>%
                         filter(time_point <= last_time_point) %>%
                         filter(eco_metaeco_type== "S (S_S)" | 
                                eco_metaeco_type == "S (S_L)"),
                  REML = FALSE)

anova(full_model, no_interaction)


## ----small-patches-no-slope---------------------------------------------------------------------------------------------------------------------------------
no_slope = lmer(lnRR_bioarea_per_volume ~
                    day * eco_metaeco_type * disturbance +
                    (1 | culture_ID),
                  data = ds_biomass_averaged_across_videos %>%
                         filter(time_point >= first_time_point) %>%
                         filter(time_point <= last_time_point) %>%
                         filter(eco_metaeco_type== "S (S_S)" | 
                                eco_metaeco_type == "S (S_L)"),
                  REML = FALSE)

anova(full_model, no_slope)


## ----small-patches-no-three-way-----------------------------------------------------------------------------------------------------------------------------
no_three_way = lmer(lnRR_bioarea_per_volume ~
                    day +
                    eco_metaeco_type +
                    disturbance +
                    day : eco_metaeco_type + 
                    day : disturbance + 
                    eco_metaeco_type : disturbance + 
                    (day | culture_ID),
                  data = ds_biomass_averaged_across_videos %>%
                         filter(time_point >= first_time_point) %>%
                         filter(time_point <= last_time_point) %>%
                         filter(eco_metaeco_type== "S (S_S)" | 
                                eco_metaeco_type == "S (S_L)"),
                  REML = FALSE)

anova(full_model, no_three_way)


## ----small-patches-no-TM------------------------------------------------------------------------------------------------------------------------------------
no_TM = lmer(lnRR_bioarea_per_volume ~
                    day +
                    eco_metaeco_type +
                    disturbance +
                    day : disturbance + 
                    eco_metaeco_type : disturbance + 
                    (day | culture_ID),
                  data = ds_biomass_averaged_across_videos %>%
                         filter(time_point >= first_time_point) %>%
                         filter(time_point <= last_time_point) %>%
                         filter(eco_metaeco_type== "S (S_S)" | 
                                eco_metaeco_type == "S (S_L)"),
                  REML = FALSE)

anova(no_three_way, no_TM)


## ----small-patches-no-TD------------------------------------------------------------------------------------------------------------------------------------
no_TD = lmer(lnRR_bioarea_per_volume ~
                    day +
                    eco_metaeco_type +
                    disturbance +
                    day : eco_metaeco_type + 
                    eco_metaeco_type : disturbance + 
                    (day | culture_ID),
                  data = ds_biomass_averaged_across_videos %>%
                         filter(time_point >= first_time_point) %>%
                         filter(time_point <= last_time_point) %>%
                         filter(eco_metaeco_type== "S (S_S)" | 
                                eco_metaeco_type == "S (S_L)"),
                  REML = FALSE)

anova(no_three_way, no_TD)


## ----small-patches-no-MD------------------------------------------------------------------------------------------------------------------------------------
no_MD = lmer(lnRR_bioarea_per_volume ~
                    day +
                    eco_metaeco_type +
                    disturbance +
                    day : eco_metaeco_type + 
                    (day | culture_ID),
                  data = ds_biomass_averaged_across_videos %>%
                         filter(time_point >= first_time_point) %>%
                         filter(time_point <= last_time_point) %>%
                         filter(eco_metaeco_type== "S (S_S)" | 
                                eco_metaeco_type == "S (S_L)"),
             REML = FALSE,
             control = lmerControl (optimizer = "Nelder_Mead"))

anova(no_TD, no_MD)


## ----small-patches-no-D-------------------------------------------------------------------------------------------------------------------------------------
no_D = lmer(lnRR_bioarea_per_volume ~
                    day +
                    eco_metaeco_type +
                    day : eco_metaeco_type +
                    (day | culture_ID),
            data = ds_biomass_averaged_across_videos %>%
                         filter(time_point >= first_time_point) %>%
                         filter(time_point <= last_time_point) %>%
                         filter(eco_metaeco_type== "S (S_S)" | 
                                eco_metaeco_type == "S (S_L)"),
            REML = FALSE,
            control = lmerControl (optimizer = "Nelder_Mead")
            )

anova(no_MD, no_D)


## ----small-t2-t7-best-model---------------------------------------------------------------------------------------------------------------------------------
best_model = no_MD
plot(best_model)
qqnorm(resid(best_model))


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------
R2_marginal = r.squaredGLMM(best_model)[1]
R2_marginal = round(R2_marginal, digits = 2)
R2_conditional = r.squaredGLMM(best_model)[2]
R2_conditional = round(R2_conditional, digits = 2)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
last_time_point = 5
t2_t5 = lmer(lnRR_bioarea_per_volume ~
                    day +
                    eco_metaeco_type +
                    disturbance +
                    day : eco_metaeco_type + 
                    (day | culture_ID),
                  data = ds_biomass_averaged_across_videos %>%
                         filter(time_point >= first_time_point) %>%
                         filter(time_point <= last_time_point) %>%
                         filter(eco_metaeco_type== "S (S_S)" | 
                                eco_metaeco_type == "S (S_L)"),
             REML = FALSE,
             control = lmerControl (optimizer = "Nelder_Mead"))


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------
R2_marginal = r.squaredGLMM(t2_t5)[1]
R2_marginal = round(R2_marginal, digits = 2)
R2_conditional = r.squaredGLMM(t2_t5)[2]
R2_conditional = round(R2_conditional, digits = 2)


## ----child = here("r_files", "biomass", "sections", "small_patches_single_points.Rmd"), eval = FALSE--------------------------------------------------------
## NA

