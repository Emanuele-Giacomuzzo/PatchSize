## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

### --- SINGLE ECOSYSTEMS --- ###

ds_biomass %>%
  filter(disturbance == "low") %>%
  #filter(eco_metaeco_type == "S (S_S)" | eco_metaeco_type == "S (S_L)") %>%
  filter(patch_size == "S") %>%
  ggplot(aes(x = day,
             y = bioarea_per_volume,
             group = system_nr,
             fill = system_nr,
             color = system_nr,
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
  #filter(eco_metaeco_type == "S (S_S)" | eco_metaeco_type == "S (S_L)") %>%
  filter(patch_size == "S") %>%
  ggplot(aes(x = day,
             y = bioarea_per_volume,
             group = system_nr,
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

### --- BOXPLOTS --- ###

local_small_low_plot = ds_biomass %>%
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
local_small_low_plot

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


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
average_biomass_S_isolated_low = ds_biomass %>%
  filter(disturbance == "low") %>%
  filter(eco_metaeco_type == "S") %>%
  group_by(culture_ID, time_point, day) %>%
  summarise(bioarea_per_volume_across_videos = mean(bioarea_per_volume)) %>% #Across videos
  group_by(time_point, day) %>%
  summarise(mean_bioarea_per_volume = mean(bioarea_per_volume_across_videos)) #Across cultures

average_biomass_S_isolated_high = ds_biomass %>%
  filter(disturbance == "high") %>%
  filter(eco_metaeco_type == "S") %>%
  group_by(culture_ID, time_point, day) %>%
  summarise(bioarea_per_volume_across_videos = mean(bioarea_per_volume)) %>%
  group_by(time_point, day) %>%
  summarise(mean_bioarea_per_volume = mean(bioarea_per_volume_across_videos))


for (time_point_input in 0:7) {
 
  ds_biomass$isolated_control[ds_biomass$patch_size == "S" & 
                              ds_biomass$disturbance == "low" &
                              ds_biomass$time_point == time_point_input] =
  subset(average_biomass_S_isolated_low,
         time_point == time_point_input)$mean_bioarea_per_volume
  
}

for (time_point_input in 0:7) {
 
  ds_biomass$isolated_control[ds_biomass$patch_size == "S" & 
                              ds_biomass$disturbance == "high" &
                              ds_biomass$time_point == time_point_input] =
  subset(average_biomass_S_isolated_high,
         time_point == time_point_input)$mean_bioarea_per_volume
  
}

ds_biomass = ds_biomass %>%
  mutate(isolated_control = as.numeric(isolated_control)) %>%
  mutate(RR_bioarea_per_volume = bioarea_per_volume / isolated_control)

ds_biomass %>%
  filter(disturbance == "low") %>%
  filter(patch_size == "S") %>%
  group_by(system_nr, day, eco_metaeco_type) %>%
  summarise(RR_bioarea_per_volume = mean(RR_bioarea_per_volume)) %>% #Across videos
  ggplot(aes(x = day,
             y = RR_bioarea_per_volume,
             group = interaction(day, eco_metaeco_type),
             fill = eco_metaeco_type)) +
  geom_boxplot() +
  labs(title = "Disturbance = low",
       x = "Day",
       y = "Response ratio bioarea (something/μl)",
       fill = "") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(.2, .95),
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
  labs(caption = "Vertical grey line: first perturbation, Response ratio bioarea: Bioarea/Mean bioarea small isolated")

ds_biomass %>%
  filter(disturbance == "high") %>%
  filter(patch_size == "S") %>%
  group_by(system_nr, day, eco_metaeco_type) %>%
  summarise(RR_bioarea_per_volume = mean(RR_bioarea_per_volume)) %>% #Across videos
  ggplot(aes(x = day,
             y = RR_bioarea_per_volume,
             group = interaction(day, eco_metaeco_type),
             fill = eco_metaeco_type)) +
  geom_boxplot() +
  labs(title = "Disturbance = high",
       x = "Day",
       y = "Response ratio bioarea (something/μl)",
       fill = "") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(.2, .95),
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
  labs(caption = "Vertical grey line: first perturbation, Response ratio bioarea: Bioarea/Mean bioarea small isolated")

small_patches_figure = ds_biomass %>%
  filter(disturbance == "low") %>%
  filter(eco_metaeco_type == "S (S_S)" | eco_metaeco_type == "S (S_L)") %>%
  group_by(system_nr, day, eco_metaeco_type) %>%
  summarise(RR_bioarea_per_volume = mean(RR_bioarea_per_volume)) %>% #Across videos
  ggplot(aes(x = day,
             y = RR_bioarea_per_volume,
             group = interaction(day, eco_metaeco_type),
             fill = eco_metaeco_type)) +
  geom_boxplot() +
  labs(x = "Day",
       y = "Response ratio bioarea (something/μl)",
       fill = "") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(.2, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
  scale_fill_discrete(labels = c("small connected to small",
                                 "small connected to large")) +
  geom_vline(xintercept = first_perturbation_day + 0.7,
             linetype="dotdash",
             color = "grey",
             size=0.7) +
  labs(caption = "Vertical grey line: first perturbation, Response ratio bioarea: Bioarea/Mean bioarea small isolated")


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full_model = lmer(RR_bioarea_per_volume ~
                    day * eco_metaeco_type * disturbance +
                    (day | system_nr),
                  data = ds_biomass %>%
                         filter(eco_metaeco_type== "S (S_S)" | eco_metaeco_type == "S (S_L)"),
                  REML = FALSE
                  )


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
no_interaction = lmer(RR_bioarea_per_volume ~
                    day * eco_metaeco_type * disturbance +
                    (day || system_nr),
                  data = ds_biomass %>%
                         filter(eco_metaeco_type== "S (S_S)" | eco_metaeco_type == "S (S_L)"),
                  REML = FALSE
                  )

anova(full_model, no_interaction)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
no_slope = lmer(RR_bioarea_per_volume ~
                    day * eco_metaeco_type * disturbance +
                    (1 | system_nr),
                  data = ds_biomass %>%
                         filter(eco_metaeco_type== "S (S_S)" | eco_metaeco_type == "S (S_L)"),
                  REML = FALSE
                  )

anova(no_interaction, no_slope)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
no_three_way = lmer(RR_bioarea_per_volume ~
                    day +
                    eco_metaeco_type +
                    disturbance +
                    day : eco_metaeco_type + 
                    day : disturbance + 
                    eco_metaeco_type : disturbance + 
                    (day || system_nr),
                  data = ds_biomass %>%
                         filter(eco_metaeco_type== "S (S_S)" | eco_metaeco_type == "S (S_L)"),
                  REML = FALSE
                  )

anova(no_interaction, no_three_way)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
no_TM = lmer(RR_bioarea_per_volume ~
                    day +
                    eco_metaeco_type +
                    disturbance +
                    day : disturbance + 
                    eco_metaeco_type : disturbance + 
                    (day || system_nr),
                  data = ds_biomass %>%
                         filter(eco_metaeco_type== "S (S_S)" | eco_metaeco_type == "S (S_L)"),
                  REML = FALSE
                  )

anova(no_three_way, no_TM)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
no_TD = lmer(RR_bioarea_per_volume ~
                    day +
                    eco_metaeco_type +
                    disturbance +
                    eco_metaeco_type : disturbance + 
                    (day || system_nr),
                  data = ds_biomass %>%
                         filter(eco_metaeco_type== "S (S_S)" | eco_metaeco_type == "S (S_L)"),
                  REML = FALSE
                  )

anova(no_TM, no_TD)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
no_MD = lmer(RR_bioarea_per_volume ~
                    day +
                    eco_metaeco_type +
                    disturbance +
                    day : disturbance + 
                    (day || system_nr),
                  data = ds_biomass %>%
                         filter(eco_metaeco_type== "S (S_S)" | eco_metaeco_type == "S (S_L)"),
                  REML = FALSE
                  )

anova(no_TM, no_MD)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
best_model = no_MD

no_D = lmer(RR_bioarea_per_volume ~
                    day +
                    eco_metaeco_type +
                    day : disturbance + 
                    (day || system_nr),
                  data = ds_biomass %>%
                         filter(eco_metaeco_type== "S (S_S)" | eco_metaeco_type == "S (S_L)"),
                  REML = FALSE,
            control = lmerControl (optimizer = "Nelder_Mead")
                  )

anova(best_model, no_D)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
AIC_min = 2172.4
AIC_max = 2174.3
exp(0.5 * (AIC_min - AIC_max))


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Create a table in which the regional biomass has been log transformed. 

### --- INITIALISE TABLE --- ###

columns = c("model", "time_point", "AIC", "BIC", "R2_mixed", "R2_fixed", "R2_mixed_M", "R2_fixed_M")
small_patches_table = data.frame(matrix(ncol = length(columns), nrow = 0))
colnames(small_patches_table) = columns

### --- POPULATE THE TABLE --- ###

for (last_point in 3:7) {
  
  full_model = lmer(RR_bioarea_per_volume ~
                    day +
                    eco_metaeco_type +
                    disturbance +
                    day : disturbance + 
                    (day || system_nr),
                  data = ds_biomass %>%
                         filter(eco_metaeco_type== "S (S_S)" | eco_metaeco_type == "S (S_L)"),
                  REML = FALSE
                  )

  
  null_model = lm(RR_bioarea_per_volume ~
                    1,
                  data = ds_biomass %>%
                         filter(eco_metaeco_type== "S (S_S)" | eco_metaeco_type == "S (S_L)"),
                  REML = FALSE
                  )
  
  metaeco_null_model = lmer(log10(regional_mean_bioarea + 1) ~
                              day +
                              disturbance +
                              day : disturbance +
                              (day | system_nr),
                            data = ds_regional %>%
                              filter(time_point >= 2) %>%
                              filter(time_point <= last_point) %>%
                              filter(metaecosystem_type == "M_M" | 
                                     metaecosystem_type == "S_L"),
                            REML = FALSE,
                            control = lmerControl(optimizer = "Nelder_Mead"))
  
  small_patches_table = update_all_models_table("t + M + D + t * M * D + (t || system_nr)",
                                             small_patches_table, 
                                             full_model, 
                                             null_model,
                                             metaeco_null_model,
                                             "mixed")
}

datatable(small_patches_table, 
          rownames = FALSE,
          options = list(pageLength = 100,
                         scrollX = TRUE,
                         autoWidth = TRUE,
                         columnDefs = list(list(targets=c(0),visible=TRUE, width='160'),
                                           list(targets=c(1), visible=TRUE, width='10'),
                                           list(targets=c(2), visible=TRUE, width='10'),
                                           list(targets=c(3), visible=TRUE, width='10'),
                                           list(targets=c(4), visible=TRUE, width='10'),
                                           list(targets=c(5), visible=TRUE, width='10'),
                                           list(targets=c(6), visible=TRUE, width='10'),
                                           list(targets=c(7), visible=TRUE, width='10'),
                                           list(targets='_all', visible=FALSE))),
          caption = "
          M = Meta-ecosystem type, 
          D = disturbance, 
          (1 | t) = random effect of time on the intercept,
          (1 | ID) = random effect of meta-ecosystem ID on the intercept, 
          || = no correlation between intercept and slope,
          | = correlation between intercept and slope,
          R2 = r squared of the whole model,
          R2_fixed = fixed part of the mixed model,
          mixed_R2 = r squared when considering both fixed and random effects (conditional r squared), 
          fixed_R2 = r squared when considering only the fixed effects (marginal r squared)")


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full = lmer(regional_mean_bioarea ~
              metaecosystem_type +
              disturbance +
              disturbance : metaecosystem_type,
            data = ds_regional %>%
              filter(time_point == 3) %>%
              filter(metaecosystem_type == "M_M" |
                     metaecosystem_type == "S_L"))


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
no_MD = lm(regional_mean_bioarea ~
              metaecosystem_type +
              disturbance,
            data = ds_regional %>%
              filter(time_point == 3) %>%
              filter(metaecosystem_type == "M_M" |
                     metaecosystem_type == "S_L"))

anova(full, no_MD)
AIC(full, no_MD)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

### --- SINGLE TIME POINTS --- ###

columns = c("time_point", "R2", "R2_M")
single_points = matrix(ncol = length(columns), 
                       nrow = 7)
single_points = as.data.frame(single_points)
colnames(single_points) = columns

for (t in 1:7) {
  
  full_model = lm(regional_mean_bioarea ~ 
               disturbance +
               metaecosystem_type,
             data = ds_regional %>%
               filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L") %>%
               filter(time_point == t))

  no_M_model = lm(regional_mean_bioarea ~ 
              disturbance,
            data = ds_regional %>%
              filter(metaecosystem_type == "M_M" |
                      metaecosystem_type == "S_L") %>%
              filter(time_point == t))
  
  R2_full_model = summary(full_model)$adj.r.squared
  R2_no_M_model = summary(no_M_model)$adj.r.squared
  R2_M = R2_full_model - R2_no_M_model
  
  single_points$time_point[t] = t
  single_points$R2[t] = R2_full_model
  single_points$R2_M[t] = R2_M
  
}

single_points = round(single_points, digits = 2)
single_points = single_points[2:nrow(single_points),]
datatable(single_points,
          rownames = FALSE,
          colnames = c("Time point", "R2 model", "R2 meta-ecosystem type"))

