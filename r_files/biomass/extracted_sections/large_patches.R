## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

### --- SINGLE PATCHES --- ###

ds_biomass %>%
  filter(disturbance == "low") %>%
  filter(patch_size == "L") %>%
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
  scale_linetype_discrete(labels = c("large isolated",
                                     "large connected to large",
                                     "large connected to small"))

ds_biomass %>%
  filter(disturbance == "high") %>%
  filter(patch_size == "L") %>%
  ggplot(aes(x = day,
             y = bioarea_per_volume,
             group = system_nr,
             fill = system_nr,
             color = system_nr,
             linetype = eco_metaeco_type)) +
  geom_line(stat = "summary", fun = "mean") + 
  labs(x = "Day",
       y = "Local bioarea (something/μl)",
       title = "Disturbance = high",
       linetype = "") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
  scale_linetype_discrete(labels = c("large isolated",
                                     "large connected to large",
                                     "large connected to small"))

### --- BOXPLOTS --- ###

ds_biomass %>%
  filter(disturbance == "low") %>%
  filter(patch_size == "L") %>%
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
  scale_fill_discrete(labels = c("large isolated", 
                                 "large connected to large",
                                 "large connected to small"))

ds_biomass %>%
  filter(disturbance == "high") %>%
  filter(patch_size == "L") %>%
  ggplot(aes(x = day,
             y = bioarea_per_volume,
             group = interaction(day,eco_metaeco_type),
             fill = eco_metaeco_type)) +
  geom_boxplot() +
  labs(x = "Day",
       y = "Local bioarea (something/μl)",
       title = "Disturbance = high",
       fill = "") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
  scale_fill_discrete(labels = c("large isolated", 
                                 "large connected to large",
                                 "large connected to small"))



## ---- echo = FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------
ds_biomass %>%
  filter(eco_metaeco_type == "L" | 
          eco_metaeco_type == "L (L_L)") %>%
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
                                 "Connected to same size")) +
  scale_color_discrete(labels = c("Isolated", 
                                  "Connected to same size")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))

ds_biomass %>%
  filter(eco_metaeco_type == "L" | 
          eco_metaeco_type == "L (L_L)") %>%
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
                                 "Connected to same size")) +
  scale_color_discrete(labels = c("Isolated", 
                                  "Connected to same size")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

ds_local_S_t2_t7 = ds_biomass %>%
  filter (eco_metaeco_type == "L" | 
          eco_metaeco_type == "L (L_L)") %>%
  filter(time_point >= 2) #Let's take off the first two time points which are before the first disturbance event.

full_model = lmer(bioarea_per_volume ~ 
                    metaecosystem_type  + 
                    disturbance + 
                    metaecosystem_type * disturbance + 
                    (metaecosystem_type || day) + 
                    (disturbance || day) + 
                    (metaecosystem_type*disturbance  || day),
                  data = ds_local_S_t2_t7, 
                  REML = FALSE)

no_metaeco_type_model = lmer(bioarea_per_volume ~ 
                    disturbance + 
                    (disturbance || day),
                  data = ds_local_S_t2_t7, 
                  REML = FALSE)

anova(full_model, no_metaeco_type_model)


## ---- echo = FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------
ds_biomass %>%
  filter(eco_metaeco_type == "L" | 
            eco_metaeco_type == "L (S_L)") %>%
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
                                 "Connected to smaller patch")) +
  scale_color_discrete(labels = c("Isolated", 
                                  "Connected to smaller patch")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = c(.95, .99),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))

ds_biomass %>%
  filter(eco_metaeco_type == "L" | 
            eco_metaeco_type == "L (S_L)") %>%
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
                                 "Connected to smaller patch")) +
  scale_color_discrete(labels = c("Isolated", 
                                  "Connected to smaller patch")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = c(.95, .99),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

ds_local_S_t2_t7 = ds_biomass %>%
  filter (eco_metaeco_type == "L" |
          eco_metaeco_type == "L (S_L)") %>%
  filter(time_point >= 2) #Let's take off the first two time points which are before the first disturbance event.

full_model = lmer(bioarea_per_volume ~
                    metaecosystem_type  +
                    disturbance +
                    metaecosystem_type * disturbance +
                    (metaecosystem_type || day) +
                    (disturbance || day) +
                    (metaecosystem_type*disturbance  || day),
                  data = ds_local_S_t2_t7,
                  REML = FALSE)

no_metaeco_type_model = lmer(bioarea_per_volume ~
                    disturbance +
                    (disturbance || day),
                  data = ds_local_S_t2_t7,
                  REML = FALSE)

anova(full_model, no_metaeco_type_model)

