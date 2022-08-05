## ------------------------------------------------------------------------------------------------------------------------------------------
ds %>%
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

ds %>%
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


## ------------------------------------------------------------------------------------------------------------------------------------------

### --- SINGLE ECOSYSTEMS --- ###

ds %>%
  filter(disturbance == "low") %>%
  filter(eco_metaeco_type == "L (L_L)" | eco_metaeco_type == "L (S_L)") %>%
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
  scale_linetype_discrete(labels = c("large connected to large",
                                     "large connected to small"))

ds %>%
  filter(disturbance == "high") %>%
  filter(eco_metaeco_type == "L (L_L)" | eco_metaeco_type == "L (S_L)") %>%
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
  scale_linetype_discrete(labels = c("large connected to large",
                                     "large connected to small"))

### --- BOXPLOTS --- ###

ds %>%
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

ds %>%
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


