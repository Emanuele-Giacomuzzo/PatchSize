## ----------------------------------------------------------------------------------------------------------------------------------------
ds_biomass %>%
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

ds_biomass %>%
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


## ----------------------------------------------------------------------------------------------------------------------------------------
ds_biomass %>%
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

ds_biomass %>%
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

