## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
for (disturbance_input in c("low", "high")) {
print(ds_biomass_abund %>%
  group_by(culture_ID, disturbance, day, eco_metaeco_type) %>%
  summarise(indiv_per_volume = mean(indiv_per_volume)) %>% #Average across videos
  filter(disturbance == disturbance_input) %>%
  ggplot(aes(x = day,
             y = indiv_per_volume,
             group = interaction(day, eco_metaeco_type),
             fill = eco_metaeco_type)) +
  geom_boxplot() +
  labs(title = paste("Disturbance =", disturbance_input),
       x = "Day",
       y = "Community density (individuals/µl)",
       fill = "") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
  #      legend.position = c(.99, .999),
  #      legend.justification = c("right", "top"),
  #      legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
  scale_fill_discrete(labels = c("large isolated",
                                 "large connected to large",
                                 "large connected to small",
                                 "medium isolated",
                                 "medium connected to medium",
                                 "small isolated",
                                 "small connected to large",
                                 "small connected to small")) +
  geom_vline(xintercept = first_perturbation_day + 0.6,
             linetype="dotdash",
             color = "grey",
             size=0.7) +
  labs(caption = "Vertical grey line: first perturbation"))
}


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
ds_abundance_total = ds_biomass_abund %>%
  filter(!culture_ID %in% ecosystems_to_take_off) %>%
  group_by(culture_ID, 
           system_nr, 
           disturbance, 
           time_point,
           day, 
           patch_size,
           patch_size_volume,
           eco_metaeco_type) %>%
  summarise(indiv_per_volume_video_averaged = mean(indiv_per_volume)) %>%
  mutate(total_patch_bioarea = indiv_per_volume_video_averaged * patch_size_volume)

for (disturbance_input in c("low", "high")) {
print(ds_abundance_total %>%
  filter(disturbance == disturbance_input) %>%
  ggplot(aes(x = day,
             y = total_patch_bioarea,
             group = interaction(day, eco_metaeco_type),
             fill = eco_metaeco_type)) +
  geom_boxplot() +
  labs(title = paste("Disturbance =", disturbance_input),
       x = "Day",
       y = "Community abundance (individuals)",
       fill = "") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
  #      legend.position = c(.99, .999),
  #      legend.justification = c("right", "top"),
  #      legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
  scale_fill_discrete(labels = c("large isolated",
                                 "large connected to large",
                                 "large connected to small",
                                 "medium isolated",
                                 "medium connected to medium",
                                 "small isolated",
                                 "small connected to large",
                                 "small connected to small")) +
  geom_vline(xintercept = first_perturbation_day + 0.6,
             linetype="dotdash",
             color = "grey",
             size=0.7) +
  labs(caption = "Vertical grey line: first perturbation"))
}

