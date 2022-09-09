## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
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


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
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

