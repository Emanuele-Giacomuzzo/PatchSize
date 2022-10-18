## --------------------------------------------------------------------------------------------------------------------------------------------------------------
for (disturbance_input in c("low", "high")) {
  
  print(ds_biomass_abund %>%
          group_by(culture_ID, disturbance, day, eco_metaeco_type) %>%
          summarise(bioarea_per_volume_video_averaged = mean(bioarea_per_volume)) %>%
          filter(disturbance == disturbance_input) %>%
          ggplot(aes(x = day,
             y = bioarea_per_volume_video_averaged,
             group = interaction(day, eco_metaeco_type),
             fill = eco_metaeco_type)) +
          geom_boxplot() +
          labs(title = paste("Disturbance =", disturbance_input),
               x = "Day",
               y = "Local bioarea (µm²/µl)",
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
          labs(caption = "Vertical grey line: first perturbation"))}


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
for (disturbance_input in c("low", "high")) {

  print(ds_biomass_abund %>%
          filter(disturbance == disturbance_input) %>%
          group_by(culture_ID, system_nr, disturbance, time_point, day, patch_size, patch_size_volume, eco_metaeco_type) %>%
          summarise(bioarea_tot_video_averaged = mean(bioarea_tot)) %>%
          ggplot(aes(x = day,
                     y = bioarea_tot_video_averaged,
                     group = interaction(day, eco_metaeco_type),
                     fill = eco_metaeco_type)) +
          geom_boxplot() +
          labs(title = paste("Disturbance =", disturbance_input),
               x = "Day",
               y = "Total patch bioarea (µm²)",
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
          labs(caption = "Vertical grey line: first perturbation"))}

