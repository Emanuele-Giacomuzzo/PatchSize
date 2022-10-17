## ----------------------------------------------------------------------------------------------------------------------
for (disturbance_input in c("low", "high")){
  
  print(ds_median_body_size %>%
          filter(disturbance == disturbance_input) %>%
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
          geom_boxplot() +
          labs(title = paste("Disturbance = ", disturbance_input),
               x = "Day",
               y = "Median body size (µm²)",
               color = "") +
          scale_fill_discrete(labels = c("large isolated", 
                                         "large connected to large",
                                         "large connected to small",
                                         "medium isolated",
                                         "medium connected to medium",
                                         "small isolated",
                                         "small connected to large",
                                         "small connected to small")) +
          theme_bw() +
          theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                #      legend.position = c(.95, .95),
                #      legend.justification = c("right", "top"),
                #      legend.box.just = "right",
                legend.margin = margin(6, 6, 6, 6)) +
          geom_vline(xintercept = first_perturbation_day + 0.7, 
                     linetype = "dotdash", 
                     color = "grey", 
                     size = 0.7) +
          labs(caption = "Vertical grey line: first perturbation"))}

