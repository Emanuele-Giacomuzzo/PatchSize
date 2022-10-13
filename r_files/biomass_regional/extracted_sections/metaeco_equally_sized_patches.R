## ----metaecosystem-different-size-single-systems------------------------------------------------------------------------------------------------------------------------------
for (disturbance_input in c("low", "high")){
  
  print(ds_regional_biomass %>%
          filter (disturbance == disturbance_input) %>%
          filter(!metaecosystem_type == "S_L") %>%
          filter(!metaecosystem_type == "S_L_from_isolated") %>%
          ggplot (aes(x = day,
                      y = total_regional_bioarea,
                      group = system_nr,
                      fill = system_nr,
                      color = system_nr,
                      linetype = metaecosystem_type)) +
          geom_line () +
          labs(title = paste("Disturbance =", disturbance_input),
               x = "Day", 
               y = "Regional bioarea (µm²)",
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
          scale_linetype_discrete(labels = c("large-large",
                                             "medium-medium",
                                             "small-small"))  +
          geom_vline(xintercept = first_perturbation_day, 
                     linetype = "dotdash", 
                     color = "grey", 
                     size = 0.7) +
          labs(caption = "Vertical grey line: first perturbation"))}


## ----metaecos-different-size-boxplots-----------------------------------------------------------------------------------------------------------------------------------------
for (disturbance_input in c("low", "high")){
  
  print(ds_regional_biomass %>%
          filter(disturbance == disturbance_input) %>%
          filter(!metaecosystem_type == "S_L") %>%
          filter(!metaecosystem_type == "S_L_from_isolated") %>%
          ggplot(aes(x = day,
                     y = total_regional_bioarea,
                     group = interaction(day, metaecosystem_type),
                     fill = metaecosystem_type)) +
          geom_boxplot() + 
          labs(title = "Disturbance = low",
               x = "Day",
               y = "Regional bioarea (µm²)",
               fill = "") + 
          theme_bw() + 
          theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                legend.position = c(.95, .95),
                legend.justification = c("right", "top"),
                legend.box.just = "right",
                legend.margin = margin(6, 6, 6, 6)) +
          scale_fill_discrete(labels = c("large-large",
                                         "medium-medium",
                                         "small-small")) +
          geom_vline(xintercept = first_perturbation_day + 0.7, 
                     linetype = "dotdash", 
                     color = "grey", 
                     size = 0.7) +
          labs(caption = "Vertical grey line: first perturbation"))}

