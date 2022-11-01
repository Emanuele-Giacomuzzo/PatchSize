for (disturbance_input in c("low", "high")) {
  
  print(ds_effect_size_bioarea_density %>%
          filter(!time_point == 0) %>% #At time point 0 all cultures were the same 
          filter(disturbance == disturbance_input) %>%
          filter(eco_metaeco_type == "S (S_S)" | eco_metaeco_type == "S (S_L)") %>%
          ggplot(aes(x = day,
                     y = bioarea_density_hedges_d,
                     color = eco_metaeco_type)) +
          geom_point(position = position_dodge(0.5)) +
          geom_line(position = position_dodge(0.5)) + 
          labs(title = paste("Disturbance =", disturbance_input),
               x = "Day",
               y = "Local bioarea Hedge's d",
               color = "") +
          #geom_errorbar(aes(ymin = lnRR_lower, 
          #                  ymax = lnRR_upper), 
          #              width = .2,
          #              position = position_dodge(0.5)) + 
          scale_color_discrete(labels = c("small connected to large", 
                                          "small connnected to small")) +
          theme_bw() +
          theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                legend.position = c(.40, .95),
                legend.justification = c("right", "top"),
                legend.box.just = "right",
                legend.margin = margin(6, 6, 6, 6)) +
          #geom_vline(xintercept = first_perturbation_day + 0.7, 
          #           linetype="dotdash", 
          #           color = "grey", 
          #           size=0.7) +
          geom_hline(yintercept = 0, 
                     linetype = "dotted", 
                     color = "black", 
                     size = 0.7))
}

