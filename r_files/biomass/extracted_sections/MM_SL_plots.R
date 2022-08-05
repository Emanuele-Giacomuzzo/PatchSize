## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ds_regional %>%
    filter ( disturbance == "low") %>%
    filter (metaecosystem_type == "S_L" | 
              metaecosystem_type == "M_M") %>%
    ggplot (aes(x = day,
                y = regional_mean_bioarea,
                group = system_nr,
                fill = system_nr,
                color = system_nr,
                linetype = metaecosystem_type)) +
    geom_line () +
    labs(x = "Day", 
         y = "Regional bioarea (something/µl)",
         title = "Disturbance = low",
         fill = "System nr",
         color = "System nr",
         linetype = "") +
    scale_y_continuous(limits = c(0, 6250)) +
    scale_x_continuous(limits = c(-2, 30)) +
    scale_linetype_discrete(labels = c("medium-medium",
                                     "small-large")) + 
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.position = c(.95, .95),
          legend.justification = c("right", "top"),
          legend.box.just = "right",
          legend.margin = margin(6, 6, 6, 6)) +
  geom_vline(xintercept = first_perturbation_day, 
             linetype="dotdash", 
             color = "grey", 
             size=0.7) +
  labs(caption = "Vertical grey line: first perturbation")

ds_regional %>%
    filter ( disturbance == "high") %>%
    filter (metaecosystem_type == "S_L" | metaecosystem_type == "M_M") %>%
    ggplot (aes(x = day,
                y = regional_mean_bioarea,
                group = system_nr,
                fill = system_nr,
                color = system_nr,
                linetype = metaecosystem_type)) +
    geom_line () +
    labs(x = "Day", 
         y = "Regional bioarea (something/µl)",
         title = "Disturbance = high",
         fill = "System nr",
         color = "System nr",
         linetype = "") +
    scale_y_continuous(limits = c(0, 6250)) +
    scale_x_continuous(limits = c(-2, 30)) +
    scale_linetype_discrete(labels = c("medium-medium",
                                     "small-large")) + 
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.position = c(.95, .95),
          legend.justification = c("right", "top"),
          legend.box.just = "right",
          legend.margin = margin(6, 6, 6, 6)) +
  geom_vline(xintercept = first_perturbation_day, 
             linetype="dotdash", 
             color = "grey", 
             size=0.7) +
  labs(caption = "Vertical grey line: first perturbation")

p_regional_low_mean = ds_regional %>%
  filter(disturbance == "low") %>%
  filter (metaecosystem_type == "S_L" | metaecosystem_type == "M_M") %>%
  ggplot (aes(x = day,
              y = regional_mean_bioarea,
              group = interaction(day, metaecosystem_type),
              fill = metaecosystem_type)) +
  geom_boxplot() +
  labs(x = "Day", 
       y = "Regional bioarea (something/µl)",
       title = "Disturbance = low",
       color='', 
       fill='') +
  scale_fill_discrete(labels = c("medium-medium", 
                                 "small-large")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))  +
  geom_vline(xintercept = first_perturbation_day + 0.7, 
             linetype="dotdash", 
             color = "grey", 
             size=0.7) +
  labs(caption = "Vertical grey line: first perturbation")
p_regional_low_mean

ds_regional %>%
  filter(disturbance == "high") %>%
  filter (metaecosystem_type == "S_L" | metaecosystem_type == "M_M") %>%
  ggplot (aes(x = day,
              y = regional_mean_bioarea,
              group = interaction (day, metaecosystem_type),
              fill = metaecosystem_type)) +
  geom_boxplot() +
  labs(x = "Day", 
       y = "Regional bioarea (something/µl)",
       title = "Disturbance = high",
       color='', 
       fill='') +
  scale_fill_discrete(labels = c("medium-medium", "small-large")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
  geom_vline(xintercept = first_perturbation_day + 0.7, 
             linetype="dotdash", 
             color = "grey", 
             size=0.7) +
  labs(caption = "Vertical grey line: first perturbation")

