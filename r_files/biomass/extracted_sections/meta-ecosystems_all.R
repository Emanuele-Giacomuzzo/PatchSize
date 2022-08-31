## ----------------------------------------------------------------------------------------------------------------------------------------
ds_regional_biomass %>%
  filter(disturbance == "low") %>%
  filter(!metaecosystem_type == "S_L_from_isolated") %>%
  ggplot(aes(x = day,
             y = total_regional_bioarea,
             group = interaction(day, metaecosystem_type),
             fill = metaecosystem_type)) +
  geom_boxplot() +
  labs(title = "Disturbance = low",
       x = "Day",
       y = "Total bioarea (µm²)",
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
                                     "small-large",
                                 "small-small")) +
  geom_vline(xintercept = first_perturbation_day + 0.5,
             linetype="dotdash",
             color = "grey",
             size=0.7) +
  labs(caption = "Vertical grey line: first perturbation")

ds_regional_biomass %>%
  filter(disturbance == "high") %>%
  filter(!metaecosystem_type == "S_L_from_isolated") %>%
  ggplot(aes(x = day,
             y = total_regional_bioarea,
             group = interaction(day, metaecosystem_type),
             fill = metaecosystem_type)) +
  geom_boxplot() +
  labs(title = "Disturbance = high",
       x = "Day",
       y = "Total bioarea (µm²)",
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
                                     "small-large",
                                 "small-small")) +
  geom_vline(xintercept = first_perturbation_day + 0.5,
             linetype="dotdash",
             color = "grey",
             size=0.7) +
  labs(caption = "Vertical grey line: first perturbation")

