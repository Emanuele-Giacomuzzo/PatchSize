ds_median_body_size %>%
  filter(disturbance == "low") %>%
  filter(patch_size == "S") %>%
  group_by(disturbance,
           patch_size,
           eco_metaeco_type,
           culture_ID,
           time_point,
           day) %>%
  summarise(median_body_size = mean(median_body_size)) %>%
  ggplot(aes(
    x = day,
    y = median_body_size,
    group = interaction(day, eco_metaeco_type),
    fill = eco_metaeco_type
  )) +
  geom_boxplot() +
  labs(
    x = "Day",
    y = "Median body size (µm²)",
    fill = ""
  ) +
   scale_fill_discrete(
     labels = c(
       "Small isolated",
       "Small connected to large",
       "Small connected to small"
     )
   ) +
   theme_bw(base_size = presentation_size) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    #legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  ) +
  geom_vline(
    xintercept = first_perturbation_day + 0.7,
    linetype = "dotdash",
    color = "grey",
    size = 0.7
  ) +
  labs(caption = "Vertical grey line: first perturbation")

ggsave(here("results", "presentation_median_body_size.png"))
