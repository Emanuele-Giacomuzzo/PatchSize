plot.patches.classes.points = function(eco_metaeco_type_input,
                                       size_class_input) {
  ds_classes %>%
    filter(disturbance == disturbance_input,
           eco_metaeco_type %in% eco_metaeco_type_input) %>%
    ungroup() %>%
    summarySE(
      measurevar = "size_class_abundance",
      groupvars = c("day",
                    "size_class_n",
                    "eco_metaeco_type")
    ) %>%
    filter(size_class_n == size_class_input) %>%
    ggplot(aes(x = day,
               y = size_class_abundance,
               color = eco_metaeco_type)) +
    geom_point(position = position_dodge(dodging)) +
    geom_line(position = position_dodge(dodging)) +
    geom_errorbar(
      aes(ymin = size_class_abundance + ci,
          ymax = size_class_abundance - ci),
      width = 0.2,
      position = position_dodge(dodging)
    ) +
    # geom_hline(
    #   yintercept = 0,
    #   linetype = zero_line_line_type,
    #   color = zero_line_colour,
    #   linewidth = zero_line_line_width
    # ) +
    labs(
      x = axis_names$axis_name[axis_names$variable == "day"],
      y = paste("Class",
                size_class_input,
                axis_names$axis_name[axis_names$variable == "size_class_abundance"]),
      color = ""
    ) +
    scale_color_manual(
      values = c(
        colours_treatments$colour[colours_treatments$treatment == eco_metaeco_type_input[1]][1],
        colours_treatments$colour[colours_treatments$treatment == eco_metaeco_type_input[2]][1],
        colours_treatments$colour[colours_treatments$treatment == eco_metaeco_type_input[3]][1],
        colours_treatments$colour[colours_treatments$treatment == eco_metaeco_type_input[4]][1],
        colours_treatments$colour[colours_treatments$treatment == eco_metaeco_type_input[5]][1],
        colours_treatments$colour[colours_treatments$treatment == eco_metaeco_type_input[6]][1],
        colours_treatments$colour[colours_treatments$treatment == eco_metaeco_type_input[7]][1],
        colours_treatments$colour[colours_treatments$treatment == eco_metaeco_type_input[8]][1]
      )
    ) +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = legend_position
    )
}
