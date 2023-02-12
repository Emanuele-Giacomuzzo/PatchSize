plot.patches.median.body.size.boxplots = function(eco_metaeco_type_input) {
  ds_median_body_size %>%
    filter(disturbance == disturbance_input,
           eco_metaeco_type %in% eco_metaeco_type_input) %>%
    ggplot(aes(
      x = day,
      y = median_body_size,
      group = interaction(day, eco_metaeco_type),
      fill = eco_metaeco_type
    )) +
    geom_boxplot(width = boxplot_width) +
    labs(x = axis_names$axis_name[axis_names$variable == "day"],
         y = axis_names$axis_name[axis_names$variable == "median_body_size"],
         fill = "") +
    
    scale_fill_manual(
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
    geom_vline(
      xintercept = resource_flow_days,
      linetype = resource_flow_line_type,
      color = resource_flow_line_colour,
      linewidth = resource_flow_line_width
    ) +
    # geom_hline(
    #   yintercept = 0,
    #   color = zero_line_colour,
    #   linetype = zero_line_line_type,
    #   linewidth = zero_line_line_width
    # ) +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = legend_position
    )
}
