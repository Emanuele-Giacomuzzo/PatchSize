plot.ecosystems.boxplots = function(ecosystem_type_input,
                                 response_variable) {
  
  ecosystem_type_input <- ecosystem_type_input[order(match(ecosystem_type_input, 
                                                   ecosystem_types_ordered))]
  
  ds_ecosystems %>%
    filter(
      ecosystem_type %in% ecosystem_type_input
    ) %>%
    ggplot(aes(
      x = day,
      y = get(response_variable),
      group = interaction(day, ecosystem_type),
      fill = ecosystem_type
    )) +
    geom_boxplot(width = boxplot_width) +
    labs(x = axis_names$axis_name[axis_names$variable == "day"],
         y = axis_names$axis_name[axis_names$variable == response_variable],
         fill = "") +
    
    scale_fill_manual(
      values = c(
        parameters_treatments$colour[parameters_treatments$treatment == ecosystem_type_input[1]][1],
        parameters_treatments$colour[parameters_treatments$treatment == ecosystem_type_input[2]][1],
        parameters_treatments$colour[parameters_treatments$treatment == ecosystem_type_input[3]][1],
        parameters_treatments$colour[parameters_treatments$treatment == ecosystem_type_input[4]][1],
        parameters_treatments$colour[parameters_treatments$treatment == ecosystem_type_input[5]][1],
        parameters_treatments$colour[parameters_treatments$treatment == ecosystem_type_input[6]][1],
        parameters_treatments$colour[parameters_treatments$treatment == ecosystem_type_input[7]][1],
        parameters_treatments$colour[parameters_treatments$treatment == ecosystem_type_input[8]][1]
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
    ) + 
    geom_rect(
      xmin = grey_background_xmin, 
      xmax = grey_background_xmax,
      ymin = grey_background_ymin, 
      ymax = grey_background_ymax, 
      fill = grey_background_fill, 
      alpha = grey_background_alpha,
      color = grey_background_color
    )
  
}
