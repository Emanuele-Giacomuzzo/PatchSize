plot.ecosystems.points.ES.log = function(ds_ecosystems_effect_size,
                                       ecosystem_type_input,
                                       response_variable) {
  
  ecosystem_type_input <- ecosystem_type_input[order(match(ecosystem_type_input, 
                                                   ecosystem_types_ordered))]
  
  ds_ecosystems_effect_size_filtered = ds_ecosystems_effect_size %>%
    filter(!time_point == 0,
           ecosystem_type %in% ecosystem_type_input)
  
  ds_ecosystems_effect_size_filtered$ecosystem_type <-
    factor(ds_ecosystems_effect_size_filtered$ecosystem_type,
           levels = ecosystem_type_input)
  
  ds_ecosystems_effect_size_filtered %>%
    filter(!time_point == 0,
           ecosystem_type %in% ecosystem_type_input) %>%
    ggplot(aes(
      x = day,
      y = log(get(response_variable) + 2),
      color = ecosystem_type
    )) +
    geom_point(position = position_dodge(dodging)) +
    geom_line(position = position_dodge(dodging),
              linewidth = treatment_lines_linewidth) +
    geom_errorbar(aes(ymin = get(paste0(
      response_variable, "_lower"
    )),
    ymax = get(paste0(
      response_variable, "_upper"
    ))),
    width = 0.2,
    position = position_dodge(dodging)) +
    labs(x = axis_names$axis_name[axis_names$variable == "day"],
         y = axis_names$axis_name[axis_names$variable == response_variable],
         color = "") +
    scale_color_manual(
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
    scale_x_continuous(breaks = unique(ds_ecosystems_effect_size$day)) +
    geom_vline(
      xintercept = resource_flow_days,
      linetype = resource_flow_line_type,
      color = resource_flow_line_colour,
      linewidth = resource_flow_line_width
    ) +
    geom_hline(
      yintercept = log(2),
      linetype = zero_line_ES_line_type,
      color = zero_line_ES_colour,
      linewidth = zero_line_ES_line_width
    ) +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = legend_position
    ) +
    guides(color = guide_legend(title = NULL,
                                nrow = 2)) +
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
