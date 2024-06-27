plot.ecosystems.points.presentations = function(data,
                                             ecosystem_type_input,
                                             response_variable) {
  
  ecosystem_type_input <- ecosystem_type_input[order(match(ecosystem_type_input, 
                                                   ecosystem_types_ordered))]
  
  data %>%
    filter(
      ecosystem_type %in% ecosystem_type_input,
      !is.na(!!sym(response_variable))
    ) %>%
    summarySE(measurevar = response_variable,
              groupvars = c("day", "ecosystem_type")) %>%
    ggplot(aes(
      x = day,
      y = get(response_variable),
      group = interaction(day, ecosystem_type),
      color = ecosystem_type,
      linetype = ecosystem_type
    )) +
    geom_point(stat = "summary",
               fun = "mean",
               position = position_dodge(dodging),
               size = presentation_treatment_points_size) +
    geom_line(
      stat = "summary",
      fun = "mean",
      aes(group = ecosystem_type),
      position = position_dodge(dodging),
      linewidth = presentation_treatment_linewidth
    ) +
    geom_errorbar(aes(
      ymax = get(response_variable) + ci,
      ymin = get(response_variable) - ci
    ),
    width = width_errorbar,
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
    scale_linetype_manual(
      values = c(
        parameters_treatments$linetype[parameters_treatments$treatment == ecosystem_type_input[1]][1],
        parameters_treatments$linetype[parameters_treatments$treatment == ecosystem_type_input[2]][1],
        parameters_treatments$linetype[parameters_treatments$treatment == ecosystem_type_input[3]][1],
        parameters_treatments$linetype[parameters_treatments$treatment == ecosystem_type_input[4]][1],
        parameters_treatments$linetype[parameters_treatments$treatment == ecosystem_type_input[5]][1],
        parameters_treatments$linetype[parameters_treatments$treatment == ecosystem_type_input[6]][1],
        parameters_treatments$linetype[parameters_treatments$treatment == ecosystem_type_input[7]][1],
        parameters_treatments$linetype[parameters_treatments$treatment == ecosystem_type_input[8]][1]
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
    guides(color = guide_legend(title = NULL,
                                nrow = 2),
           linetype = guide_legend(title = NULL,
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
