plot.metaecos.points.presentations = function(data,
                                              metaecosystem_type_i,
                                              response_variable,
                                              n_legend_rows) {
  
  metaecosystem_type_i <- metaecosystem_type_i[order(match(metaecosystem_type_i, 
                                                           metaecosystem_types_ordered))]
  
  data %>%
    filter(metaecosystem_type %in% metaecosystem_type_i,
           !is.na(!!sym(response_variable))) %>%
    summarySE(measurevar = response_variable,
              groupvars = c("day", "metaecosystem_type")) %>%
    ggplot(
      aes(
        x = day,
        y = get(response_variable),
        group = interaction(day, metaecosystem_type),
        color = metaecosystem_type,
        linetype = metaecosystem_type
      )
    ) +
    geom_point(stat = "summary",
               fun = "mean",
               position = position_dodge(dodging),
               size = presentation_treatment_points_size) +
    geom_line(
      stat = "summary",
      fun = "mean",
      aes(group = metaecosystem_type),
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
    scale_linetype_manual(
      values = c(
        parameters_treatments$linetype[parameters_treatments$treatment == metaecosystem_type_i[1]][1],
        parameters_treatments$linetype[parameters_treatments$treatment == metaecosystem_type_i[2]][1],
        parameters_treatments$linetype[parameters_treatments$treatment == metaecosystem_type_i[3]][1],
        parameters_treatments$linetype[parameters_treatments$treatment == metaecosystem_type_i[4]][1],
        parameters_treatments$linetype[parameters_treatments$treatment == metaecosystem_type_i[5]][1],
        parameters_treatments$linetype[parameters_treatments$treatment == metaecosystem_type_i[6]][1],
        parameters_treatments$linetype[parameters_treatments$treatment == metaecosystem_type_i[7]][1],
        parameters_treatments$linetype[parameters_treatments$treatment == metaecosystem_type_i[8]][1]
      )
    ) +
    scale_color_manual(
      values = c(
        parameters_treatments$colour[parameters_treatments$treatment == metaecosystem_type_i[1]][1],
        parameters_treatments$colour[parameters_treatments$treatment == metaecosystem_type_i[2]][1],
        parameters_treatments$colour[parameters_treatments$treatment == metaecosystem_type_i[3]][1],
        parameters_treatments$colour[parameters_treatments$treatment == metaecosystem_type_i[4]][1],
        parameters_treatments$colour[parameters_treatments$treatment == metaecosystem_type_i[5]][1],
        parameters_treatments$colour[parameters_treatments$treatment == metaecosystem_type_i[6]][1],
        parameters_treatments$colour[parameters_treatments$treatment == metaecosystem_type_i[7]][1],
        parameters_treatments$colour[parameters_treatments$treatment == metaecosystem_type_i[8]][1]
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
      legend.position = legend_position,
      legend.key.width = unit(legend_width_cm, "cm")
    ) +
    guides(color = guide_legend(title = NULL,
                                nrow = n_legend_rows),
           linetype = guide_legend(title = NULL,
                                   nrow = n_legend_rows)) +
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
