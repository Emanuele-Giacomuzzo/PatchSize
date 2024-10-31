plot.empty.plot.presentations = function(y_min,
                                         y_max,
                                         response_variable,
                                         metaecosystem_type_i = ""){
  
  ds_metaecosystems %>%
    filter(metaecosystem_type %in% metaecosystem_type_i,
           !is.na(!!sym(response_variable))) %>%
    ggplot(
      aes(
        x = day,
        y = get(response_variable),
        group = interaction(day, metaecosystem_type),
        color = metaecosystem_type,
        linetype = metaecosystem_type
      )
    ) +
    geom_point(
      stat = "summary",
      fun = "mean",
      position = position_dodge(dodging),
      size = presentation_treatment_points_size
    ) +
    geom_line(
      stat = "summary",
      fun = "mean",
      aes(group = metaecosystem_type),
      position = position_dodge(dodging),
      linewidth = presentation_treatment_linewidth
    ) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) + 
    xlim(x_min, x_max) +
    ylim(y_min, y_max) +
    labs(x = axis_names$axis_name[axis_names$variable == "day"],
         y = axis_names$axis_name[axis_names$variable == response_variable],
         color = "") +
    geom_vline(
      xintercept = resource_flow_days,
      linetype = resource_flow_line_type,
      color = resource_flow_line_colour,
      linewidth = resource_flow_line_width
    ) +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = legend_position,
      legend.key.width = unit(legend_width_cm, "cm"),
      axis.title = element_text(size = 20)#,
      #axis.text = element_text(size = 16)
    ) +
    guides(color = guide_legend(title = NULL,
                                nrow = 3),
           linetype = guide_legend(title = NULL,
                                   nrow = 3)) +
    guides(color = "none", 
           linetype = "none") +
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
