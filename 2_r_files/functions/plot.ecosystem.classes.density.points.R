plot.ecosystems.classes.density.points = function(ecosystem_type_input,
                                               time_point_input){
  
  ecosystem_type_input <- ecosystem_type_input[order(match(ecosystem_type_input, 
                                                   ecosystem_types_ordered))]
  
  day = time_point_day$day[time_point_day$time_point == time_point_input]
  
  ds_classes_effect_size %>%
    filter(
      ecosystem_type %in% ecosystem_type_input,
      day == day
    ) %>%
    ggplot(
      aes(
        x = log_size_class,
        y = log_abundance,
        group = interaction(log_size_class, ecosystem_type),
        color = ecosystem_type
      )
    ) +
    geom_point(stat = "summary", fun = "mean") +
    geom_line(stat = "summary",
              fun = "mean",
              aes(group = ecosystem_type),
              linewidth = treatment_lines_linewidth
              ) +
    geom_errorbar(
      aes(ymax = log_abundance_upper_ci,
          ymin = log_abundance_lower_ci),
      width = .2,
      position = position_dodge(0.05)
    ) +
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
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = legend_position
    ) +
    labs(
      title = paste("Day =", day),
      x = axis_names$axis_name[axis_names$variable == "log_size_class"],
      y = axis_names$axis_name[axis_names$variable == "log_abundance"],
      color = ""
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
