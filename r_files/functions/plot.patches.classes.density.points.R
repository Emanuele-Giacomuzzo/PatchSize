plot.patches.classes.density.points = function(patch_type_input,
                                               time_point_input){
  
  patch_type_input <- patch_type_input[order(match(patch_type_input, 
                                                   patch_types_ordered))]
  
  day = time_point_day$day[time_point_day$time_point == time_point_input]
  
  ds_classes_effect_size %>%
    filter(
      patch_type %in% patch_type_input,
      day == day
    ) %>%
    ggplot(
      aes(
        x = log_size_class,
        y = log_abundance,
        group = interaction(log_size_class, patch_type),
        color = patch_type
      )
    ) +
    geom_point(stat = "summary", fun = "mean") +
    geom_line(stat = "summary",
              fun = "mean",
              aes(group = patch_type),
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
        parameters_treatments$colour[parameters_treatments$treatment == patch_type_input[1]][1],
        parameters_treatments$colour[parameters_treatments$treatment == patch_type_input[2]][1],
        parameters_treatments$colour[parameters_treatments$treatment == patch_type_input[3]][1],
        parameters_treatments$colour[parameters_treatments$treatment == patch_type_input[4]][1],
        parameters_treatments$colour[parameters_treatments$treatment == patch_type_input[5]][1],
        parameters_treatments$colour[parameters_treatments$treatment == patch_type_input[6]][1],
        parameters_treatments$colour[parameters_treatments$treatment == patch_type_input[7]][1],
        parameters_treatments$colour[parameters_treatments$treatment == patch_type_input[8]][1]
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
    )
  
}
