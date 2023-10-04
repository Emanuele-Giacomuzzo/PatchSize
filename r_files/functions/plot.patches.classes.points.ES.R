plot.patches.classes.points.ES = function(patch_type_input,
                                          size_class_input) {
  
  patch_type_input <- factor(patch_type_input, 
                             levels = patch_types_ordered)
  
  ds_classes_effect_size %>%
    filter(
      patch_type %in% patch_type_input,
      time_point >= first_time_point_model,
      time_point <= last_time_point_model,
      size_class_n == size_class_input,
      day > 0
    ) %>%
    ungroup() %>%
    ggplot(aes(x = day,
               y = class_indiv_per_µl_d,
               color = patch_type)) +
    geom_point(position = position_dodge(0.5)) +
    geom_line(position = position_dodge(0.5),
              linewidth = treatment_lines_linewidth) +
    geom_errorbar(
      aes(ymin = class_indiv_per_µl_d_lower,
          ymax = class_indiv_per_µl_d_upper),
      width = .2,
      position = position_dodge(dodging_error_bar)
    ) +
    geom_hline(
      yintercept = 0,
      linetype = zero_line_ES_line_type,
      color = zero_line_ES_colour,
      linewidth = zero_line_ES_line_width
    ) +
    labs(
      x = axis_names$axis_name[axis_names$variable == "day"],
      y = paste("Class",
                size_class_input,
                axis_names$axis_name[axis_names$variable == "class_indiv_per_µl_d"]),
      color = ""
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
      ),
    ) +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = legend_position
    )
  
}