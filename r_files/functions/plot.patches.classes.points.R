plot.patches.classes.points = function(patch_type_input,
                                       size_class_input) {
  ds_classes %>%
    filter(patch_type %in% patch_type_input,
           !is.na(class_indiv_per_µl)) %>%
    ungroup() %>%
    summarySE(
      measurevar = "class_indiv_per_µl",
      groupvars = c("day",
                    "size_class_n",
                    "patch_type")
    ) %>%
    filter(size_class_n == size_class_input) %>%
    ggplot(aes(x = day,
               y = class_indiv_per_µl,
               color = patch_type)) +
    geom_point(position = position_dodge(dodging)) +
    geom_line(position = position_dodge(dodging),
              linewidth = treatment_lines_linewidth) +
    geom_errorbar(
      aes(ymin = class_indiv_per_µl + ci,
          ymax = class_indiv_per_µl - ci),
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
                axis_names$axis_name[axis_names$variable == "class_indiv_per_µl"]),
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
      )
    ) +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = legend_position
    )
}
