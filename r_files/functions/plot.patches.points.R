plot.patches.points = function(ds_patches,
                               patch_type_input,
                               response_variable) {
  
  ds_patches %>%
    filter(
      patch_type %in% patch_type_input,
      !is.na(!!sym(response_variable))
    ) %>%
    summarySE(measurevar = response_variable,
              groupvars = c("day", "patch_type")) %>%
    ggplot(aes(
      x = day,
      y = get(response_variable),
      group = interaction(day, patch_type),
      color = patch_type
    )) +
    geom_point(stat = "summary",
               fun = "mean",
               position = position_dodge(dodging),
               size = treatment_points_size) +
    geom_line(
      stat = "summary",
      fun = "mean",
      aes(group = patch_type),
      position = position_dodge(dodging),
      linewidth = treatment_lines_linewidth
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
    guides(color = guide_legend(nrow = 1,
                                title.position = "top"))
}
