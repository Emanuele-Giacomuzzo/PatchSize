plot.patches.replicates = function(patch_type_input,
                                    response_variable) {
  
  patch_type_input <- factor(patch_type_input, 
                             levels = patch_types_ordered)

    ds_patches %>%
    filter(patch_type %in% patch_type_input) %>%
    ggplot(
      aes(
        x = day,
        y = get(response_variable),
        group = culture_ID,
        fill = culture_ID,
        color = culture_ID,
        linetype = patch_type
      )
    ) +
    geom_line(stat = "summary", fun = "mean") +
    geom_vline(
      xintercept = resource_flow_days,
      linetype = resource_flow_line_type,
      color = resource_flow_line_colour,
      linewidth = resource_flow_line_width
    ) +
    labs(
      x = axis_names$axis_name[axis_names$variable == "day"],
      y = axis_names$axis_name[axis_names$variable == response_variable],
      linetype = ""
    ) +
    scale_x_continuous(breaks = unique(ds_patches$day)) +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = legend_position
    )
  
}