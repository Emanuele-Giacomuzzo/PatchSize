plot.metaecos.replicates = function(metaecosystem_type_input,
                                    response_variable) {
  
  ds_metaecosystems %>%
    filter(metaecosystem_type %in% metaecosystem_type_input) %>%
    ggplot(
      aes(
        x = day,
        y = get(response_variable),
        group = system_nr,
        fill = system_nr,
        color = system_nr,
        linetype = metaecosystem_type
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