plot.ecosystems.replicates = function(ecosystem_type_input,
                                    response_variable) {
  
  ecosystem_type_input <- ecosystem_type_input[order(match(ecosystem_type_input, 
                                                   ecosystem_types_ordered))]

    ds_ecosystems %>%
    filter(ecosystem_type %in% ecosystem_type_input) %>%
    ggplot(aes(x = day,
               y = get(response_variable),
               group = culture_ID,
               fill = culture_ID,
               color = culture_ID,
               linetype = ecosystem_type)) +
    geom_line(stat = "summary", fun = "mean") +
    geom_vline(xintercept = resource_flow_days,
               linetype = resource_flow_line_type,
               color = resource_flow_line_colour,
               linewidth = resource_flow_line_width) +
    labs(x = axis_names$axis_name[axis_names$variable == "day"],
         y = axis_names$axis_name[axis_names$variable == response_variable],
         linetype = "") +
    scale_x_continuous(breaks = unique(ds_ecosystems$day)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = legend_position) +
      geom_rect(xmin = grey_background_xmin, 
                xmax = grey_background_xmax,
                ymin = grey_background_ymin, 
                ymax = grey_background_ymax, 
                fill = grey_background_fill, 
                alpha = grey_background_alpha,
                color = grey_background_color)
  
}