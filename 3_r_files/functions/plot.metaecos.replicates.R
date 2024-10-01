plot.metaecos.replicates = function(data,
                                    metaecosystem_type_i,
                                    response_variable) {
  
  data %>%
    filter(metaecosystem_type %in% metaecosystem_type_i) %>%
    ggplot(aes(x = day,
               y = get(response_variable),
               group = system_nr,
               color = metaecosystem_type,
               linetype = connection)) +
    geom_line(stat = "summary", fun = "mean") +
    geom_vline(xintercept = resource_flow_days,
               linetype = resource_flow_line_type,
               color = resource_flow_line_colour,
               linewidth = resource_flow_line_width) +
    labs(x = axis_names$axis_name[axis_names$variable == "day"],
         y = axis_names$axis_name[axis_names$variable == response_variable],
         linetype = "") +
    scale_linetype_manual(values = treatment_linetype) +
    scale_x_continuous(breaks = unique(data$day)) +
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
              color = grey_background_color) + 
    guides(color = guide_legend(title = NULL,
                                nrow = 4),
           linetype = guide_legend(title = NULL,
                                   nrow = 4))
  
}
