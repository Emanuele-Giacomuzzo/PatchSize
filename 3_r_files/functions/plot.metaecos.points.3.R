plot.metaecos.points.3 = function(data,
                                  response_variable){
  
  p = data %>%
    filter(!is.na(!!sym(response_variable))) %>%
    summarySE(measurevar = response_variable,
              groupvars = c("day", "type", "connection")) %>%
    ggplot(aes(x = day,
               y = get(response_variable),
               group = interaction(day, type, connection),
               color = type,
               linetype = connection)) +
    geom_point(stat = "summary",
               fun = "mean",
               position = position_dodge(dodging),
               size = treatment_points_size) +
    geom_line(stat = "summary",
              fun = "mean",
              aes(group =  interaction(type, connection)),
              position = position_dodge(dodging),
              linewidth = treatment_lines_linewidth) +
    geom_errorbar(aes(ymax = get(response_variable) + ci,
                      ymin = get(response_variable) - ci),
                  width = width_errorbar,
                  position = position_dodge(dodging)) +
    labs(x = axis_names$axis_name[axis_names$variable == "day"],
         y = axis_names$axis_name[axis_names$variable == response_variable],
         color = "",
         linetype = "") +
    scale_x_continuous(breaks = unique(data$day)) +
    scale_color_manual(values = treatment_colours) +
    scale_linetype_manual(values = treatment_linetype) + 
    geom_vline(xintercept = resource_flow_days,
               linetype = resource_flow_line_type,
               color = resource_flow_line_colour,
               linewidth = resource_flow_line_width) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = legend_position,
          legend.key.width = unit(legend_width_cm, "cm")) +
    guides(color = guide_legend(title = NULL,
                                nrow = 2),
           linetype = guide_legend(title = NULL,
                                   nrow = 2)) +
    geom_rect(xmin = grey_background_xmin, 
              xmax = grey_background_xmax,
              ymin = grey_background_ymin, 
              ymax = grey_background_ymax, 
              fill = grey_background_fill, 
              alpha = grey_background_alpha,
              color = grey_background_color)
  
  return(p)
}
