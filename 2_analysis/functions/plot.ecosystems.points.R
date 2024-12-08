plot.ecosystems.points = function(data,
                                  ecosystem_type_selected,
                                  response_variable_selected) {
  
  data %>%
    filter(ecosystem_type %in% ecosystem_type_selected,
           !is.na(!!sym(response_variable_selected))) %>%
    summarySE(measurevar = response_variable_selected,
              groupvars = c("day", "ecosystem_size", "connection")) %>%
    ggplot(aes(x = day,
               y = get(response_variable_selected),
               group = interaction(day, ecosystem_size, connection),
               color = ecosystem_size,
               linetype = connection)) +
    geom_point(stat = "summary",
               fun = "mean",
               position = position_dodge(dodging),
               size = treatment_points_size) +
    geom_line(stat = "summary",
              fun = "mean",
              aes(group = interaction(ecosystem_size, connection)),
              position = position_dodge(dodging),
              linewidth = treatment_lines_linewidth) +
    geom_errorbar(aes(ymax = get(response_variable_selected) + ci,
                      ymin = get(response_variable_selected) - ci),
                  width = width_errorbar,
                  position = position_dodge(dodging)) +
    labs(x = axis_names$axis_name[axis_names$variable == "day"],
         y = axis_names$axis_name[axis_names$variable == response_variable_selected],
         color = "") +
    scale_x_continuous(breaks = unique(data$day)) +
    scale_linetype_manual(values = treatment_linetype) + 
    scale_color_manual(values = treatment_colours) + 
    geom_vline(xintercept = resource_flow_days,
               linetype = resource_flow_line_type,
               color = resource_flow_line_colour,
               linewidth = resource_flow_line_width) +
    geom_hline(yintercept = 0,
               color = zero_line_colour,
               linetype = zero_line_line_type,
               linewidth = zero_line_line_width) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = legend_position,
          legend.key.width = unit(legend_width_cm, "cm")) +
    guides(color = guide_legend(title = NULL,
                                nrow = 3),
           linetype = guide_legend(title = NULL,
                                   nrow = 3)) +
    geom_rect(xmin = grey_background_xmin, 
              xmax = grey_background_xmax,
              ymin = grey_background_ymin, 
              ymax = grey_background_ymax, 
              fill = grey_background_fill, 
              alpha = grey_background_alpha,
              color = grey_background_color)
}
