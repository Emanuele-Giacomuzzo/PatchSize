plot.ecosystems.points.presentations = function(data,
                                                ecosystem_type_selected,
                                                response_variable_selected) {
  
  data %>%
    filter(ecosystem_type %in% ecosystem_type_selected,
           !is.na(!!sym(response_variable_selected))) %>%
    summarySE(measurevar = response_variable_selected,
              groupvars = c("day", "ecosystem_type")) %>%
    ggplot(aes(x = day,
               y = get(response_variable_selected),
               group = interaction(day, ecosystem_type),
               color = ecosystem_type,
               linetype = ecosystem_type)) +
    geom_point(stat = "summary",
               fun = "mean",
               position = position_dodge(dodging),
               size = presentation_treatment_points_size) +
    geom_line(stat = "summary",
              fun = "mean",
              aes(group = ecosystem_type),
              position = position_dodge(dodging),
              linewidth = presentation_treatment_linewidth) +
    geom_errorbar(aes(ymax = get(response_variable_selected) + ci,
                      ymin = get(response_variable_selected) - ci),
    width = width_errorbar,
    position = position_dodge(dodging)) +
    labs(x = axis_names$axis_name[axis_names$variable == "day"],
         y = axis_names$axis_name[axis_names$variable == response_variable_selected],
         color = "") +
    scale_linetype_manual(values = treatment_linetype) + 
    scale_color_manual(values = treatment_colours) + 
    geom_vline(xintercept = resource_flow_days,
               linetype = resource_flow_line_type,
               color = resource_flow_line_colour,
               linewidth = resource_flow_line_width) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = legend_position) +
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
}
