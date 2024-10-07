#' Plot ecosystem points effect sizes 
#'
#' @param ds_ecosystems_effect_size A data.frame 
#' @param ecosystem_type_selected 
#' @param response_variable_selected 
#'
#' @return Generate a ggplot object
#'
plot.ecosystems.points.ES = function(ds_ecosystems_effect_size, 
                                     ecosystem_type_selected,
                                     response_variable_selected) {
  
  # ds_ecosystems_effect_size = data frame with effect sizes of ecosystems
  
  ecosystem_type_selected <- ecosystem_type_selected[order(match(ecosystem_type_selected, 
                                                   ecosystem_types_ordered))]
  
  ds_ecosystems_effect_size_filtered = ds_ecosystems_effect_size %>%
    filter(!time_point == 0,
           ecosystem_type %in% ecosystem_type_selected)
  
  ds_ecosystems_effect_size_filtered$ecosystem_type <- factor(ds_ecosystems_effect_size_filtered$ecosystem_type, 
                                                       levels = ecosystem_type_selected)
  
  ds_ecosystems_effect_size_filtered %>%
    filter(!time_point == 0,
           ecosystem_type %in% ecosystem_type_selected) %>%
    ggplot(aes(
      x = day,
      y = get(response_variable_selected),
      color = ecosystem_type,
      group = interaction(day, ecosystem_type),
      linetype = ecosystem_type
    )) +
    geom_point(stat = "summary",
               fun = "mean",
               position = position_dodge(dodging)) +
    geom_line(
      stat = "summary",
      fun = "mean",
      aes(group = ecosystem_type),
      position = position_dodge(dodging),
      linewidth = treatment_lines_linewidth
    ) + 
    geom_errorbar(aes(ymin = get(paste0(
      response_variable_selected, "_lower"
    )),
    ymax = get(paste0(
      response_variable_selected, "_upper"
    ))),
    width = 0.2,
    position = position_dodge(dodging)) +
    labs(x = axis_names$axis_name[axis_names$variable == "day"],
         y = axis_names$axis_name[axis_names$variable == response_variable_selected],
         color = "") +
    scale_linetype_manual(
      values = c(
        parameters_treatments$linetype[parameters_treatments$treatment == ecosystem_type_selected[1]][1],
        parameters_treatments$linetype[parameters_treatments$treatment == ecosystem_type_selected[2]][1],
        parameters_treatments$linetype[parameters_treatments$treatment == ecosystem_type_selected[3]][1],
        parameters_treatments$linetype[parameters_treatments$treatment == ecosystem_type_selected[4]][1],
        parameters_treatments$linetype[parameters_treatments$treatment == ecosystem_type_selected[5]][1],
        parameters_treatments$linetype[parameters_treatments$treatment == ecosystem_type_selected[6]][1],
        parameters_treatments$linetype[parameters_treatments$treatment == ecosystem_type_selected[7]][1],
        parameters_treatments$linetype[parameters_treatments$treatment == ecosystem_type_selected[8]][1]
      )
    ) +
    scale_color_manual(
      values = c(
        parameters_treatments$colour[parameters_treatments$treatment == ecosystem_type_selected[1]][1],
        parameters_treatments$colour[parameters_treatments$treatment == ecosystem_type_selected[2]][1],
        parameters_treatments$colour[parameters_treatments$treatment == ecosystem_type_selected[3]][1],
        parameters_treatments$colour[parameters_treatments$treatment == ecosystem_type_selected[4]][1],
        parameters_treatments$colour[parameters_treatments$treatment == ecosystem_type_selected[5]][1],
        parameters_treatments$colour[parameters_treatments$treatment == ecosystem_type_selected[6]][1],
        parameters_treatments$colour[parameters_treatments$treatment == ecosystem_type_selected[7]][1],
        parameters_treatments$colour[parameters_treatments$treatment == ecosystem_type_selected[8]][1]
      )
    ) + 
    scale_x_continuous(breaks = unique(ds_ecosystems_effect_size$day)) +
    geom_vline(
      xintercept = resource_flow_days,
      linetype = resource_flow_line_type,
      color = resource_flow_line_colour,
      linewidth = resource_flow_line_width
    ) +
    geom_hline(
      yintercept = 0,
      linetype = zero_line_ES_line_type,
      color = zero_line_ES_colour,
      linewidth = zero_line_ES_line_width
    ) +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = legend_position,
      legend.key.width = unit(legend_width_cm, "cm")
    ) +
    guides(color = guide_legend(title = NULL,
                                nrow = 2),
           linetype = guide_legend(title = NULL,
                                   nrow = 2)) + 
    geom_rect(
      xmin = grey_background_xmin, 
      xmax = grey_background_xmax,
      ymin = grey_background_ymin, 
      ymax = grey_background_ymax, 
      fill = grey_background_fill, 
      alpha = grey_background_alpha,
      color = grey_background_color
    )
}
