plot.metaecos.biomass.boxplots = function(metaecosystem_type_input,
                                 colour_level_1 = NA,
                                 colour_level_2 = NA,
                                 colour_level_3 = NA,
                                 colour_level_4 = NA,
                                 colour_level_5 = NA,
                                 colour_level_6 = NA,
                                 colour_level_7 = NA,
                                 colour_level_8 = NA,
                                 name_level_1 = NA,
                                 name_level_2 = NA,
                                 name_level_3 = NA,
                                 name_level_4 = NA,
                                 name_level_5 = NA,
                                 name_level_6 = NA,
                                 name_level_7 = NA,
                                 name_level_8 = NA) {
  
  number_of_metaecos = length(metaecosystem_type_input)
  legend_rows = ceiling(number_of_metaecos/3)
  
  print(
    ds_metaecosystems %>%
      filter(
        disturbance == disturbance_input,
        metaecosystem_type %in% metaecosystem_type_input
      ) %>%
      ggplot(
        aes(
          x = day,
          y = total_metaecosystem_bioarea,
          group = interaction(day, metaecosystem_type),
          fill = metaecosystem_type
        )
      ) +
      geom_boxplot(width = boxplot_width) +
      labs(x = x_axis_day,
           y = y_axis_total_bioarea,
           fill = "") +
      
      scale_fill_manual(
        values = c(
          colour_level_1,
          colour_level_2,
          colour_level_3,
          colour_level_4,
          colour_level_5,
          colour_level_6,
          colour_level_7,
          colour_level_8
        ),
        labels = c(
          name_level_1,
          name_level_2,
          name_level_3,
          name_level_4,
          name_level_5,
          name_level_6,
          name_level_7,
          name_level_8
        )
      ) +
      geom_vline(
        xintercept = resource_flow_days,
        linetype = resource_flow_line_type,
        color = resource_flow_line_colour,
        linewidth = resource_flow_line_width
      ) +
      geom_hline(
        yintercept = 0,
        color = zero_line_colour,
        linetype = zero_line_line_type,
        linewidth = zero_line_line_width
      ) +
      theme_bw() +
       theme(
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         legend.position = legend_position
       )
      
    
  )
  
}
