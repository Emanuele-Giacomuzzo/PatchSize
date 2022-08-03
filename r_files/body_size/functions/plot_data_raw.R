plot_data_raw = function (day_input, disturbance_input, patch_type){
  
  if (patch_type == "S") {
    
    temporary_plot <<-  ds_classes %>%
      filter(day == day_input, disturbance == disturbance_input, patch_size == "S") %>%
      ggplot(aes(x = log_size_class,
                 y = log_abundance,
                 group = interaction(log_size_class, eco_metaeco_type),
                 fill = eco_metaeco_type)) +
      geom_boxplot() +
      labs(title = paste0("Day = ", day_input), x = "log body size (µm2)", y = "log mean abundance + 1 (indiv/µl)") +
      labs(fill='Patch type') +
      scale_y_continuous(limits = c(0,5))+
      scale_x_continuous(limits = c(7,10.5))}
  
  if (patch_type == "closed") {
    
    temporary_plot <<- ds_classes %>%
      filter(day == day_input, disturbance == disturbance_input, metaecosystem == "no") %>%
      ggplot(aes(x = log_size_class,
                 y = log_abundance,
                 group = interaction(log_size_class, eco_metaeco_type),
                 fill = eco_metaeco_type)) +
      geom_boxplot() +
      labs(title = paste0("Day = ", day_input), x = "log body size (µm2)", y = "log mean abundance + 1 (indiv/µl)") +
      labs(fill='Patch type') +
      scale_y_continuous(limits = c(0,5))+
      scale_x_continuous(limits = c(7,10.5))}
  
  return(temporary_plot)
  
  
}