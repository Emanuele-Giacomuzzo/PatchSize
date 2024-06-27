#plot_time_points = function(ecosystem_type){
#  
#  for(time_point_input in first_time_point + 1:last_time_point) {
#    
#    print(data %>%
#      filter(ecosystem_type == ecosystem_type,
#             time_point == time_point_input) %>%
#      ggplot(aes(x = dominance,
#                 y = bioarea_µm2_per_ml)) +
#      geom_point() +
#      facet_wrap( ~ species) +
#      labs(title = paste0("Patch type = ",
#                          ecosystem_type,
#                          ", Day = ",
#                          time_point_input * 4)))
#  }
#  
#}
#plot_time_points("Small unconnected")
#plot_time_points("Medium unconnected")
#plot_time_points("Large unconnected")