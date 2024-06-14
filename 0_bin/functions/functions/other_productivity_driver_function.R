#plot_time_points = function(patch_type){
#  
#  for(time_point_input in first_time_point + 1:last_time_point) {
#    
#    print(data %>%
#      filter(eco_metaeco_type == patch_type,
#             time_point == time_point_input) %>%
#      ggplot(aes(x = dominance,
#                 y = bioarea_per_volume)) +
#      geom_point() +
#      facet_wrap( ~ species) +
#      labs(title = paste0("Patch type = ",
#                          patch_type,
#                          ", Day = ",
#                          time_point_input * 4)))
#  }
#  
#}
#plot_time_points("Small isolated")
#plot_time_points("Medium isolated")
#plot_time_points("Large isolated")