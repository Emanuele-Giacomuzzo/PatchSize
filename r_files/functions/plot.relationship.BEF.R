plot.relationship.BEF = function(patch_type_input,
                                 response_variable) {
  
  patch_type_input <- factor(patch_type_input, 
                             levels = patch_types_ordered)
  
  ds_patches %>%
      filter(patch_type == patch_type_input) %>%
      ggplot(aes(x = get(response_variable),
                 y = bioarea_mm2_per_ml)) +
      geom_point() +
      facet_wrap( ~ time_point) +
      labs(title = paste0("Patch type = ",
                          patch_type_input),
           x = response_variable)
}