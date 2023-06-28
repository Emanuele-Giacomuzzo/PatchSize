plot.relationship.BEF = function(patch_type_input,
                                 response_variable) {
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