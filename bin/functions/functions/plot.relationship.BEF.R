plot.relationship.BEF = function(patch_type,
                                 explanatory_variable) {
  print(
    ds_patches %>%
      filter(eco_metaeco_type == patch_type) %>%
      ggplot(aes(x = get(explanatory_variable),
                 y = bioarea_per_volume)) +
      geom_point() +
      facet_wrap( ~ time_point) +
      labs(title = paste0("Patch type = ",
                          patch_type),
           x = explanatory_variable)
  )
}
