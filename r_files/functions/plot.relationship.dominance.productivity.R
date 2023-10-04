plot.relationship.dominance.productivity = function(patch_type_input,
                                                    protist_species,
                                                    protist_species_dominance) {
  
  patch_type_input <- factor(patch_type_input, 
                             levels = patch_types_ordered)
  
  for (species_input in protist_species) {
    print(
      ds_patches %>%
        select(
          patch_type,
          time_point,
          bioarea_mm2_per_ml,
          protist_species_dominance
        ) %>%
        rename_at(vars(protist_species_dominance), funs(gsub("_indiv_per_ml_dominance", "", .))) %>%
        pivot_longer(protist_species,
                     names_to = "species",
                     values_to = "dominance") %>%
        filter(patch_type == patch_type_input,
               species == species_input) %>%
        ggplot(aes(x = dominance,
                   y = bioarea_mm2_per_ml)) +
        geom_point() +
        facet_wrap( ~ time_point) +
        labs(title = paste0(
          species_input,
          " (Patch type = ",
          patch_type_input,
          ")"
        ))
    )
  }
}
