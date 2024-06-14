plot.patches.species.composition.stacked.time = function(patch_type_input) {
  
  
  patch_type_input <- patch_type_input[order(match(patch_type_input, 
                                                   patch_types_ordered))]
  
  species_composition = ds_patches %>%
    ungroup() %>%
    filter(indiv_per_ml != 0) %>%
    select(time_point,
           patch_type,
           all_of(protist_species_dominance)) %>%
    rename_at(vars(protist_species_dominance),
              funs(gsub("_indiv_per_ml_dominance", "", .))) %>%
    pivot_longer(all_of(protist_species),
                 names_to = "species",
                 values_to = "dominance")
  
  species_composition[is.na(species_composition)] = 0
  
  species_composition %>%
    group_by(time_point,
             patch_type,
             species) %>%
    summarise(dominance = mean(dominance),
              dominance = round(dominance, digits = 1)) %>%
    filter(
      patch_type %in% patch_type_input,
      !dominance == 0
    ) %>%
    ggplot(aes(x = time_point,
               y = dominance,
               fill = species)) +
    geom_bar(stat = "identity") +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = legend_position
    ) +
    labs(x = "Time point",
         y = axis_names$axis_name[axis_names$variable == "dominance"],
         fill = "") #+
  # geom_text(aes(label = dominance),
  #           position = position_stack(vjust = 0.5))
  
}
