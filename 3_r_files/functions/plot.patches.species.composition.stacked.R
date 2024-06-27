plot.ecosystems.species.composition.stacked = function(ecosystem_type_input,
                                                    time_point_input) {
  
  ecosystem_type_input <- ecosystem_type_input[order(match(ecosystem_type_input, 
                                                   ecosystem_types_ordered))]
  
  species_composition = ds_ecosystems %>%
    ungroup() %>%
    filter(indiv_per_ml != 0) %>%
    select(time_point,
           ecosystem_type,
           all_of(protist_species_dominance)) %>%
    rename_at(vars(paste0(protist_species_indiv_per_ml, "_dominance")), 
              funs(gsub("_indiv_per_ml_dominance", "", .))) %>%
    pivot_longer(protist_species,
                 names_to = "species",
                 values_to = "dominance") 
  
  species_composition[is.na(species_composition)] = 0
  
  species_composition %>%
    group_by(time_point,
             ecosystem_type,
             species) %>%
    summarise(dominance = mean(dominance),
              dominance = round(dominance, digits = 1)) %>%
    filter(
      time_point == time_point_input,
      ecosystem_type %in% ecosystem_type_input,!dominance == 0
    ) %>%
    ggplot(aes(x = ecosystem_type,
               y = dominance,
               fill = species)) +
    geom_bar(stat = "identity") +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = legend_position
    ) +
    labs(x = "",
         y = axis_names$axis_name[axis_names$variable == "dominance"],
         fill = "") #+
  # geom_text(aes(label = dominance),
  #           position = position_stack(vjust = 0.5))
  
}
