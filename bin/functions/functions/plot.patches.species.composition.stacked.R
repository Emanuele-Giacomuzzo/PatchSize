plot.patches.species.composition.stacked = function(eco_metaeco_type_input,
                                  time_point_input){
  
  species_composition = ds_patches %>%
    filter(disturbance == disturbance_input) %>%
    select(time_point,
           eco_metaeco_type,
           Ble_dominance:Tet_dominance) %>%
    rename(Ble = Ble_dominance,
           Cep = Cep_dominance,
           Col = Col_dominance,
           Eug = Eug_dominance,
           Eup = Eup_dominance,
           Lox = Lox_dominance,
           Pau = Pau_dominance,
           Pca = Pca_dominance,
           Spi = Spi_dominance,
           Spi_te = Spi_te_dominance,
           Tet = Tet_dominance) %>%
    pivot_longer(Ble:Tet, 
                 names_to = "species", 
                 values_to = "dominance")
  
  species_composition[is.na(species_composition)] = 0
  
  species_composition %>%
    group_by(time_point,
             eco_metaeco_type, 
             species) %>%
    summarise(dominance = mean(dominance),
              dominance = round(dominance, digits = 1)) %>%
    filter(time_point == time_point_input,
           eco_metaeco_type %in% eco_metaeco_type_input,
           !dominance == 0) %>%
    ggplot(aes(x = eco_metaeco_type,
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
