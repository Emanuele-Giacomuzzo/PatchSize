plot.relationship.dominance.productivity = function(patch_type) {
  for (species_input in protist_species) {
    print(
      ds_patches %>%
        filter(disturbance == disturbance_input) %>%
        select(
          eco_metaeco_type,
          time_point,
          bioarea_per_volume,
          Ble_dominance:Tet_dominance
        ) %>%
        rename(
          Ble = Ble_dominance,
          Cep = Cep_dominance,
          Col = Col_dominance,
          Eug = Eug_dominance,
          Eup = Eup_dominance,
          Lox = Lox_dominance,
          Pau = Pau_dominance,
          Pca = Pca_dominance,
          Spi = Spi_dominance,
          Spi_te = Spi_te_dominance,
          Tet = Tet_dominance
        ) %>%
        pivot_longer(Ble:Tet,
                     names_to = "species",
                     values_to = "dominance") %>%
        filter(eco_metaeco_type == patch_type,
               species == species_input) %>%
        ggplot(aes(x = dominance,
                   y = bioarea_per_volume)) +
        geom_point() +
        facet_wrap( ~ time_point) +
        labs(title = paste0(
          species_input,
          " (Patch type = ",
          patch_type,
          ")"
        ))
    )
  }
}
