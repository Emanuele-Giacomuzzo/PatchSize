### All data points

ds_patches %>%
  filter(disturbance == disturbance_input) %>%
  ggplot(aes(x = evenness_pielou,
             y = bioarea_per_volume)) +
  geom_point()

ds_patches %>%
  filter(disturbance == disturbance_input) %>%
  ggplot(aes(x = species_richness,
             y = bioarea_per_volume)) +
  geom_point()

ds_patches %>%
  filter(disturbance == disturbance_input) %>%
  ggplot(aes(x = shannon,
             y = bioarea_per_volume)) +
  geom_point()

ds_patches %>%
  filter(disturbance == disturbance_input) %>%
  ggplot(aes(x = shannon,
             y = bioarea_per_volume)) +
  geom_point()

ds_patches %>%
  filter(disturbance == disturbance_input) %>%
  ggplot(aes(x = Ble_dominance,
             y = bioarea_per_volume)) +
  geom_point()

ds_patches %>%
  filter(disturbance == disturbance_input) %>%
  ggplot(aes(x = Cep_dominance,
             y = bioarea_per_volume)) +
  geom_point()

ds_patches %>%
  filter(disturbance == disturbance_input) %>%
  ggplot(aes(x = Col_dominance,
             y = bioarea_per_volume)) +
  geom_point()
ds_patches %>%
  filter(disturbance == disturbance_input) %>%
  ggplot(aes(x = Eug_dominance,
             y = bioarea_per_volume)) +
  geom_point()

ds_patches %>%
  filter(disturbance == disturbance_input) %>%
  ggplot(aes(x = Eup_dominance,
             y = bioarea_per_volume)) +
  geom_point()

ds_patches %>%
  filter(disturbance == disturbance_input) %>%
  ggplot(aes(x = Lox_dominance,
             y = bioarea_per_volume)) +
  geom_point()

ds_patches %>%
  filter(disturbance == disturbance_input) %>%
  ggplot(aes(x = Pau_dominance,
             y = bioarea_per_volume)) +
  geom_point()

ds_patches %>%
  filter(disturbance == disturbance_input) %>%
  ggplot(aes(x = Pca_dominance,
             y = bioarea_per_volume)) +
  geom_point()

ds_patches %>%
  filter(disturbance == disturbance_input) %>%
  ggplot(aes(x = Spi_dominance,
             y = bioarea_per_volume)) +
  geom_point()

ds_patches %>%
  filter(disturbance == disturbance_input) %>%
  ggplot(aes(x = Spi_te_dominance,
             y = bioarea_per_volume)) +
  geom_point()

ds_patches %>%
  filter(disturbance == disturbance_input) %>%
  ggplot(aes(x = Tet_dominance,
             y = bioarea_per_volume)) +
  geom_point()

### Isolated patches points

ds_patches %>%
  filter(disturbance == disturbance_input,
         metaecosystem == "no") %>%
  ggplot(aes(x = evenness_pielou,
             y = bioarea_per_volume,
             color = eco_metaeco_type)) +
  geom_point()

ds_patches %>%
  filter(disturbance == disturbance_input,
         metaecosystem == "no") %>%
  ggplot(aes(x = shannon,
             y = bioarea_per_volume,
             color = eco_metaeco_type)) +
  geom_point()

ds_patches %>%
  filter(disturbance == disturbance_input,
         metaecosystem == "no") %>%
  ggplot(aes(x = Ble_dominance,
             y = bioarea_per_volume,
             color = eco_metaeco_type)) +
  geom_point()

ds_patches %>%
  filter(disturbance == disturbance_input,
         metaecosystem == "no") %>%
  ggplot(aes(x = Col_dominance,
             y = bioarea_per_volume,
             color = eco_metaeco_type)) +
  geom_point()

ds_patches %>%
  filter(disturbance == disturbance_input,
         metaecosystem == "no") %>%
  ggplot(aes(x = Eug_dominance,
             y = bioarea_per_volume,
             color = eco_metaeco_type)) +
  geom_point()

ds_patches %>%
  filter(disturbance == disturbance_input,
         metaecosystem == "no") %>%
  ggplot(aes(x = Eup_dominance,
             y = bioarea_per_volume,
             color = eco_metaeco_type)) +
  geom_point()

ds_patches %>%
  filter(disturbance == disturbance_input) %>%
  ggplot(aes(x = Lox_dominance,
             y = bioarea_per_volume)) +
  geom_point()

ds_patches %>%
  filter(disturbance == disturbance_input) %>%
  ggplot(aes(x = Pau_dominance,
             y = bioarea_per_volume)) +
  geom_point()

ds_patches %>%
  filter(disturbance == disturbance_input) %>%
  ggplot(aes(x = Pca_dominance,
             y = bioarea_per_volume)) +
  geom_point()

ds_patches %>%
  filter(disturbance == disturbance_input) %>%
  ggplot(aes(x = Spi_dominance,
             y = bioarea_per_volume)) +
  geom_point()

ds_patches %>%
  filter(disturbance == disturbance_input) %>%
  ggplot(aes(x = Spi_te_dominance,
             y = bioarea_per_volume)) +
  geom_point()

ds_patches %>%
  filter(disturbance == disturbance_input) %>%
  ggplot(aes(x = Tet_dominance,
             y = bioarea_per_volume)) +
  geom_point()

# 

data = ds_patches %>%
  select(eco_metaeco_type,
         time_point,
         bioarea_per_volume,
         Ble_dominance:Tet_dominance) %>%
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
               values_to = "dominance")

patch_type = "L"

plot = NA
plot_time_points = function(patch_type){
  
  for(time_point_input in first_time_point + 1:last_time_point) {
    
    plot[[time_point_input]] = data %>%
      filter(eco_metaeco_type == patch_type,
             time_point == time_point_input) %>%
      ggplot(aes(x = dominance,
                 y = bioarea_per_volume)) +
      geom_point() +
      facet_wrap( ~ species) +
      labs(title = paste0("Patch type = ",
                          patch_type,
                          ", Day = ",
                          time_point_input * 4))
  }
  
}





