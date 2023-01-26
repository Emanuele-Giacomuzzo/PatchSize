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