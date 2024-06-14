#Old calculatino of the effect size of different patch treatments. 

#### Patch biomass effect size (`ds_effect_size_bioarea_density)`

```{r ds_effect_size_bioarea_density-create}
eco_metaeco_types = unique(ds_biomass_abund$eco_metaeco_type)
small_patches = c("S", "S (S_S)", "S (S_L)")
medium_patches = c("M", "M (M_M)")
large_patches = c("L", "L (L_L)", "L (S_L)")

averaged_bioarea_density = ds_biomass_abund %>%
  group_by(culture_ID, time_point) %>%
  mutate(bioarea_per_volume_across_videos = mean(bioarea_per_volume)) %>%
  ungroup() %>%
  group_by(disturbance, eco_metaeco_type, time_point) %>%
  mutate(
    mean_bioarea_density = mean(bioarea_per_volume_across_videos),
    sd_bioarea_density = sd(bioarea_per_volume_across_videos),
    sam_size_bioarea_density = n()
  ) %>%
  ungroup() %>%
  mutate(
    mean_bioarea_density_isolated = NA,
    sd_bioarea_density_isolated = NA,
    sam_size_bioarea_density_isolated = NA
  )

for (disturbance_input in c("low", "high")) {
  for (eco_metaeco_input in eco_metaeco_types) {
    for (time_point_input in 0:7) {
      if (eco_metaeco_input %in% small_patches) {
        isolated_patch_type = "S"
      }
      if (eco_metaeco_input %in% medium_patches) {
        isolated_patch_type = "M"
      }
      if (eco_metaeco_input %in% large_patches) {
        isolated_patch_type = "L"
      }
      
      control_row = averaged_bioarea_density %>%
        filter(
          eco_metaeco_type == isolated_patch_type,
          disturbance == disturbance_input,
          time_point == time_point_input
        )
      
      averaged_bioarea_density$mean_bioarea_density_isolated[averaged_bioarea_density$eco_metaeco_type == eco_metaeco_input &
                                                               averaged_bioarea_density$disturbance == disturbance_input &
                                                               averaged_bioarea_density$time_point == time_point_input] =
        control_row$mean_bioarea_density
      
      averaged_bioarea_density$sd_bioarea_density_isolated[averaged_bioarea_density$eco_metaeco_type == eco_metaeco_input &
                                                             averaged_bioarea_density$disturbance == disturbance_input &
                                                             averaged_bioarea_density$time_point == time_point_input] =
        control_row$sd_bioarea_density
      
      averaged_bioarea_density$sam_size_bioarea_density_isolated[averaged_bioarea_density$eco_metaeco_type == eco_metaeco_input &
                                                                   averaged_bioarea_density$disturbance == disturbance_input &
                                                                   averaged_bioarea_density$time_point == time_point_input] =
        control_row$sam_size_bioarea_density
      
    }
  }
}

ds_effect_size_bioarea_density = averaged_bioarea_density %>%
  mutate(
    bioarea_density_hedges_d = calculate.hedges_d(
      mean_bioarea_density,
      sd_bioarea_density,
      sam_size_bioarea_density,
      mean_bioarea_density_isolated,
      sd_bioarea_density_isolated,
      sam_size_bioarea_density_isolated
    )
  ) %>%
  mutate(bioarea_density_lnRR = ln(mean_bioarea_density / mean_bioarea_density_isolated))
```

```{r ds_effect_size_bioarea_density-datatable}
datatable(ds_effect_size_bioarea_density,
          rownames = FALSE,
          options = list(scrollX = TRUE),
          filter = list(position = 'top', 
                        clear = FALSE))
```

#### Patch community density effect size (`ds_lnRR_community_density)`

```{r ds_effect_size_community_density-create}
eco_metaeco_types = unique(ds_biomass_abund$eco_metaeco_type)
small_patches = c("S", "S (S_S)", "S (S_L)")
medium_patches = c("M", "M (M_M)")
large_patches = c("L", "L (L_L)", "L (S_L)")

averaged_community_density = ds_biomass_abund %>%
  group_by(disturbance, eco_metaeco_type, time_point, culture_ID, patch_size, day) %>%
  summarise(indiv_per_volume_across_videos = mean(indiv_per_volume)) %>%
  group_by(disturbance, eco_metaeco_type, time_point, patch_size, day) %>%
  summarise(mean_community_density = mean(indiv_per_volume_across_videos),
            sd_community_density = sd(indiv_per_volume_across_videos),
            sam_size_community_density = n())

for (disturbance_input in c("low", "high")){
  for (eco_metaeco_input in eco_metaeco_types){
    for (time_point_input in 0:7){
      
      if (eco_metaeco_input %in% small_patches){isolated_patch_type = "S"}
      if (eco_metaeco_input %in% medium_patches){isolated_patch_type = "M"}
      if (eco_metaeco_input %in% large_patches){isolated_patch_type = "L"}
      
      control_row = averaged_community_density %>%
        filter(eco_metaeco_type == isolated_patch_type,
               disturbance == disturbance_input,
               time_point == time_point_input)
      
      averaged_community_density$mean_community_density_isolated[
        averaged_community_density$eco_metaeco_type == eco_metaeco_input &
          averaged_community_density$disturbance == disturbance_input &
          averaged_community_density$time_point == time_point_input] = 
        control_row$mean_community_density
      
      averaged_community_density$sd_community_density_isolated[
        averaged_community_density$eco_metaeco_type == eco_metaeco_input & 
          averaged_community_density$disturbance == disturbance_input &
          averaged_community_density$time_point == time_point_input] = 
        control_row$sd_community_density
      
      averaged_community_density$sam_size_community_density_isolated[
        averaged_community_density$eco_metaeco_type == eco_metaeco_input & 
          averaged_community_density$disturbance == disturbance_input &
          averaged_community_density$time_point == time_point_input] = 
        control_row$sam_size_community_density
      
    }}}

ds_effect_size_community_density = averaged_community_density %>%
  mutate(community_density_hedges_d = calculate.hedges_d(mean_community_density,
                                                         sd_community_density,
                                                         sam_size_community_density,
                                                         mean_community_density_isolated,
                                                         sd_community_density_isolated,
                                                         sam_size_community_density_isolated)) %>%
  mutate(community_density_lnRR = ln(mean_community_density/mean_community_density_isolated))
```

```{r ds_effect_size_community_density-datatable}
datatable(ds_effect_size_community_density,
          rownames = FALSE,
          options = list(scrollX = TRUE),
          filter = list(position = 'top', 
                        clear = FALSE))
```

```{r}
ds_classes_averaged$richness_hedges_d = NA

treatments_and_controls = data.frame(
  treatment = c("S (S_S)", "S (S_L)", "L (L_L)", "L (S_L)"),
  control = c("S", "S", "L", "L")
)

n_of_treatments = nrow(treatments_and_controls)

row_n = 0
for (disturbance_input in c("low", "high")) {
  for (treatment_n in 1:n_of_treatments) {
    for (time_point_input in 0:7) {
      row_n = row_n + 1
      
      
      eco_metaeco_treatment = treatments_and_controls$treatment[treatment_n]
      
      treatment = ds_biomass_abund %>%
        filter(
          disturbance == disturbance_input,
          eco_metaeco_type == eco_metaeco_treatment,
          time_point == time_point_input
        )
      
      eco_metaeco_control = treatments_and_controls$control[treatment_n]
      
      control = ds_biomass_abund %>%
        filter(
          disturbance == disturbance_input,
          eco_metaeco_type == eco_metaeco_control,
          time_point == time_point_input,
        ) 
      
      d = calculate.hedges_d(
        treatment$species_richness,
        treatment$species_richness_sd,
        treatment$sample_size,
        control$species_richness,
        control$species_richness_sd,
        control$sample_size
      )
      
      ds_classes_averaged$abundance_hedges_d[ds_classes_averaged$disturbance == disturbance_input &
                                               ds_classes_averaged$eco_metaeco_type == eco_metaeco_treatment &
                                               ds_classes_averaged$time_point == time_point_input &
                                               ds_classes_averaged$size_class_n == size_class_input] = d
    }
  }
}
```
