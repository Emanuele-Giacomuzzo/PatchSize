```{r calculate-the-confidence-intervals-of-biomass-density-effect-size}

#Here I'm going to have combinations of treatment patches and isolated patches. That is for each disturbance and time point.

#################################################
### --- Find culture ID of each treatment --- ###
#################################################

low_small_isolated_ID = culture_info %>%
  filter(disturbance == "low") %>%
  filter(eco_metaeco_type == "S") %>%
  select(culture_ID)
low_small_isolated_ID = unlist(low_small_isolated_ID)

low_small_to_small_ID = culture_info %>%
  filter(disturbance == "low") %>%
  filter(eco_metaeco_type == "S (S_S)") %>%
  select(culture_ID)
low_small_to_small_ID = unlist(low_small_to_small_ID)

low_small_to_large_ID = culture_info %>%
  filter(disturbance == "low") %>%
  filter(eco_metaeco_type == "S (S_L)") %>%
  select(culture_ID)
low_small_to_large_ID = unlist(low_small_to_large_ID)

low_large_isolated_ID = culture_info %>%
  filter(disturbance == "low") %>%
  filter(eco_metaeco_type == "L") %>%
  select(culture_ID)
low_large_isolated_ID = unlist(low_large_isolated_ID)

low_large_to_large_ID = culture_info %>%
  filter(disturbance == "low") %>%
  filter(eco_metaeco_type == "L (L_L)") %>%
  select(culture_ID)
low_large_to_large_ID = unlist(low_large_to_large_ID)

low_large_to_small_ID = culture_info %>%
  filter(disturbance == "low") %>%
  filter(eco_metaeco_type == "L (S_L)") %>%
  select(culture_ID)
low_large_to_small_ID = unlist(low_large_to_small_ID)

high_small_isolated_ID = culture_info %>%
  filter(disturbance == "high") %>%
  filter(eco_metaeco_type == "S") %>%
  select(culture_ID)
high_small_isolated_ID = unlist(high_small_isolated_ID)

high_small_to_small_ID = culture_info %>%
  filter(disturbance == "high") %>%
  filter(eco_metaeco_type == "S (S_S)") %>%
  select(culture_ID)
high_small_to_small_ID = unlist(high_small_to_small_ID)

high_small_to_large_ID = culture_info %>%
  filter(disturbance == "high") %>%
  filter(eco_metaeco_type == "S (S_L)") %>%
  select(culture_ID)
high_small_to_large_ID = unlist(high_small_to_large_ID)

high_large_isolated_ID = culture_info %>%
  filter(disturbance == "high") %>%
  filter(eco_metaeco_type == "L") %>%
  select(culture_ID)
high_large_isolated_ID = unlist(high_large_isolated_ID)

high_large_to_large_ID = culture_info %>%
  filter(disturbance == "high") %>%
  filter(eco_metaeco_type == "L (L_L)") %>%
  select(culture_ID)
high_large_to_large_ID = unlist(high_large_to_large_ID)

high_large_to_small_ID = culture_info %>%
  filter(disturbance == "high") %>%
  filter(eco_metaeco_type == "L (S_L)") %>%
  select(culture_ID)
high_large_to_small_ID = unlist(high_large_to_small_ID)

#####################################################################
### --- Create the combination of each treatment and isolated --- ###
#####################################################################

low_small_to_small_comb = expand.grid(low_small_to_small_ID, low_small_isolated_ID) %>%
  rename(treatment_ID = Var1) %>%
  rename(isolated_ID = Var2) %>%
  mutate (disturbance = "low") %>%
  mutate (eco_metaeco_type = "S (S_S)")

low_small_to_large_comb = expand.grid(low_small_to_large_ID, low_small_isolated_ID) %>%
  rename(treatment_ID = Var1) %>%
  rename(isolated_ID = Var2) %>%
  mutate (disturbance = "low") %>%
  mutate (eco_metaeco_type = "S (S_L)")

low_large_to_large_comb = expand.grid(low_large_to_large_ID, low_large_isolated_ID) %>%
  rename(treatment_ID = Var1) %>%
  rename(isolated_ID = Var2) %>%
  mutate (disturbance = "low") %>%
  mutate (eco_metaeco_type = "L (L_L)")

low_large_to_small_comb = expand.grid(low_large_to_small_ID, low_large_isolated_ID) %>%
  rename(treatment_ID = Var1) %>%
  rename(isolated_ID = Var2) %>%
  mutate (disturbance = "low") %>%
  mutate (eco_metaeco_type = "L (S_L)")

high_small_to_small_comb = expand.grid(high_small_to_small_ID, high_small_isolated_ID) %>%
  rename(treatment_ID = Var1) %>%
  rename(isolated_ID = Var2) %>%
  mutate (disturbance = "high") %>%
  mutate (eco_metaeco_type = "S (S_S)")

high_small_to_large_comb = expand.grid(high_small_to_large_ID, high_small_isolated_ID) %>%
  rename(treatment_ID = Var1) %>%
  rename(isolated_ID = Var2) %>%
  mutate (disturbance = "high") %>%
  mutate (eco_metaeco_type = "S (S_L)")

high_large_to_large_comb = expand.grid(high_large_to_large_ID, high_large_isolated_ID) %>%
  rename(treatment_ID = Var1) %>%
  rename(isolated_ID = Var2) %>%
  mutate (disturbance = "high") %>%
  mutate (eco_metaeco_type = "L (L_L)")

high_large_to_small_comb = expand.grid(high_large_to_small_ID, high_large_isolated_ID) %>%
  rename(treatment_ID = Var1) %>%
  rename(isolated_ID = Var2) %>%
  mutate (disturbance = "high") %>%
  mutate (eco_metaeco_type = "L (S_L)")

combinations_treatments_n_isolated = rbind(low_small_to_small_comb,
                                           low_small_to_large_comb,
                                           low_large_to_large_comb,
                                           low_large_to_small_comb,
                                           high_small_to_small_comb,
                                           high_small_to_large_comb,
                                           high_large_to_large_comb,
                                           high_large_to_small_comb)

combinations_n = nrow(combinations_treatments_n_isolated)


###### Let's try something here

#combination = 1

combinations_treatments_n_isolated$time_point = NA
combinations_treatments_n_isolated$mean_bioarea_density_treatment = NA
combinations_treatments_n_isolated$sd_bioarea_density_treatment = NA
combinations_treatments_n_isolated$sam_size_bioarea_density_treatment = NA
combinations_treatments_n_isolated$mean_bioarea_density_treatment_isolated = NA
combinations_treatments_n_isolated$sd_bioarea_density_treatment_isolated = NA
combinations_treatments_n_isolated$sam_size_bioarea_density_treatment_isolated = NA

for (combination in 1:combinations_n){
  for (time_point_input in 0:7){
    
    time_point_input = 7
    combination = 1
    combination_row = ds_biomass_abund %>%
      filter(time_point == time_point_input) %>%
      filter(disturbance == combinations_treatments_n_isolated[combination,]$disturbance) %>%
      filter(culture_ID == combinations_treatments_n_isolated[combination,]$treatment_ID) %>%
      group_by(culture_ID, disturbance, time_point, day, eco_metaeco_type)
    summarise(bioarea_per_volume_video_averaged = mean(bioarea_per_volume))
    
    
    mean_bioarea_density_treatment = combination_row$mean_bioarea_density
    sd_bioarea_density_treatment = combination_row$sd_bioarea_density
    sam_size_bioarea_density_treatment = combination_row$sam_size_bioarea_density
    
    combinations_treatments_n_isolated[combination,]$time_point = time_point_input
    combinations_treatments_n_isolated[combination,]$mean_bioarea_density_treatment[combination] = averaged_bioarea_density %>%
      filter(eco_metaeco_type == )
    
    combinations_treatments_n_isolated[combination,]$sd_bioarea_density_treatment[combination] = ...
    combinations_treatments_n_isolated[combination,]$sam_size_bioarea_density_treatment[combination] = ...
    combinations_treatments_n_isolated[combination,]$mean_bioarea_density_isolated[combination] = ...
    combinations_treatments_n_isolated[combination,]$sd_bioarea_density_isolated[combination] = ...
    combinations_treatments_n_isolated[combination,]$sam_size_bioarea_density_isolated[combination] = ...
    
  }
}



##Old
single_iteration = NULL
iteration = 0
for (combination in 1:combinations_n){
  
  disturbance_type = combinations_treatments_n_isolated$disturbance[combination]
  treatment_type = combinations_treatments_n_isolated$eco_metaeco_type[combination]
  treatment_culture_ID = combinations_treatments_n_isolated$treatment_ID[combination]
  isolated_culture_ID = combinations_treatments_n_isolated$isolated_ID[combination]
  
  for (time_point_input in 0:7){
    
    iteration = iteration + 1
    
    treatment_bioarea_density = ds_biomass %>%
      filter(culture_ID == treatment_culture_ID) %>%
      filter(time_point == time_point_input) %>%
      summarise(bioarea_per_volume_video_average = mean(bioarea_per_volume))
    
    isolated_bioarea_density = ds_biomass %>%
      filter(culture_ID == isolated_culture_ID) %>%
      filter(time_point == time_point_input) %>%
      summarise(bioarea_per_volume_video_average = mean(bioarea_per_volume))
    
    lnRR_bioarea_density_nr = ln(treatment_bioarea_density/isolated_bioarea_density)
    lnRR_bioarea_density_nr = unlist(lnRR_bioarea_density_nr)
    
    single_iteration[[iteration]] = tibble(
      disturbance = disturbance_type,
      eco_metaeco_type = treatment_type,
      time_point = time_point_input,
      day = time_point_input * 4,
      lnRR_bioarea_density = lnRR_bioarea_density_nr)}}
```