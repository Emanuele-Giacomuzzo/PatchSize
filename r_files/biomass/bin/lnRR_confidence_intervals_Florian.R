#Here I try to calculate the lnRR (biomass density) confidence intervals as suggested by Florian (I do all the combinations 
#of treatment and isolated).

low_small_isolated_nr = culture_info %>%
  filter(disturbance == "low") %>%
  filter(eco_metaeco_type == "S") %>%
  select(culture_ID)
low_small_isolated_nr = unlist(low_small_isolated_nr)

low_small_to_small_nr = culture_info %>%
  filter(disturbance == "low") %>%
  filter(eco_metaeco_type == "S (S_S)") %>%
  select(culture_ID)
low_small_to_small_nr = unlist(low_small_to_small_nr)

low_small_to_large_nr = culture_info %>%
  filter(disturbance == "low") %>%
  filter(eco_metaeco_type == "S (S_L)") %>%
  select(culture_ID)
low_small_to_large_nr = unlist(low_small_to_large_nr)

low_large_isolated_nr = culture_info %>%
  filter(disturbance == "low") %>%
  filter(eco_metaeco_type == "L") %>%
  select(culture_ID)
low_large_isolated_nr = unlist(low_large_isolated_nr)

low_large_to_large_nr = culture_info %>%
  filter(disturbance == "low") %>%
  filter(eco_metaeco_type == "L (L_L)") %>%
  select(culture_ID)
low_large_to_large_nr = unlist(low_large_to_lage_nr)

low_large_to_small_nr = culture_info %>%
  filter(disturbance == "low") %>%
  filter(eco_metaeco_type == "L (S_L)") %>%
  select(culture_ID)
low_large_to_small_nr = unlist(low_large_to_small_nr)

high_small_isolated_nr = culture_info %>%
  filter(disturbance == "high") %>%
  filter(eco_metaeco_type == "S") %>%
  select(culture_ID)
high_small_isolated_nr = unlist(high_small_isolated_nr)

high_small_to_small_nr = culture_info %>%
  filter(disturbance == "high") %>%
  filter(eco_metaeco_type == "S (S_S)") %>%
  select(culture_ID)
high_small_to_small_nr = unlist(high_small_to_small_nr)

high_small_to_large_nr = culture_info %>%
  filter(disturbance == "high") %>%
  filter(eco_metaeco_type == "S (S_L)") %>%
  select(culture_ID)
high_small_to_large_nr = unlist(high_small_to_large_nr)

high_large_isolated_nr = culture_info %>%
  filter(disturbance == "high") %>%
  filter(eco_metaeco_type == "L") %>%
  select(culture_ID)
high_large_isolated_nr = unlist(high_large_isolated_nr)

high_large_to_large_nr = culture_info %>%
  filter(disturbance == "high") %>%
  filter(eco_metaeco_type == "L (L_L)") %>%
  select(culture_ID)
high_large_to_large_nr = unlist(high_large_to_large_nr)

high_large_to_small_nr = culture_info %>%
  filter(disturbance == "high") %>%
  filter(eco_metaeco_type == "L (S_L)") %>%
  select(culture_ID)
high_large_to_small_nr = unlist(high_large_to_small_nr)


low_small_to_small_comb = expand.grid(low_small_to_small_nr, low_small_isolated_nr) %>%
  rename(treatment_ID = Var1) %>%
  rename(isolated_ID = Var2) %>%
  mutate (disturbance = "low") %>%
  mutate (eco_metaeco_type = "S (S_S)")

low_small_to_large_comb = expand.grid(low_small_to_large_nr, low_small_isolated_nr) %>%
  rename(treatment_ID = Var1) %>%
  rename(isolated_ID = Var2) %>%
  mutate (disturbance = "low") %>%
  mutate (eco_metaeco_type = "S (S_L)")

low_large_to_large_comb = expand.grid(low_large_to_large_nr, low_large_isolated_nr) %>%
  rename(treatment_ID = Var1) %>%
  rename(isolated_ID = Var2) %>%
  mutate (disturbance = "low") %>%
  mutate (eco_metaeco_type = "L (L_L)")

low_large_to_small_comb = expand.grid(low_large_to_small_nr, low_large_isolated_nr) %>%
  rename(treatment_ID = Var1) %>%
  rename(isolated_ID = Var2) %>%
  mutate (disturbance = "low") %>%
  mutate (eco_metaeco_type = "L (S_L)")

high_small_to_small_comb = expand.grid(high_small_to_small_nr, high_small_isolated_nr) %>%
  rename(treatment_ID = Var1) %>%
  rename(isolated_ID = Var2) %>%
  mutate (disturbance = "high") %>%
  mutate (eco_metaeco_type = "S (S_S)")

high_small_to_large_comb = expand.grid(high_small_to_large_nr, high_small_isolated_nr) %>%
  rename(treatment_ID = Var1) %>%
  rename(isolated_ID = Var2) %>%
  mutate (disturbance = "high") %>%
  mutate (eco_metaeco_type = "S (S_L)")

high_large_to_large_comb = expand.grid(high_large_to_large_nr, high_large_isolated_nr) %>%
  rename(treatment_ID = Var1) %>%
  rename(isolated_ID = Var2) %>%
  mutate (disturbance = "high") %>%
  mutate (eco_metaeco_type = "L (L_L)")

high_large_to_small_comb = expand.grid(high_large_to_small_nr, high_large_isolated_nr) %>%
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
      lnRR_bioarea_density = lnRR_bioarea_density_nr)
    
  }
}

all_iterations = single_iteration %>%
  bind_rows()

##Explore
all_iterations %>%
  filter(time_point == 1) %>%
  filter(lnRR_bioarea_density == "NaN")


low_small_to_small_t7_iterations = all_iterations %>%
  filter(eco_metaeco_type == "S (S_L)") %>%
  filter(time_point == 7) %>%
  ungroup() %>%
  select(lnRR_bioarea_density)

low_small_to_small_t7_iterations = sort(low_small_to_small_t7_iterations, decreasing = TRUE)