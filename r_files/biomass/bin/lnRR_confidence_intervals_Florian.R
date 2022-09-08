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