#This would be another way of doing it
system_nr_S_low = unique(isolated_S$system_nr)[1:5]
system_nr_L_low = unique(isolated_L$system_nr)[1:5]
system_nr_S_high = unique(isolated_S$system_nr)[6:9]
system_nr_L_high = unique(isolated_L$system_nr)[6:9]

low_pairs = expand.grid(system_nr_S_low,system_nr_L_low)
high_pairs = expand.grid(system_nr_S_high, system_nr_L_high)
pairs = rbind(low_pairs, high_pairs)
number_of_pairs = nrow(pairs)


SL_from_isolated_all_combinations = NULL
for (pair in 1:number_of_pairs){
  
  SL_from_isolated_one_combination = ds_biomass %>%
    filter(system_nr %in% pairs[pair,]) %>%
    group_by(disturbance, day, time_point, system_nr) %>%
    summarise(regional_bioarea_across_videos = mean(bioarea_per_volume)) %>%
    group_by(disturbance, day, time_point) %>%
    summarise(total_regional_bioarea = mean(regional_bioarea_across_videos)) %>%
    mutate(system_nr = 1000 + pair) %>%
    mutate(metaecosystem_type = "S_L_from_isolated")
  
  SL_from_isolated_all_combinations = rbind(SL_from_isolated_one_combination,
                                            SL_from_isolated_all_combinations)
  
}

ds_regional_with_SL_from_isolated = rbind(SL_from_isolated_all_combinations, ds_regional_biomass)

#Next step: bootstrap 5 isolated small patches and 5 isolated large patches