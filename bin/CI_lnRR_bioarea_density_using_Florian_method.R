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
low_large_to_large_nr = unlist(low_large_to_large_nr)

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
                                           high_large_to_small_comb) %>%
  filter(!treatment_ID %in% ecosystems_to_take_off) %>%
  filter(!isolated_ID %in% ecosystems_to_take_off)

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
    
    treatment_bioarea_density = ds_biomass_abund %>%
      filter(culture_ID == treatment_culture_ID) %>%
      filter(time_point == time_point_input) %>%
      summarise(bioarea_per_volume_video_average = mean(bioarea_per_volume))
    
    isolated_bioarea_density = ds_biomass_abund %>%
      filter(culture_ID == isolated_culture_ID) %>%
      filter(time_point == time_point_input) %>%
      summarise(bioarea_per_volume_video_average = mean(bioarea_per_volume))
    
    lnRR_bioarea_density_nr = ln(treatment_bioarea_density / isolated_bioarea_density)
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

## Look at NaN
# NaN_iterations = all_iterations %>%
#   filter(lnRR_bioarea_density == "NaN")
# NaN_patches = unique(NaN_iterations[,c('disturbance','eco_metaeco_type','time_point')])

single_row = NULL
row_number = 0
for (disturbance_input in c("low", "high")){
  for (eco_metaeco_type_input in c("S (S_S)", "S (S_L)", "M (M_M)", "L (L_L)", "L (S_L)")){
    for (time_point_input in 0:7){
      
      row_number = row_number + 1
      
      iterations_disturb_eco_time_point = all_iterations %>% #The problem is here
        filter(disturbance == disturbance_input,
               eco_metaeco_type == eco_metaeco_type_input,
               time_point == time_point_input) %>%
        ungroup() %>%
        select(lnRR_bioarea_density)
      
      iterations_disturb_eco_time_point = sort(unlist(iterations_disturb_eco_time_point), decreasing = TRUE)
      
      n_of_combinations = length(iterations_disturb_eco_time_point)
      n_combinations_to_take_off = 2
      n_of_combinations_to_keep = n_of_combinations - n_combinations_to_take_off
      
      is.even = function (nr) {nr %% 2 == 0}
      if (is.even(n_of_combinations) == TRUE){lnRR_bioarea_density = mean(iterations_disturb_eco_time_point[(n_of_combinations/2)], iterations_disturb_eco_time_point[(n_of_combinations/2)+1])}
      if (is.even(n_of_combinations) == FALSE){lnRR_bioarea_density = iterations_disturb_eco_time_point[(n_of_combinations/2)+0.5]}
      
      confidence_interval_level = 100 - (n_combinations_to_take_off / (n_of_combinations/100))
      upper_confidence_interval = iterations_disturb_eco_time_point[2]
      lower_confidence_interval = iterations_disturb_eco_time_point[n_of_combinations - 1]
      
      if(eco_metaeco_type_input %in% small_patches){patch_size_input = "S"}
      if(eco_metaeco_type_input %in% medium_patches){patch_size_input = "M"}
      if(eco_metaeco_type_input %in% large_patches){patch_size_input = "L"}
      
      single_row[[row_number]] = tibble(
        disturbance = disturbance_input,
        eco_metaeco_type = eco_metaeco_type_input,
        patch_size = patch_size_input,
        time_point = time_point_input,
        day = time_point_input * 4,
        CI_level = confidence_interval_level,
        lnRR_bioarea_density = lnRR_bioarea_density,
        lnrRR_bioarea_density_upper_CI = upper_confidence_interval,
        lnrRR_bioarea_density_lower_CI = lower_confidence_interval)}}}

ds_effect_size_bioarea_density_with_CI = single_row %>%
  bind_rows()

ds_effect_size_bioarea_density$bioarea_density_lnRR_CI_upper = NA
ds_effect_size_bioarea_density$bioarea_density_lnRR_CI_lower = NA
for (disturbance_input in c("low", "high")){
  for (eco_metaeco_type_input in c("S (S_S)", "S (S_L)", "L (L_L)", "L (S_L)")){
    for (time_point_input in 0:7){
      
      ds_effect_size_bioarea_density$bioarea_density_lnRR_CI_upper[ds_effect_size_bioarea_density$disturbance == disturbance_input &
                                                                     ds_effect_size_bioarea_density$eco_metaeco_type == eco_metaeco_type_input &
                                                                     ds_effect_size_bioarea_density$time_point == time_point_input] =
        ds_effect_size_bioarea_density_with_CI$lnrRR_bioarea_density_upper_CI[ds_effect_size_bioarea_density_with_CI$disturbance == disturbance_input &
                                                                                ds_effect_size_bioarea_density_with_CI$eco_metaeco_type == eco_metaeco_type_input &
                                                                                ds_effect_size_bioarea_density_with_CI$time_point == time_point_input]
      
      ds_effect_size_bioarea_density$bioarea_density_lnRR_CI_lower[ds_effect_size_bioarea_density$disturbance == disturbance_input &
                                                                     ds_effect_size_bioarea_density$eco_metaeco_type == eco_metaeco_type_input &
                                                                     ds_effect_size_bioarea_density$time_point == time_point_input] =
        ds_effect_size_bioarea_density_with_CI$lnrRR_bioarea_density_lower_CI[ds_effect_size_bioarea_density_with_CI$disturbance == disturbance_input &
                                                                                ds_effect_size_bioarea_density_with_CI$eco_metaeco_type == eco_metaeco_type_input &
                                                                                ds_effect_size_bioarea_density_with_CI$time_point == time_point_input]
      
    }}}

saveRDS(ds_effect_size_bioarea_density_with_CI, file = here("results", "ds_effect_size_bioarea_density_with_CI.RData"))

ds_effect_size_bioarea_density_with_CI = readRDS(here("results", "ds_effect_size_bioarea_density_with_CI.RData"))

for (disturbance_input in c("low", "high")) {
  
  print(ds_effect_size_bioarea_density_with_CI %>%
          filter(!time_point == 0) %>% #At time point 0 all cultures were the same 
          filter(disturbance == disturbance_input) %>%
          filter(eco_metaeco_type == "S (S_S)" | eco_metaeco_type == "S (S_L)") %>%
          ggplot(aes(x = day,
                     y = lnRR_bioarea_density,
                     color = eco_metaeco_type)) +
          geom_point(position = position_dodge(0.5)) +
          geom_line(position = position_dodge(0.5)) + 
          labs(title = paste("Disturbance =", disturbance_input),
               x = "Day",
               y = "LnRR bioarea density",
               color = "") +
          geom_errorbar(aes(ymin = lnrRR_bioarea_density_upper_CI, 
                            ymax = lnrRR_bioarea_density_lower_CI), 
                        width = .2,
                        position = position_dodge(0.5)) + 
          scale_color_discrete(labels = c("small connected to large", 
                                          "small connnected to small")) +
          theme_bw() +
          theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                legend.position = c(.40, .95),
                legend.justification = c("right", "top"),
                legend.box.just = "right",
                legend.margin = margin(6, 6, 6, 6)) +
          #geom_vline(xintercept = first_perturbation_day + 0.7, 
          #           linetype="dotdash", 
          #           color = "grey", 
          #           size=0.7) +
          geom_hline(yintercept = 0, 
                     linetype = "dotted", 
                     color = "black", 
                     size = 0.7))
}

for (disturbance_input in c("low", "high")) {
  
  print(ds_effect_size_bioarea_density_with_CI %>%
          filter(!time_point == 0) %>% #At time point 0 all cultures were the same 
          filter(disturbance == disturbance_input) %>%
          filter(eco_metaeco_type == "S (S_S)" | eco_metaeco_type == "S (S_L)") %>%
          ggplot(aes(x = day,
                     y = lnRR_bioarea_density,
                     color = eco_metaeco_type)) +
          geom_point(position = position_dodge(0.5)) +
          geom_line(position = position_dodge(0.5)) + 
          labs(title = paste("Disturbance =", disturbance_input),
               x = "Day",
               y = "LnRR bioarea density",
               color = "") +
          geom_errorbar(aes(ymin = lnrRR_bioarea_density_upper_CI, 
                            ymax = lnrRR_bioarea_density_lower_CI), 
                        width = .2,
                        position = position_dodge(0.5)) + 
          scale_color_discrete(labels = c("small connected to large", 
                                          "small connnected to small")) +
          theme_bw() +
          theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                legend.position = c(.40, .95),
                legend.justification = c("right", "top"),
                legend.box.just = "right",
                legend.margin = margin(6, 6, 6, 6)) +
          #geom_vline(xintercept = first_perturbation_day + 0.7, 
          #           linetype="dotdash", 
          #           color = "grey", 
          #           size=0.7) +
          geom_hline(yintercept = 0, 
                     linetype = "dotted", 
                     color = "black", 
                     size = 0.7))
}

