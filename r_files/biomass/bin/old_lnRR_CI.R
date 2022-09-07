ds_biomass_averaged_treatments = data.frame(eco_metaeco_type = character(),
                                            disturbance = character(),
                                            time_point = integer(),
                                            mean_bioarea_per_volume = double())

eco_metaeco_types = unique(ds_biomass$eco_metaeco_type)

for (disturbance_input in c("low", "high")){
  for (eco_metaeco_input in eco_metaeco_types){
    for (time_point_input in 0:7){
      
      temporary = ds_biomass %>%
        filter(eco_metaeco_type == eco_metaeco_input) %>%
        filter(disturbance == disturbance_input) %>%
        filter(time_point == time_point_input) %>% 
        group_by(culture_ID, eco_metaeco_type, patch_size, disturbance, time_point, day) %>%
        summarise(bioarea_per_volume_across_videos = mean(bioarea_per_volume)) %>%
        group_by(eco_metaeco_type, patch_size, disturbance, time_point, day) %>%
        summarise(mean_bioarea_per_volume = mean(bioarea_per_volume_across_videos))
      
      ds_biomass_averaged_treatments = rbind(ds_biomass_averaged_treatments, temporary)
      
    }
  }
}

ds_biomass_averaged_treatments$isolated_control = NA
for (patch_size_input in c("S", "L")){
  for (disturbance_input in c("low", "high")){
    for (time_point_input in 0:7){
      
      averaged_value_isolated_control = ds_biomass_averaged_treatments %>%
        filter(eco_metaeco_type == patch_size_input) %>%
        filter(disturbance == disturbance_input) %>%
        filter(time_point == time_point_input) %>%
        ungroup() %>%
        select(mean_bioarea_per_volume)
      
      ds_biomass_averaged_treatments$isolated_control[
        ds_biomass_averaged_treatments$patch_size == patch_size_input & 
          ds_biomass_averaged_treatments$disturbance == disturbance_input &
          ds_biomass_averaged_treatments$time_point == time_point_input] = 
        averaged_value_isolated_control
      
    }
  }
}

ds_biomass_averaged_treatments = ds_biomass_averaged_treatments %>%
  filter(!patch_size == "M") %>%
  mutate(isolated_control = as.numeric(isolated_control)) %>%
  mutate(lnRR_biomass = ln(mean_bioarea_per_volume / isolated_control))

#Takes about 3h to run. 
ds_lnRR = data.frame()
iterations_n = 1000
upper_bound = iterations_n * 0.025
lower_bound = iterations_n * 0.975
rows_to_subsample = 5

for (eco_metaeco_type_input in c("S (S_S)", "S (S_L)", "L (L_L)", "L (S_L)")){
  for (disturbance_input in c("low", "high")){
    for (time_point_input in 0:7){
      
      mean_bioarea_isolated = ds_biomass_averaged_treatments %>%
        filter(eco_metaeco_type == eco_metaeco_type_input) %>%
        filter(time_point == time_point_input) %>%
        filter(disturbance == disturbance_input) %>%
        ungroup() %>%
        select(isolated_control)
      mean_bioarea_isolated = unlist(mean_bioarea_isolated)
      
      mean_bioarea_all_iterations = NULL
      for (iteration in 1:iterations_n){
        
        mean_bioarea_iteration = ds_biomass %>%
          filter(eco_metaeco_type == eco_metaeco_type_input) %>%
          filter(time_point == time_point_input) %>%
          filter(disturbance == disturbance_input) %>%
          group_by(culture_ID, system_nr, eco_metaeco_type, disturbance,time_point,day) %>%
          summarise(mean_bioarea_per_volume_video_averaged = mean(bioarea_per_volume)) %>%
          group_by(system_nr) %>%
          slice_sample(n = 1) %>%
          ungroup() %>%
          slice_sample(n = rows_to_subsample, 
                       replace = TRUE) %>%
          summarise(mean_bioarea_per_volume = mean(mean_bioarea_per_volume_video_averaged))
        mean_bioarea_all_iterations[iteration] = as.numeric(unlist(mean_bioarea_iteration))
        
      }
      
      mean_bioarea_all_iterations = sort(mean_bioarea_all_iterations, decreasing = TRUE)
      
      mean_bioarea_lower_CI =  mean_bioarea_all_iterations[lower_bound]
      mean_bioarea_density = mean_bioarea_all_iterations[iterations_n/2]
      mean_bioarea_upper_CI =  mean_bioarea_all_iterations[upper_bound]
      
      lnRR_bioarea_lower_CI = ln(mean_bioarea_lower_CI/mean_bioarea_isolated)
      lnRR_bioarea_density = ln(mean_bioarea_density/mean_bioarea_isolated)
      lnRR_bioarea_upper_CI = ln(mean_bioarea_upper_CI/mean_bioarea_isolated)
      
      new_row = nrow(ds_lnRR) + 1 
      ds_lnRR[new_row,] = NA
      ds_lnRR$disturbance[new_row] = disturbance_input
      ds_lnRR$eco_metaeco_type[new_row] = eco_metaeco_type_input
      ds_lnRR$time_point[new_row] = time_point_input
      ds_lnRR$day[new_row] = time_point_input*4
      ds_lnRR$lnRR_lower[new_row] = lnRR_bioarea_lower_CI
      ds_lnRR$lnRR[new_row] = lnRR_bioarea_density
      ds_lnRR$lnRR_upper[new_row] = lnRR_bioarea_upper_CI
      
    }
  }
}

write.csv(ds_lnRR, 
          file = here("results", "biomass", "bootstrapped_lnRR_patches.csv"),
          sep = ",",
          col.names = TRUE)

ds_lnRR = read.csv(here("results", "biomass", "bootstrapped_lnRR_patches.csv"), header = TRUE)
ds_lnRR = ds_lnRR %>%
  rename(lnRR_bioarea_density = lnRR)

datatable(ds_lnRR,
          rownames = FALSE,
          options = list(scrollX = TRUE),
          filter = list(position = 'top', 
                        clear = FALSE))