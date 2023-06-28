calculate.temporal.divergence = function(){
  
  ds_patches$beta_diversity_from_previous_time = NA
  for(culture_ID_input in 1:n_cultures) {
    for(time_point_input in first_time_point + 1 :last_time_point){ #We are not looking at the first time point because wouldn't make sense
      
      bray_curtis = ds_patches %>%
        filter(
          culture_ID == culture_ID_input,
          time_point %in% c(time_point_input, time_point_input - 1)
        ) %>%
        select(all_of(protist_species_indiv_per_ml)) %>%
        vegdist(.,
                method = "bray")
      
      ds_patches$beta_diversity_from_previous_time[
        ds_patches$culture_ID == culture_ID_input &
        ds_patches$time_point == time_point_input] = 
        bray_curtis
      
    }
  } 
  
  return(ds_patches)
  
}