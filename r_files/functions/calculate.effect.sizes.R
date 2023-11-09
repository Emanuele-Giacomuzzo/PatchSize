#From PatchSize
calculate.effect.sizes = function(ds,
                                  vars_to_group_by,
                                  vars_for_ES) {
  
  #Calculate the mean & sd of response variables for each treatment/control at each time point
  ds_effect_size = NULL
  variable_nr = 0
  
  #Delete me 
  #variable_input = "shannon"
  #variable_nr = 1
  
  for (variable_input in vars_for_ES) {
    variable_nr = variable_nr + 1
    
    ds_effect_size[[variable_nr]] = ds %>%
      filter(!is.na(!!sym(variable_input))) %>%
      group_by_at(all_of(vars_to_group_by)) %>%
      summarise(across(all_of(variable_input),
                       list(mean = mean,
                            sd = sd)),
                sample_size = n()) %>%
      rename_with( ~ paste0(variable_input, "_sample_size"), matches("sample_size"))
    
    #ds_effect_size[[variable_nr]] %>% filter(patch_size == "Small", day == 28, disturbance == "high") %>% View()
    
  }
  
  ds_effect_size <- reduce(ds_effect_size, full_join, by = vars_to_group_by)
  
  expect_equal(
    nrow(ds_effect_size),
    (n_controls + n_treatments) * n_time_points * n_disturbance_levels
  )
  
  #Initialise columns
  for (var in vars_for_ES) {
    ds_effect_size <- ds_effect_size %>%
      mutate(
        !!paste0(var, "_d") := NA,!!paste0(var, "_d_upper") := NA,!!paste0(var, "_d_lower") := NA
      )
  }
  
  #Calculate the effect size (Hedge's d)
  row_n = 0
  
  #Delete me 
  #disturbance_input = "high"
  #treatment_input = "Small connected to small"
  #time_point_input = 7
  #row_n = 1
  
  for (disturbance_input in disturbance_levels) {
    for (treatment_input in treatments_and_controls$treatment) {
      for (time_point_input in time_points) {
        row_n = row_n + 1
        
        treatment_row = ds_effect_size %>%
          filter(
            disturbance == disturbance_input,
            patch_type == treatment_input,
            time_point == time_point_input
          )
        
        control_input = treatments_and_controls$control[treatments_and_controls$treatment == treatment_input]
        
        control_row = ds_effect_size %>%
          filter(
            disturbance == disturbance_input,
            patch_type == control_input,
            time_point == time_point_input
          )
        
        for (response_variable in vars_for_ES) {
          
          hedges_d = calculate.hedges_d(
            treatment_row[[paste0(response_variable, "_mean")]],
            treatment_row[[paste0(response_variable, "_sd")]],
            treatment_row[[paste0(response_variable, "_sample_size")]],
            control_row[[paste0(response_variable, "_mean")]],
            control_row[[paste0(response_variable, "_sd")]],
            control_row[[paste0(response_variable, "_sample_size")]]
          )
          
          ds_effect_size[[paste0(response_variable, "_d")]][ds_effect_size$patch_type == treatment_input &
                                                              ds_effect_size$time_point == time_point_input &
                                                              ds_effect_size$disturbance == disturbance_input] =
            hedges_d$d
          
          ds_effect_size[[paste0(response_variable, "_d_upper")]][ds_effect_size$patch_type == treatment_input &
                                                                    ds_effect_size$time_point == time_point_input &
                                                                    ds_effect_size$disturbance == disturbance_input] =
            hedges_d$upper_CI
          
          ds_effect_size[[paste0(response_variable, "_d_lower")]][ds_effect_size$patch_type == treatment_input &
                                                                    ds_effect_size$time_point == time_point_input &
                                                                    ds_effect_size$disturbance == disturbance_input] =
            hedges_d$lower_CI
          
        }
      }
    }
  }
  
  return(ds_effect_size)
  
}