calculate.effect.sizes.classes = function(ds,
                                          vars_to_group_by,
                                          vars_for_ES) {
  #Calculate the mean & sd of response variables for each treatment/control at each time point
  ds_effect_size <- ds %>%
    group_by_at(vars(all_of(vars_to_group_by))) %>%
    summarise(across(all_of(vars_for_ES),
                     list(mean = mean,
                          sd = sd)),
              sample_size = n())
  
  test_that("Check the number of rows.",
            {expect_equal(nrow(ds_effect_size),
                          n_time_points * n_disturbance_levels * (n_treatments + n_controls) * n_size_classes)}
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
  for (time_point_input in time_points) {
    for (disturbance_input in disturbance_levels) {
      for (treatment_input in treatments_and_controls$treatment) {
        for (class_input in 1:n_size_classes) {
          row_n = row_n + 1
          
          print(paste("Row", 
                      row_n, 
                      "Time point", 
                      time_point_input, 
                      "Disturbance", 
                      disturbance_input,
                      "Treatment",
                      treatment_input,
                      "Class",
                      class_input))
          
          treatment_row = ds_effect_size %>%
            filter(
              time_point == time_point_input,
              disturbance == disturbance_input,
              patch_type == treatment_input,
              size_class_n == class_input
            )
          
          control_input = treatments_and_controls$control[treatments_and_controls$treatment == treatment_input]
          
          control_row = ds_effect_size %>%
            filter(
              time_point == time_point_input,
              disturbance == disturbance_input,
              patch_type == control_input,
              size_class_n == class_input
            )
          
          for (response_variable in vars_for_ES) {
            hedges_d = calculate.hedges_d(
              treatment_row[[paste0(response_variable, "_mean")]],
              treatment_row[[paste0(response_variable, "_sd")]],
              treatment_row$sample_size,
              control_row[[paste0(response_variable, "_mean")]],
              control_row[[paste0(response_variable, "_sd")]],
              control_row$sample_size
            )
            
            ds_effect_size[[paste0(response_variable, "_d")]][
              ds_effect_size$time_point == time_point_input & 
              ds_effect_size$disturbance == disturbance_input &
              ds_effect_size$patch_type == treatment_input &
              ds_effect_size$size_class_n == class_input] =
              hedges_d$d
            
            ds_effect_size[[paste0(response_variable, "_d_upper")]][
              ds_effect_size$time_point == time_point_input & 
              ds_effect_size$disturbance == disturbance_input &
              ds_effect_size$patch_type == treatment_input &
              ds_effect_size$size_class_n == class_input] =
              hedges_d$upper_CI
            
            ds_effect_size[[paste0(response_variable, "_d_lower")]][
              ds_effect_size$time_point == time_point_input & 
              ds_effect_size$disturbance == disturbance_input &
              ds_effect_size$patch_type == treatment_input &
              ds_effect_size$size_class_n == class_input] =
              hedges_d$lower_CI
            
          }
        }
      }
    }
  }
  
  return(ds_effect_size)
  
}
