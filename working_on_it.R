disturbance_nr = 0
MM_isolated_sys_sets <- vector("list",
                               length(disturbance_levels))

for (disturbance_input in disturbance_levels) {
  
  disturbance_nr = disturbance_nr + 1
  
  ID_small_patches = ds_patches %>%
    filter(disturbance == disturbance_input,
           patch_type == "Small isolated") %>%
    pull(culture_ID) %>%
    unique()
  
  ID_large_patches = ds_patches %>%
    filter(disturbance == disturbance_input,
           patch_type == "Large isolated") %>%
    pull(culture_ID) %>%
    unique()
  
  #Force small and large patches vectors to have the same length
  length_difference <- length(ID_small_patches) - length(ID_large_patches)
  if (length_difference > 0) {
    
    ID_large_patches = c(ID_large_patches,
                         rep("Patch taken off",
                             times = abs(length(ID_small_patches) - 
                                           length(ID_large_patches))))
    
  } else if (length_difference < 0) {
    
    ID_small_patches = c(ID_small_patches,
                         rep("Patch taken off",
                             times = abs(length(ID_large_patches) - 
                                           length(ID_small_patches))))
  }
  
  # Create dataframe
  permutations_large = permn(ID_large_patches)
  
  MM_isolated_sys_sets[[disturbance_nr]] = data.frame(
    disturbance = disturbance_input,
    metaecosystem_type = "Small-Large isolated",
    ID_first_patch = rep(ID_small_patches, 
                         times = length(permutations_large)),
    ID_second_patch = unlist(permutations_large),
    connection = "isolated",
    set = rep(1 : length(permutations_large), 
              each = length(ID_small_patches)))
  
  expect_equal(nrow(MM_isolated_sys_sets[[disturbance_nr]]),
               length(ID_small_patches) * length(permutations_large))
  
  MM_isolated_sys_sets[[disturbance_nr]] = MM_isolated_sys_sets[[disturbance_nr]] %>%
    filter(!ID_first_patch == "Patch taken off",
           !ID_second_patch == "Patch taken off") %>%
    mutate(ID_first_patch = as.double(ID_first_patch),
           ID_second_patch = as.double(ID_second_patch)) %>%
    full_join(patch_combinations %>%
                filter(disturbance == disturbance_input, 
                       metaecosystem_type == "Small-Large isolated"))
  
}

MM_isolated_sys_sets_before_binding = MM_isolated_sys_sets
MM_isolated_sys_sets = MM_isolated_sys_sets %>%
  bind_rows()