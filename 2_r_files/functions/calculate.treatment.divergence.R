calculate.treatment.divergence = function() {
  ds_ecosystems$beta_diversity_from_unconnected = NA
  for (time_point_input in time_points) {
    
    for (culture_ID_treatment in 1:n_cultures) {
      
      print("culture ID")
      print(culture_ID_treatment)
      
      if(culture_ID_treatment %in% ecosystems_to_take_off){
        next
      }
      
      if(unique(ds_ecosystems$metaecosystem[ds_ecosystems$culture_ID == culture_ID_treatment]) == "no"){
        next
      }
      
      treatment_ecosystem_type = unique(ds_ecosystems$ecosystem_type[ds_ecosystems$culture_ID == culture_ID_treatment])
      control_ecosystem_type = treatments_and_controls$control[treatments_and_controls$treatment == treatment_ecosystem_type]
      disturbance_input = unique(ds_ecosystems$disturbance[ds_ecosystems$culture_ID == culture_ID_treatment])
      
      treatment_species_vector = ds_ecosystems %>%
        filter(time_point == time_point_input,
               culture_ID == culture_ID_treatment) %>%
        select(all_of(protist_species_indiv_per_ml))
      
      #Test
      expect_equal(nrow(treatment_species_vector),
                   1)
      expect_equal(ncol(treatment_species_vector),
                   length(protist_species))
      
      control_culture_IDs = ds_ecosystems %>%
        filter(ecosystem_type == control_ecosystem_type,
               disturbance == disturbance_input) %>%
        select(culture_ID) %>%
        unique() %>%
        pull()
      
      pairwise_bray_curtis = NA
      row_pairwise_bray_curtis = 0
      
      for (culture_ID_control in control_culture_IDs) {
        
        if(culture_ID_treatment %in% ecosystems_to_take_off){
          next
        }
        
        row_pairwise_bray_curtis = row_pairwise_bray_curtis + 1
        
        control_species_vector = ds_ecosystems %>%
          filter(time_point == time_point_input,
                 culture_ID == culture_ID_control) %>%
          select(all_of(protist_species_indiv_per_ml))
        
        #Test
        expect_equal(nrow(control_species_vector),
                     1)
        expect_equal(ncol(control_species_vector),
                     length(protist_species))
        
        species_control_n_treatment = rbind(treatment_species_vector,
                                            control_species_vector)
        
        #Test
        expect_equal(nrow(species_control_n_treatment),
                     2)
        expect_equal(ncol(species_control_n_treatment),
                     length(protist_species))
        
        pairwise_bray_curtis[row_pairwise_bray_curtis] <-
          vegdist(species_control_n_treatment,
                  method = "bray") %>%
          as.numeric()
        
        expect_length(pairwise_bray_curtis[row_pairwise_bray_curtis],
                      1)
        
      }
      
      ds_ecosystems$beta_diversity_from_unconnected[ds_ecosystems$time_point == time_point_input &
                                                ds_ecosystems$culture_ID ==  culture_ID_treatment] =
        mean(pairwise_bray_curtis)
      
    }
  }
  
  return(ds_ecosystems)
  
}
