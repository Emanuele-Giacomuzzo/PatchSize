create.meatecosystem.ds = function(){
  
  #Compute meta-ecosystems  for each time point 
  ds_metaecosystems = NULL
  row_n = 0
  for (combination in 1:n_patches_combinations){
    for (time_point_input in time_points) {
      
      row_n = row_n + 1
      
      current_day = sampling_days[time_point_input+1]
      current_system_nr = combinations_isolated_n_metaecos[combination,]$system_nr
      current_combination = combinations_isolated_n_metaecos[combination,]$patches_combined
      current_disturbance = combinations_isolated_n_metaecos[combination,]$disturbance
      current_metaeco_type = combinations_isolated_n_metaecos[combination,]$metaecosystem_type
      current_IDs = c(combinations_isolated_n_metaecos[combination,]$ID_first_patch,
                      combinations_isolated_n_metaecos[combination,]$ID_second_patch)
      
      if (current_system_nr %in% metaecosystems_to_take_off)
        next
      
      if(current_IDs[1] == current_IDs[2])
        next
      
      species_vector_two_patches = ds_patches %>%
        filter(time_point == time_point_input,
               culture_ID %in% current_IDs) %>%
        ungroup() %>%
        select(all_of(protist_species_indiv_per_ml))
      
      absence_presence_two_patches <- ifelse(species_vector_two_patches > 0, 1, 0)
      
      #Alpha diversity: Shannon (mean between the two patches)
      shannon_patch_1 = diversity(species_vector_two_patches[1,], index = "shannon")
      shannon_patch_2 = diversity(species_vector_two_patches[2,], index = "shannon")
      shannon_value = (shannon_patch_1 + shannon_patch_2) / 2
      
      #Alpha diversity: Species richness (mean between the two patches)
      richness_patch_1 = specnumber(species_vector_two_patches[1,])
      richness_patch_2 = specnumber(species_vector_two_patches[2,])
      mean_richness_value = (richness_patch_1 + richness_patch_2) / 2
      
      #Beta diversity: Jaccard
      jaccard_index_value = vegdist(species_vector_two_patches, 
                              method = "jaccard") %>%
        as.numeric()
      
      #Beta diversity: Bray Curtis
      bray_curtis_value = vegdist(species_vector_two_patches, 
                            method = "bray") %>%
        as.numeric()
      
      #Beta diversity: partitioning of beta diversity from Sorensen index into turnover (Simpson pair-wise dissimilarity) and nestedness (nestedness-fraction of Sorensen)
      betapart_core_object = betapart.core(absence_presence_two_patches)
      beta_spatial_turnover_value = beta.pair(betapart_core_object)$beta.sim %>% as.double()
      beta_nestedness_value = beta.pair(betapart_core_object)$beta.sne %>% as.double()
      beta_total_value = beta.pair(betapart_core_object)$beta.sor %>% as.double()
      
      #Gamma diversity: Meta-ecosystem richness
      metaecosystem_richness_value = colSums(species_vector_two_patches) %>%
        specnumber()
      
      #Put everything together
      ds_metaecosystems[[row_n]] = ds_patches %>%
        filter(culture_ID %in% current_IDs,
               time_point == time_point_input) %>%
        summarise(total_metaecosystem_bioarea_mm2 = sum(bioarea_tot_mm2),
                  total_metaecosystem_Ble_indiv = sum(Ble_tot_indiv),
                  total_metaecosystem_Cep_indiv = sum(Cep_tot_indiv),
                  total_metaecosystem_Col_indiv = sum(Col_tot_indiv),
                  total_metaecosystem_Eug_indiv = sum(Eug_tot_indiv),
                  total_metaecosystem_Eup_indiv = sum(Eup_tot_indiv),
                  total_metaecosystem_Lox_indiv = sum(Lox_tot_indiv),
                  total_metaecosystem_Pau_indiv = sum(Pau_tot_indiv),
                  total_metaecosystem_Pca_indiv = sum(Pca_tot_indiv),
                  total_metaecosystem_Spi_indiv = sum(Spi_tot_indiv),
                  total_metaecosystem_Spi_te_indiv = sum(Spi_te_tot_indiv),
                  total_metaecosystem_Tet_indiv = sum(Tet_tot_indiv)) %>% 
        mutate(system_nr = current_system_nr,
               patches_combined = current_combination,
               metaecosystem_type = current_metaeco_type,
               disturbance = current_disturbance,
               time_point = time_point_input,
               day = current_day,
               jaccard_index = jaccard_index_value,
               bray_curtis = bray_curtis_value,
               beta_spatial_turnover = beta_spatial_turnover_value,
               beta_nestedness = beta_nestedness_value,
               beta_total = beta_total_value,
               metaecosystem_richness = metaecosystem_richness_value,
               mean_shannon = shannon_value,
               mean_richness = mean_richness_value) %>%
        ungroup()
      
    }
  }
  
  ds_metaecosystems = ds_metaecosystems %>%
    bind_rows() %>%
    as.data.frame() %>%
    select(time_point,
           day,
           system_nr,
           patches_combined,
           disturbance,
           metaecosystem_type,
           mean_shannon,
           mean_richness,
           jaccard_index,
           bray_curtis,
           beta_spatial_turnover,
           beta_nestedness,
           beta_total,
           metaecosystem_richness,
           total_metaecosystem_bioarea_mm2,
           paste0("total_metaecosystem_",protist_species,"_indiv"))
  
  return(ds_metaecosystems)
  
}
