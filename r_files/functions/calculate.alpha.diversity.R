calculate.alpha.diversity = function() {
  ds_patches = ds_patches %>%
    mutate(
      species_richness = NA,
      shannon = NA,
      simpson = NA,
      inv_simpson = NA,
      evenness_pielou = NA
    )
  
  for (row in 1:nrow(ds_patches)) {
    
    species_vector = ds_patches %>%
      slice(row) %>%
      select(all_of(protist_species_indiv_per_ml))
    
    ds_patches[row, ]$species_richness = specnumber(species_vector)
    
    ds_patches[row,]$simpson = diversity(species_vector,
                                         index = "simpson")
    
    ds_patches[row,]$shannon = diversity(species_vector,
                                         index = "shannon")
    
    ds_patches[row,]$inv_simpson = diversity(species_vector,
                                             index = "invsimpson")
    
    ds_patches[row,]$evenness_pielou = diversity(species_vector, index = "shannon") /
      log(specnumber(species_vector))
    
  }
  
  ds_patches = ds_patches %>%
    mutate(shannon = ifelse(species_richness == 0,
                            yes = NA,
                            no = shannon))
  
  return(ds_patches)
  
}