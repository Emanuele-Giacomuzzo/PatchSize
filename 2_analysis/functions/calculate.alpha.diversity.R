calculate.alpha.diversity = function() {
  ds_ecosystems = ds_ecosystems %>%
    mutate(
      species_richness = NA,
      shannon = NA,
      simpson = NA,
      inv_simpson = NA,
      evenness_pielou = NA
    )
  
  for (row in 1:nrow(ds_ecosystems)) {
    
    species_vector = ds_ecosystems %>%
      slice(row) %>%
      select(all_of(protist_species_indiv_per_ml))
    
    ds_ecosystems[row, ]$species_richness = specnumber(species_vector)
    
    ds_ecosystems[row,]$simpson = diversity(species_vector,
                                         index = "simpson")
    
    ds_ecosystems[row,]$shannon = diversity(species_vector,
                                         index = "shannon")
    
    ds_ecosystems[row,]$inv_simpson = diversity(species_vector,
                                             index = "invsimpson")
    
    ds_ecosystems[row,]$evenness_pielou = diversity(species_vector, index = "shannon") /
      log(specnumber(species_vector))
    
  }
  
  ds_ecosystems = ds_ecosystems %>%
    mutate(shannon = ifelse(species_richness == 0,
                            yes = NA,
                            no = shannon))
  
  return(ds_ecosystems)
  
}