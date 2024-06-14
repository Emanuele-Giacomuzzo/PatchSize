calculate.alpha.diversity = function()

#Calculate alpha diversity (shannon, simpson, inv simpson, evenness)

ds_patches$shannon = NA
ds_patches$simpson = NA
ds_patches$inv_simpson = NA
ds_patches$evenness_pielou = NA

for (row in 1:nrow(ds_patches)){
  
  species_vector = ds_patches %>%
    slice(row) %>%
    select(Ble:Tet)
  
  simpson = diversity(species_vector, 
                      index = "simpson")
  
  shannon = diversity(species_vector, 
                      index = "shannon")
  
  inv_simpson = diversity(species_vector, 
                          index = "invsimpson")
  
  evenness_pielou = diversity(species_vector) / log(specnumber(species_vector))
  
  ds_patches[row,]$shannon = shannon
  ds_patches[row,]$simpson = simpson
  ds_patches[row,]$inv_simpson = inv_simpson
  ds_patches[row,]$evenness_pielou = evenness_pielou
  
}