
### --- CHECK THAT THE SPECIES ID WORKED PROPERLY --- ###

#During the video analysis process, we encountered an issue with Blepherisma as it was added to the culture not as a monoculture, but alongside its prey, Chilomonas, which was the same size as Tetrahymena. This made it impossible to remove Chilomonas without removing Tetrahymena from the analysis. To overcome this problem, we conducted two separate analyses.

#The first analysis, referred to as the main analysis, was carried out to analyze the biomass and identify the species. ImageJ software was set so that it would detect smaller protists, including Tetrahymena and Chilomonas, to ensure all protists in the experiment were included in the biomass. The results of this analysis were used for species identification.

#The second analysis, called the Blepherisma analysis, was conducted solely to calculate the density of Blepherisma. To achieve this, ImageJ was set up to exclude smaller protists, allowing us to use the Blepherisma videos as a training dataset after removing its prey. By doing this, we were able to estimate the density of Blepherisma accurately. From this analysis, we only retained the density of Blepherisma for further analysis.

#We now want to make sure that everything worked correctly with the species identification. To do so, we want to look only at the species identification of the main analysis. We will use the number of individuals of Blepherisma that were found in the main analysis - which we here refer to as Ble_main_analysis. So, first let's calculate the percentage of individuals that have identified by the species identification script. 

#Check that all individuals have been identified in the species ID script.
minimum_percentage_of_idd_individuals = ds_patches %>%
  filter(indiv_per_μL > 0) %>%
  mutate(
    indiv_identified_per_μL =
      Ble_main_analysis_indiv_per_μL +
      Cep_main_analysis_indiv_per_μL +
      Col_indiv_per_μL +
      Eug_indiv_per_μL +
      Eup_indiv_per_μL +
      Lox_indiv_per_μL +
      Pau_indiv_per_μL +
      Pca_indiv_per_μL +
      Spi_main_analysis_indiv_per_μL +
      Spi_te_indiv_per_μL +
      Tet_indiv_per_μL,
    individuals_identified_percent = (indiv_identified_per_μL / indiv_per_μL) * 100
  ) %>%
  pull(individuals_identified_percent) %>%
  min() %>%
  round(digits = 5)

if(!minimum_percentage_of_idd_individuals == 100) {
  stop("You didn't ID all the individuals.")
}

#Now that we know that all individuals have been identified, let's look at how they were classified. Let's start from the confusion matrix of main analysis.
#confusion_matrix_main_analysis = 
confusion_main = read.csv(here("data", "species_ID_main_analysis", "confusion_matrix.csv")) %>%
  print()

# confusion_matrix_as_matrix = confusion_matrix_main_analysis %>%
#   select(Ble:Tet) %>%
#   as.matrix()
# 
# indiv_of_this_species_present = rowSums(confusion_matrix_as_matrix)
# indiv_idd_as_this_species = colSums(confusion_matrix_as_matrix)
# indiv_idd_as_this_species_correctly = diag(confusion_matrix_as_matrix)
# indiv_idd_as_this_species_wrongly = indiv_idd_as_this_species - indiv_idd_as_this_species_correctly
# 
# indiv_idd_as_this_species_but_that_are_not = (indiv_idd_as_this_species_wrongly / indiv_idd_as_this_species_correctly) * 100
# indiv_idd_as_this_species_but_that_are_not = round(indiv_idd_as_this_species_but_that_are_not, digits = 1)

#Now the confusion matrix of the Blepherisma analysis.
#confusion_matrix_Ble_analysis = 
confusion_Ble = read.csv(here("data", "species_ID_Ble_analysis", "confusion_matrix.csv")) %>%
  print()

comb_confusion = data.frame(
  species = confusion_main$X[2:10],
  main_analysis = confusion_main$indiv_misclass_percent[2:10],
  Ble_analysis = confusion_Ble$indiv_misclass_percent[2:10]
) %>%
  mutate(ratio_main_Ble = round((main_analysis / Ble_analysis), digits = 1))

comb_confusion$better_analysis <- ifelse(comb_confusion$ratio_main_Ble < 1, 
                                         yes = "main", 
                                         no = "Ble")

comb_confusion

#Let's now build a table for the main analysis and one for the Ble analysis to see if the number of individuals per species that we see in the analysis matches the ones in the videos. BEMOVI, as well as the species identification script I'm using (originally written by Lynn Govaert), calculate the number of individuals per volume as the average number of individuals in each frame (individuals/μL = Σframes of all individuals / frames per video / volume recorded). Therefore, in the tables the number of individuals per frame will represent an average over the frames we recorded. 

# table_main_analysis = ds_patches %>%
#   mutate(indiv_per_frame = indiv_per_μL * volume_recorded_μl,
#          Col_indiv_per_frame = Col_indiv_per_μL * volume_recorded_μl,
#          Eug_indiv_per_frame = Eug_indiv_per_μL * volume_recorded_μl,
#          Eup_indiv_per_frame = Eup_indiv_per_μL * volume_recorded_μl,
#          Lox_indiv_per_frame = Lox_indiv_per_μL * volume_recorded_μl,
#          Pau_indiv_per_frame = Pau_indiv_per_μL * volume_recorded_μl,
#          Pca_indiv_per_frame = Pca_indiv_per_μL * volume_recorded_μl,
#          Spi_te_indiv_per_frame = Spi_te_indiv_per_μL * volume_recorded_μl,
#          Tet_indiv_per_frame = Tet_indiv_per_μL * volume_recorded_μl) %>%
#   select(time_point,
#          file,
#          culture_ID,
#          patch_type,
#          bioarea_µm2_per_μL,
#          indiv_per_frame,
#          paste0(protists_from_main_analysis, "_indiv_per_frame")) %>%
#   mutate(bioarea_µm2_per_μL = round(bioarea_µm2_per_μL, digits = 0))
# 
# write.csv(table_main_analysis, 
#           here("results", "table_main_analysis.csv"),
#           row.names = FALSE)
# 
# table_Ble_analysis = ds_patches %>%
#   mutate(indiv_per_frame = indiv_per_μL * volume_recorded_μl,
#          Ble_indiv_per_frame = Ble_indiv_per_μL * volume_recorded_μl,
#          Cep_indiv_per_frame = Cep_indiv_per_μL * volume_recorded_μl,
#          Spi_indiv_per_frame = Spi_indiv_per_μL * volume_recorded_μl,
#          Spi_te_Ble_analysis_indiv_per_frame = Spi_te_indiv_per_μL * volume_recorded_μl) %>%
#   select(time_point,
#          file,
#          patch_type,
#          culture_ID,
#          bioarea_µm2_per_μL,
#          indiv_per_frame,
#          paste0(protists_from_Ble_analysis, "_indiv_per_frame"),
#          Spi_te_Ble_analysis_indiv_per_frame) %>%
#   mutate(bioarea_µm2_per_μL = round(bioarea_µm2_per_μL, digits = 0))
# 
# write.csv(table_Ble_analysis, 
#           here("results", "table_Ble_analysis.csv"),
#           row.names = FALSE)
# 
# table_analyses_together = cbind(table_main_analysis,
#                                 (
#                                   table_Ble_analysis %>%
#                                     select(Ble_indiv_per_frame:Spi_te_Ble_analysis_indiv_per_frame)
#                                 )) %>%
#   rename(Spi_te_main_analysis_indiv_per_frame = Spi_te_indiv_per_frame) %>%
#   select(time_point,
#          file,
#          culture_ID,
#          patch_type,
#          bioarea_µm2_per_μL,
#          indiv_per_frame,
#          Ble_indiv_per_frame,
#          Cep_indiv_per_frame,
#          Col_indiv_per_frame,
#          Eug_indiv_per_frame, 
#          Eup_indiv_per_frame,
#          Lox_indiv_per_frame,
#          Pau_indiv_per_frame,
#          Pca_indiv_per_frame,
#          Spi_indiv_per_frame,
#          Spi_te_main_analysis_indiv_per_frame,
#          Spi_te_Ble_analysis_indiv_per_frame,
#          Tet_indiv_per_frame)
# 
# unique(table_analyses_together$Spi_te_Ble_analysis_indiv_per_frame == table_analyses_together$Spi_te_main_analysis_indiv_per_frame)
# 
# write.csv(table_analyses_together, 
#           here("results", "table_analyses_together.csv"),
#           row.names = FALSE)

table_analyses_together = ds_patches %>%
  mutate(indiv_per_frame = indiv_per_μL * volume_recorded_μl,
         Ble_indiv_per_frame = Ble_indiv_per_μL * volume_recorded_μl,
         Cep_indiv_per_frame = Cep_indiv_per_μL * volume_recorded_μl,
         Col_indiv_per_frame = Col_indiv_per_μL * volume_recorded_μl,
         Eug_indiv_per_frame = Eug_indiv_per_μL * volume_recorded_μl,
         Eup_indiv_per_frame = Eup_indiv_per_μL * volume_recorded_μl,
         Lox_indiv_per_frame = Lox_indiv_per_μL * volume_recorded_μl,
         Pau_indiv_per_frame = Pau_indiv_per_μL * volume_recorded_μl,
         Pca_indiv_per_frame = Pca_indiv_per_μL * volume_recorded_μl,
         Spi_indiv_per_frame = Spi_indiv_per_μL * volume_recorded_μl,
         Spi_te_indiv_per_frame = Spi_te_indiv_per_μL * volume_recorded_μl,
         Tet_indiv_per_frame = Tet_indiv_per_μL * volume_recorded_μl
  ) %>%
  select(time_point,
         file,
         culture_ID,
         patch_type,
         bioarea_µm2_per_μL,
         indiv_per_frame,
         Ble_indiv_per_frame,
         Cep_indiv_per_frame,
         Col_indiv_per_frame,
         Eug_indiv_per_frame,
         Eup_indiv_per_frame,
         Lox_indiv_per_frame,
         Pau_indiv_per_frame,
         Pca_indiv_per_frame,
         Spi_indiv_per_frame,
         Spi_te_indiv_per_frame,
         Tet_indiv_per_frame) %>%
  mutate_if(is.numeric, round)

write.csv(table_analyses_together, 
          here("results", "table_analyses_together.csv"),
          row.names = FALSE)