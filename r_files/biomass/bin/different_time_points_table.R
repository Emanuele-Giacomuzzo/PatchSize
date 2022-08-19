{r small-data-table}
#Create a table in which the regional biomass has been log transformed. 

### --- INITIALISE TABLE --- ###

columns = c("model", "time_point", "AIC", "BIC", "R2_mixed", "R2_fixed", "R2_mixed_M", "R2_fixed_M")
small_patches_matrix = matrix(ncol = length(columns), nrow = 0)
small_patches_table = data.frame(small_patches_matrix)
colnames(small_patches_table) = columns

### --- POPULATE THE TABLE --- ###

for (last_point in 4:7) {
  
  full_model = lmer(lnRR_bioarea_per_volume ~
                      day +
                      eco_metaeco_type +
                      disturbance +
                      day : disturbance + 
                      day : eco_metaeco_type +
                      (day | culture_ID),
                    data = ds_biomass_averaged_across_videos %>%
                      filter(time_point >= 2) %>%
                      filter(time_point <= last_point) %>%
                      filter(eco_metaeco_type== "S (S_S)" | 
                               eco_metaeco_type == "S (S_L)"),
                    REML = FALSE,
                    control = lmerControl(optimizer = 'optimx', 
                                          optCtrl = list(method = 'L-BFGS-B')))
  
  r.squaredGLMM(full_model)
  
  
  null_model = lm(lnRR_bioarea_per_volume ~
                    1,
                  data = ds_biomass_averaged_across_videos %>%
                    filter(time_point >= 2) %>%
                    filter(time_point <= last_point) %>%
                    filter(eco_metaeco_type== "S (S_S)" | 
                             eco_metaeco_type == "S (S_L)"))
  
  metaeco_null = lmer(lnRR_bioarea_per_volume ~
                        day +
                        disturbance +
                        day : disturbance +
                        day : eco_metaeco_type + 
                        (day | culture_ID),
                      data = ds_biomass_averaged_across_videos %>%
                        filter(time_point >= 2) %>%
                        filter(time_point <= last_point) %>%
                        filter(eco_metaeco_type== "S (S_S)" | 
                                 eco_metaeco_type == "S (S_L)"),
                      REML = FALSE,
                      control = lmerControl(optimizer = "Nelder_Mead"))
  r.squaredGLMM(metaeco_null)
  
  small_patches_table = update_all_models_table("t + M + D + t*M + t*D + (t | ID)",
                                                small_patches_table, 
                                                full_model, 
                                                null_model,
                                                metaeco_null,
                                                "mixed")
}

datatable(small_patches_table, 
          rownames = FALSE,
          options = list(pageLength = 100,
                         scrollX = TRUE,
                         autoWidth = TRUE,
                         columnDefs = list(list(targets=c(0),visible=TRUE, width='160'),
                                           list(targets=c(1), visible=TRUE, width='10'),
                                           list(targets=c(2), visible=TRUE, width='10'),
                                           list(targets=c(3), visible=TRUE, width='10'),
                                           list(targets=c(4), visible=TRUE, width='10'),
                                           list(targets=c(5), visible=TRUE, width='10'),
                                           list(targets=c(6), visible=TRUE, width='10'),
                                           list(targets=c(7), visible=TRUE, width='10'),
                                           list(targets='_all', visible=FALSE))),
          caption = "
          M = Meta-ecosystem type, 
          D = disturbance, 
          (1 | t) = random effect of time on the intercept,
          (1 | ID) = random effect of meta-ecosystem ID on the intercept, 
          || = no correlation between intercept and slope,
          | = correlation between intercept and slope,
          R2 = r squared of the whole model,
          R2_fixed = fixed part of the mixed model,
          mixed_R2 = r squared when considering both fixed and random effects (conditional r squared), 
          fixed_R2 = r squared when considering only the fixed effects (marginal r squared)")