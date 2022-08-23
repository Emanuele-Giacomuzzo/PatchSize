## ----eval = FALSE--------------------------------------------------------------------------------------------------
## full = lmer(log10(total_regional_bioarea + 1) ~
##             metaecosystem_type +
##             disturbance +
##             metaecosystem_type : disturbance +
##             (1 | system_nr),
##             data = ds_regional_biomass %>%
##               filter(time_point == 3) %>%
##               filter(metaecosystem_type == "M_M" |
##                      metaecosystem_type == "S_L"),
##             REML = FALSE)
## 
## # Next step: understanding what is a grouping factor. I need to understand what a grouping factor because I cannot run the full model, as it gives me the following problem: "Error: number of levels of each grouping factor must be < number of observations (problems: system_nr)."

