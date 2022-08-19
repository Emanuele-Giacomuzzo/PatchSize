## ----small-time-point-input---------------------------------------------------------------------------------------------------------------------------------
time_point_input = 4

full = lmer(lnRR_bioarea_per_volume ~
              eco_metaeco_type +
              disturbance +
              disturbance : eco_metaeco_type +
              (1 | culture_ID),
            data = ds_biomass_averaged_across_videos %>%
              filter(time_point == time_point_input) %>%
              filter(eco_metaeco_type== "S (S_S)" | 
                     eco_metaeco_type == "S (S_L)"),
          REML = FALSE)


## ----small-no-random-effects-single-point, eval = FALSE-----------------------------------------------------------------------------------------------------
## no_random = lm(lnRR_bioarea_per_volume ~
##               eco_metaeco_type +
##               disturbance +
##               disturbance : eco_metaeco_type,
##             data = ds_biomass_averaged_across_videos %>%
##               filter(time_point == time_point_input) %>%
##               filter(eco_metaeco_type== "S (S_S)" |
##                      eco_metaeco_type == "S (S_L)"))
## 
## anova(full, no_random)


## ----small-no-DM-single-point, eval = FALSE-----------------------------------------------------------------------------------------------------------------
## no_MD = lmer(lnRR_bioarea_per_volume ~
##               eco_metaeco_type +
##               disturbance +
##               (1 | culture_ID),
##             data = ds_biomass_averaged_across_videos %>%
##               filter(time_point == time_point_input) %>%
##               filter(eco_metaeco_type== "S (S_S)" |
##                      eco_metaeco_type == "S (S_L)"),
##           REML = FALSE)
## 
## anova(full, no_MD)

