## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
ds_local_S_t2_t7 = ds %>%
  filter (eco_metaeco_type == "S" | 
          eco_metaeco_type == "S (S_S)") %>%
  filter(time_point >= 2) #Let's take off the first two time points which are before the first disturbance event.


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
ds_local_S_t2_t7 %>%
    ggplot(aes(x = day,
               y = bioarea_per_volume,
               fill = eco_metaeco_type,
               color = eco_metaeco_type)) +
    geom_point(stat = "summary", fun = "mean") +
    geom_line(stat = "summary", fun = "mean") +
    labs(x = "Day", 
         y = "Local biomass (bioarea/µl)", 
         color = '', 
         fill = '') +
    scale_fill_discrete(labels = c("Isolated patch", 
                                   "Patch connected to patch of the same size")) +
    scale_color_discrete(labels = c("Isolated patch", 
                                    "Patch connected to patch of the same size")) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
full_model = lmer(bioarea_per_volume ~ 
                    metaecosystem_type  + 
                    disturbance + 
                    metaecosystem_type * disturbance + 
                    (metaecosystem_type || day) + 
                    (disturbance || day) + 
                    (metaecosystem_type*disturbance  || day),
                  data = ds_local_S_t2_t7, 
                  REML = FALSE)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------

no_metaeco_type_model = lmer(bioarea_per_volume ~ 
                    disturbance + 
                    (disturbance || day),
                  data = ds_local_S_t2_t7, 
                  REML = FALSE)

anova(full_model, no_metaeco_type_model)

