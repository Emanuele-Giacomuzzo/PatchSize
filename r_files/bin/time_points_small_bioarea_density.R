Let's now look at the full model and see if we should keep the interaction between meta-ecosystem type and disturbance. We are not using mixed effects because a certain system nr can't be at different perturbations or at different meta-ecosystem types.

$$
  Local \: \: Bioarea \: Density = P + D + PD
  $$
    
    Time point = 2
    
    {r}
    chosen_time_point = 2
    
    {r}
    full = lm(lnRR_bioarea_density ~
                eco_metaeco_type +
                disturbance +
                eco_metaeco_type * disturbance,
              data = ds_lnRR_bioarea_density %>%
                filter(time_point == chosen_time_point) %>%
                filter(eco_metaeco_type == "S (S_S)" | eco_metaeco_type == "S (S_L)"))
    
    Should we keep P * D?
      
      {r}
    no_PD = lm(lnRR_bioarea_density ~
                 eco_metaeco_type +
                 disturbance,
               data = ds_lnRR_bioarea_density %>%
                 filter(time_point == chosen_time_point) %>%
                 filter(eco_metaeco_type == "S (S_S)" | eco_metaeco_type == "S (S_L)"))
    
    AIC(full,no_PD)
    
    No.
    
    {r}
    best_model = no_PD
    
    par(mfrow=c(2,3))
    plot(best_model, which = 1:5)
    
    R2_full = glance(best_model)$r.squared
    
    no_M = lm(lnRR_bioarea_density ~
                disturbance,
              data = ds_lnRR_bioarea_density %>%
                filter(time_point == chosen_time_point) %>%
                filter(eco_metaeco_type == "S (S_S)" | eco_metaeco_type == "S (S_L)"))
    
    R2_no_M = glance(no_M)$r.squared
    R2_M = R2_full - R2_no_M
    
    R2_full = round(R2_full, digits = 2)
    R2_M = round(R2_M, digits = 2)
    
    The adjusted R squared of the model is r R2_full and the adjusted R squared of patch type is r R2_M.
    
    Time point = 3
    
    {r}
    chosen_time_point = 3
    
    {r}
    full = lm(lnRR_bioarea_density ~
                eco_metaeco_type +
                disturbance +
                eco_metaeco_type * disturbance,
              data = ds_lnRR_bioarea_density %>%
                filter(time_point == chosen_time_point) %>%
                filter(eco_metaeco_type == "S (S_S)" | eco_metaeco_type == "S (S_L)"))
    
    Should we keep P * D?
      
      {r}
    no_PD = lm(lnRR_bioarea_density ~
                 eco_metaeco_type +
                 disturbance,
               data = ds_lnRR_bioarea_density %>%
                 filter(time_point == chosen_time_point) %>%
                 filter(eco_metaeco_type == "S (S_S)" | eco_metaeco_type == "S (S_L)"))
    
    AIC(full,no_PD)
    
    Yes.
    
    {r}
    best_model = full
    
    par(mfrow=c(2,3))
    plot(best_model, which = 1:5)
    
    R2_full = glance(best_model)$r.squared
    
    no_M = lm(lnRR_bioarea_density ~
                disturbance,
              data = ds_lnRR_bioarea_density %>%
                filter(time_point == chosen_time_point) %>%
                filter(eco_metaeco_type == "S (S_S)" | eco_metaeco_type == "S (S_L)"))
    
    R2_no_M = glance(no_M)$r.squared
    R2_M = R2_full - R2_no_M
    
    R2_full = round(R2_full, digits = 2)
    R2_M = round(R2_M, digits = 2)
    
    The adjusted R squared of the model is r R2_full and the adjusted R squared of patch type is r R2_M (which includes also the interaction with disturbance).
    
    Time point = 4
    
    {r}
    chosen_time_point = 4
    
    {r}
    full = lm(lnRR_bioarea_density ~
                eco_metaeco_type +
                disturbance +
                eco_metaeco_type * disturbance,
              data = ds_lnRR_bioarea_density %>%
                filter(time_point == chosen_time_point) %>%
                filter(eco_metaeco_type == "S (S_S)" | eco_metaeco_type == "S (S_L)"))
    
    Should we keep P * D?
      
      {r}
    no_PD = lm(lnRR_bioarea_density ~
                 eco_metaeco_type +
                 disturbance,
               data = ds_lnRR_bioarea_density %>%
                 filter(time_point == chosen_time_point) %>%
                 filter(eco_metaeco_type == "S (S_S)" | eco_metaeco_type == "S (S_L)"))
    
    AIC(full,no_PD)
    
    No.
    
    {r}
    best_model = no_PD
    
    par(mfrow=c(2,3))
    plot(best_model, which = 1:5)
    
    R2_full = glance(best_model)$r.squared
    
    no_M = lm(lnRR_bioarea_density ~
                disturbance,
              data = ds_lnRR_bioarea_density %>%
                filter(time_point == chosen_time_point) %>%
                filter(eco_metaeco_type == "S (S_S)" | eco_metaeco_type == "S (S_L)"))
    
    R2_no_M = glance(no_M)$r.squared
    R2_M = R2_full - R2_no_M
    
    R2_full = round(R2_full, digits = 2)
    R2_M = round(R2_M, digits = 2)
    
    The adjusted R squared of the model is r R2_full and the adjusted R squared of patch type is r R2_M.
    
    Time point = 5
    
    {r}
    chosen_time_point = 5
    
    {r}
    full = lm(lnRR_bioarea_density ~
                eco_metaeco_type +
                disturbance +
                eco_metaeco_type * disturbance,
              data = ds_lnRR_bioarea_density %>%
                filter(time_point == chosen_time_point) %>%
                filter(eco_metaeco_type == "S (S_S)" | eco_metaeco_type == "S (S_L)"))
    
    Should we keep P * D?
      
      {r}
    no_PD = lm(lnRR_bioarea_density ~
                 eco_metaeco_type +
                 disturbance,
               data = ds_lnRR_bioarea_density %>%
                 filter(time_point == chosen_time_point) %>%
                 filter(eco_metaeco_type == "S (S_S)" | eco_metaeco_type == "S (S_L)"))
    
    AIC(full,no_PD)
    
    No.
    
    {r}
    best_model = no_PD
    
    par(mfrow=c(2,3))
    plot(best_model, which = 1:5)
    
    R2_full = glance(best_model)$r.squared
    
    no_M = lm(lnRR_bioarea_density ~
                disturbance,
              data = ds_lnRR_bioarea_density %>%
                filter(time_point == chosen_time_point) %>%
                filter(eco_metaeco_type == "S (S_S)" | eco_metaeco_type == "S (S_L)"))
    
    R2_no_M = glance(no_M)$r.squared
    R2_M = R2_full - R2_no_M
    
    R2_full = round(R2_full, digits = 2)
    R2_M = round(R2_M, digits = 2)
    
    The adjusted R squared of the model is r R2_full and the adjusted R squared of patch type is r R2_M.
    
    Time point = 6
    
    {r}
    chosen_time_point = 6
    
    {r}
    full = lm(lnRR_bioarea_density ~
                eco_metaeco_type +
                disturbance +
                eco_metaeco_type * disturbance,
              data = ds_lnRR_bioarea_density %>%
                filter(time_point == chosen_time_point) %>%
                filter(eco_metaeco_type == "S (S_S)" | eco_metaeco_type == "S (S_L)"))
    
    Should we keep P * D?
      
      {r}
    no_PD = lm(lnRR_bioarea_density ~
                 eco_metaeco_type +
                 disturbance,
               data = ds_lnRR_bioarea_density %>%
                 filter(time_point == chosen_time_point) %>%
                 filter(eco_metaeco_type == "S (S_S)" | eco_metaeco_type == "S (S_L)"))
    
    AIC(full,no_PD)
    
    No.
    
    {r}
    best_model = no_PD
    
    par(mfrow=c(2,3))
    plot(best_model, which = 1:5)
    
    R2_full = glance(best_model)$r.squared
    
    no_M = lm(lnRR_bioarea_density ~
                disturbance,
              data = ds_lnRR_bioarea_density %>%
                filter(time_point == chosen_time_point) %>%
                filter(eco_metaeco_type == "S (S_S)" | eco_metaeco_type == "S (S_L)"))
    
    R2_no_M = glance(no_M)$r.squared
    R2_M = R2_full - R2_no_M
    
    R2_full = round(R2_full, digits = 2)
    R2_M = round(R2_M, digits = 2)
    
    The adjusted R squared of the model is r R2_full and the adjusted R squared of patch type is r R2_M.
    
    Time point = 7
    
    {r}
    chosen_time_point = 7
    
    {r}
    full = lm(lnRR_bioarea_density ~
                eco_metaeco_type +
                disturbance +
                eco_metaeco_type * disturbance,
              data = ds_lnRR_bioarea_density %>%
                filter(time_point == chosen_time_point) %>%
                filter(eco_metaeco_type == "S (S_S)" | eco_metaeco_type == "S (S_L)"))
    
    Should we keep P * D?
      
      {r}
    no_PD = lm(lnRR_bioarea_density ~
                 eco_metaeco_type +
                 disturbance,
               data = ds_lnRR_bioarea_density %>%
                 filter(time_point == chosen_time_point) %>%
                 filter(eco_metaeco_type == "S (S_S)" | eco_metaeco_type == "S (S_L)"))
    
    AIC(full,no_PD)
    
    No.
    
    {r}
    best_model = no_PD
    
    par(mfrow=c(2,3))
    plot(best_model, which = 1:5)
    
    R2_full = glance(best_model)$r.squared
    
    no_M = lm(lnRR_bioarea_density ~
                disturbance,
              data = ds_lnRR_bioarea_density %>%
                filter(time_point == chosen_time_point) %>%
                filter(eco_metaeco_type == "S (S_S)" | eco_metaeco_type == "S (S_L)"))
    
    R2_no_M = glance(no_M)$r.squared
    R2_M = R2_full - R2_no_M
    
    R2_full = round(R2_full, digits = 2)
    R2_M = round(R2_M, digits = 2)
    
    The adjusted R squared of the model is r R2_full and the adjusted R squared of patch type is r R2_M.