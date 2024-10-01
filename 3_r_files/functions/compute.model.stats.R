compute.model.stats = function(full_model,
                               null_model,
                               model_type){
  
  #Delta AIC
  AIC = AIC(full_model, null_model) %>% 
    rownames_to_column("model")
  
  AIC_full = AIC %>%
    filter(model == "full_model") %>%
    pull(AIC)
  
  AIC_null = AIC %>%
    filter(model == "null_model") %>%
    pull(AIC)
  
  deltaAIC = AIC_full - AIC_null
  
  #P-value and R2
  anova = anova(full_model, null_model)
  anova
  
  if(model_type == "linear_model"){
    
    R2_full = glance(full_model)$r.squared
    R2_null = glance(null_model)$r.squared
    R2 =  R2_full - R2_null
    R2 = round(R2, digits = 2)
    
    p_value = anova$`Pr(>F)`[2]
    
  }
  
  if(model_type == "mixed_model"){
    
    R2 = NA
    
    p_value = anova$`Pr(>Chisq)`[2]

  }
  
  #Put all together
  stats = data.frame(deltaAIC = deltaAIC,
                     p_value = p_value,
                     R2 = R2) %>%
    mutate(deltaAIC = round(deltaAIC, digits = 1),
           p_value = round(p_value, digits = 3),
           R2 = NULL,
           evidence = "",
           evidence = ifelse(p_value > 0.1, 
                             "none",
                             evidence),
           evidence = ifelse(p_value < 0.1, 
                             "* weak",
                             evidence),
           evidence = ifelse(p_value < 0.05, 
                             "** moderate",
                             evidence),
           evidence = ifelse(p_value < 0.01, 
                             "*** strong",
                             evidence),
           evidence = ifelse(p_value < 0.001, 
                             "**** very strong",
                             evidence),
           p_value = ifelse(p_value < 0.001,
                            "< 0.001",
                            p_value))
  
  return(stats)
  
}
