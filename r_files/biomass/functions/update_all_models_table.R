update_all_models_table = function(model_name,
                                   all_models_table,
                                   full_model, 
                                   null_model,
                                   metaeco_null,
                                   mixed_or_fixed) {
  
  R2_range = NULL
  fixed_R2_whole = NULL
  mixed_R2_whole = NULL
  
  #R2
  
  if (mixed_or_fixed == "mixed") {
  
    R2_whole = round(r.squaredGLMM(full_model), digits = 2)
    R2_metaeco = round(R2_whole - r.squaredGLMM(metaeco_null), digits = 2)
    
    R2 = R2_whole[2]
    mixed_R2_whole = R2_whole[2]
    mixed_R2_metaeco = R2_metaeco[2]
    fixed_R2_whole = R2_whole[1]
    fixed_R2_metaeco = R2_metaeco[1]

    anova = anova(full_model, null_model)
    p_whole = round(anova$`Pr(>Chisq)`[2], digits = 5)
    if (p_whole < 0.00001) {p_whole = "< 0.00001"}
    
    AIC_whole = round(anova$AIC[2], digits = 2)
    BIC_whole = round(anova$BIC[2], digits = 2)
    
    
    
  }
  
  if (mixed_or_fixed == "fixed") {
    
    R2_whole = round(summary(full_model)$adj.r.squared, digits = 2)
    R2_metaeco = round(R2_whole - summary(metaeco_null)$adj.r.squared, digits = 2)
    
    R2 = R2_whole
    mixed_R2_whole = "NA"
    fixed_R2_whole = "NA"
    mixed_R2_metaeco = "NA"
    fixed_R2_metaeco = R2_metaeco
    
    anova = anova(full_model, null_model)
    p_whole = round(anova$`Pr(>F)`[2], digits = 5)
    if (p_whole < 0.00001) {p_whole = "< 0.00001"}
    
    AIC_whole = round(AIC(full_model), digits = 2)
    BIC_whole = round(BIC(full_model), digits = 2)
    
  }
  
  #Assign
  row = nrow(all_models_table) + 1
  all_models_table[row,] = "insert data here"
  
  all_models_table$model[row] = model_name
  all_models_table$time_point[row] = paste0(" t2 - t", last_point)
  all_models_table$AIC[row] = AIC_whole
  all_models_table$BIC[row] = BIC_whole
  all_models_table$R2[row] = R2
  all_models_table$R2_mixed[row] = mixed_R2_whole
  all_models_table$R2_fixed[row] = fixed_R2_whole
  all_models_table$R2_meta_mixed[row] = mixed_R2_metaeco
  all_models_table$R2_meta_fixed[row] = fixed_R2_metaeco
  
  return(all_models_table)
  
}