update_all_models_table = function(model_name,
                                   all_models_table,
                                   full_model, 
                                   null_model,
                                   metaeco_null,
                                   mixed_or_fixed) {
  
  R2_fixed = NULL
  R2_mixed = NULL

  if (mixed_or_fixed == "mixed") {
  
    #AIC and BIC
    anova = anova(full_model, null_model)
    AIC = round(anova$AIC[2], digits = 2)
    BIC = round(anova$BIC[2], digits = 2)
    
    #Marginal and conditional R2
    R2_full = r.squaredGLMM(full_model)                    
    R2_without_metaeco = r.squaredGLMM(metaeco_null)
    R2_metaeco = R2_full - R2_without_metaeco
    
    R2_mixed = R2_full[2]
    R2_fixed = R2_full[1]
    R2_mixed_M = R2_metaeco[2]
    R2_fixed_M = R2_metaeco[1]
    
    #P-value (not used now)
    anova = anova(full_model, null_model)
    p_whole = round(anova$`Pr(>Chisq)`[2], digits = 5)
    if (p_whole < 0.00001) {p_whole = "< 0.00001"}
    
    
    
    ### Round
    R2_mixed = round(R2_mixed, digits = 2)
    R2_fixed = round(R2_fixed, digits = 2)
    R2_mixed_M = round(R2_mixed_M, digits = 2)
    R2_fixed_M = round(R2_fixed_M, digits = 2)
    
    
  }
  
  if (mixed_or_fixed == "fixed") {
    
    R2_full = summary(full_model)$adj.r.squared
    R2_without_metaeco = summary(metaeco_null)$adj.r.squared
    R2_metaeco = R2_full - R2_without_metaeco
    
    R2_mixed = "NA"
    R2_fixed = "NA"

    R2_mixed_M = "NA"
    R2_fixed_M = R2_metaeco
    
    anova = anova(full_model, null_model)
    p_whole = round(anova$`Pr(>F)`[2], digits = 5)
    if (p_whole < 0.00001) {p_whole = "< 0.00001"}
    
    AIC = round(AIC(full_model), digits = 2)
    BIC = round(BIC(full_model), digits = 2)
    
    ### Round
    R2_fixed_M = round(R2_fixed_M, digits = 2)

  }
  
  #Assign
  row = nrow(all_models_table) + 1
  all_models_table[row,] = "insert data here"
  
  all_models_table$model[row] = model_name
  all_models_table$time_point[row] = paste0(" t2 - t", last_point)
  all_models_table$AIC[row] = AIC
  all_models_table$BIC[row] = BIC
  all_models_table$R2_mixed[row] = R2_mixed
  all_models_table$R2_fixed[row] = R2_fixed
  all_models_table$R2_mixed_M[row] = R2_mixed_M
  all_models_table$R2_fixed_M[row] = R2_fixed_M
  
  return(all_models_table)
  
}