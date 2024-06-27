fill.results.table = function(results_table,
                              response_variable,
                              ecosystem_or_metaecosystem_type_input,
                              model_stats_full,
                              model_stats_fixed){
  
  results_table = results_table %>%
    add_row(
      Response = response_variable,
      Levels = paste(ecosystem_or_metaecosystem_type_input, collapse = " , "),
      ΔAIC_full = model_stats_full$deltaAIC,
      p_full = model_stats_full$p_value,
      ΔR2_full = model_stats_full$R2,
      ΔAIC_fix = model_stats_fixed$deltaAIC,
      p_fix = model_stats_fixed$p_value,
      ΔR2_fix = model_stats_fixed$R2
    )
  
  return(results_table)
  
}