fill.iterated.results.table = function(iterated_results_table,
                                       response_variable,
                                       patch_or_metaecosystem_type_i,
                                       model_stats_full,
                                       model_stats_reduced) {
  
  iterated_results_table = iterated_results_table %>%
    add_row(
      Response = response_variable,
      Levels = paste(patch_or_metaecosystem_type_i, collapse = " , "),
      ΔAIC_full = model_stats_full$deltaAIC,
      p_full = model_stats_full$p_value,
      ΔR2_full = model_stats_full$R2,
      ΔAIC_reduced = model_stats_reduced$deltaAIC,
      p_reduced = model_stats_reduced$p_value,
      ΔR2_reduced = model_stats_reduced$R2
    )
  
  return(iterated_results_table)
  
}
