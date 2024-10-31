show.tidy.model.stats = function(stats){
  
  stats = stats %>%
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
