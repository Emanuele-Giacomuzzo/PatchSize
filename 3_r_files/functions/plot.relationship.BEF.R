plot.relationship.BEF = function(ecosystem_type_input,
                                 response_variable) {
  
  ecosystem_type_input <- ecosystem_type_input[order(match(ecosystem_type_input, 
                                                   ecosystem_types_ordered))]
  
  ds_ecosystems %>%
      filter(ecosystem_type == ecosystem_type_input) %>%
      ggplot(aes(x = get(response_variable),
                 y = bioarea_mm2_per_ml)) +
      geom_point() +
      facet_wrap( ~ time_point) +
      labs(title = paste0("Patch type = ",
                          ecosystem_type_input),
           x = response_variable)
}