create.res.vs.fit.metaecos = function(data,
                                      model){
  
  res_vs_fit = data %>%
    mutate(predicted = fitted(model),
           residuals = resid(model)) %>%
    plot_ly(x = ~predicted,
            y = ~residuals,
            type = "scatter",
            mode = "markers",
            marker = list(size = 5, color = "#4C78A8"),
            text = paste(" ID: ", 
                         data$system_nr, 
                         "<br>",
                         "Day: ", 
                         data$day, 
                         "<br>",
                         "Patch Type: ", 
                         data$metaecosystem_type, 
                         "<br>",
                         "Biomass density: ", 
                         round(data$total_metaecosystem_bioarea_mm2, digits = 2), 
                         "<br>",
                         "Species richness: ", 
                         data$mean_shannon, 
                         "<br>"),
            hoverinfo = "text") %>%
    plotly::layout(title = "Residuals vs. Fitted Values",
                   xaxis = list(title = "Fitted Values"),
                   yaxis = list(title = "Residuals"))
  
  data %>%
    mutate(predicted = fitted(model),
           residuals = resid(model)) %>%
    ggplot(aes(x = predicted,
               y = residuals)) +
    geom_point()
  
  return(res_vs_fit)
  
}
