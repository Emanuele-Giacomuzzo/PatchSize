create.res.vs.fit.ecos = function(data,
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
                         data$culture_ID, 
                         "<br>",
                         "Day: ", 
                         data$day, 
                         "<br>",
                         "Patch Type: ", 
                         data$ecosystem_type, 
                         "<br>",
                         "Biomass density: ", 
                         round(data$bioarea_mm2_per_ml, digits = 2), 
                         "<br>",
                         "Species richness: ", 
                         data$species_richness, 
                         "<br>"),
            hoverinfo = "text") %>%
    plotly::layout(title = "Residuals vs. Fitted Values",
                   xaxis = list(title = "Fitted Values"),
                   yaxis = list(title = "Residuals"))
  
  return(res_vs_fit)
}
