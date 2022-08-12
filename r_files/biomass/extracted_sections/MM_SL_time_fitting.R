## ----regional biomass plot-----------------------------------------------------------------------------------
ds_regional %>%
  filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L") %>%
  ggplot(aes(x = day,
             y = regional_mean_bioarea,
             group = day)) + 
  geom_boxplot() +
  labs(x = "day", y = "Regional bioarea (something/microlitres)")  +
  geom_vline(xintercept = first_perturbation_day + 0.7, 
             linetype="dotdash", 
             color = "grey", 
             size=0.7) +
  labs(caption = "Vertical grey line: first perturbation")


## ----time-function-------------------------------------------------------------------------------------------

a1 = -0.1
a4 = 1200
a5 = -1

day = seq(0, 30, 0.01)
biomass = a4*(day-a5) * exp(a1*(day-a5))
plot(biomass ~ day)


## ----parameterise-time-function, results = FALSE-------------------------------------------------------------

ds_regional_shrunk_type = ds_regional %>%
    filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L")

model = nls(regional_mean_bioarea ~ a4 * (day-a5) * exp(a1 * (day-a5)), 
            start = list(a1 = -0.1, a4 = 1200, a5 = -1),
            trace = T,
            data = ds_regional_shrunk_type)

a1 = as.numeric(model$m$getPars()[1])
a4 = as.numeric(model$m$getPars()[2])
a5 = as.numeric(model$m$getPars()[3])


## ----show-fitted-parameters----------------------------------------------------------------------------------
model$m$getPars()


## ------------------------------------------------------------------------------------------------------------

day = seq(0,30,1)
predicted = a4*(day-a5)*exp(a1*(day-a5))
data_fitted=data.frame(day=day,regional_mean_bioarea=predicted)

ds_regional_shrunk_type%>%
  ggplot(aes(x = day,
             y = regional_mean_bioarea,
             group = day)) + 
  geom_boxplot() +
  labs(x = "Day", y = "Regional bioarea") +
  geom_line(data=data_fitted,aes(x = day, y=regional_mean_bioarea),color="red", group = 1) +
  geom_vline(xintercept = first_perturbation_day + 0.7, 
             linetype="dotdash", 
             color = "grey", 
             size=0.7) +
  labs(caption = "Vertical grey line: first perturbation")


## ----predicted-ds--------------------------------------------------------------------------------------------

ds_regional_predicted_shrunk_type = ds_regional %>%
  mutate(predicted_from_time = a4*(day-a5)*exp(a1*(day-a5))) %>%
  filter(metaecosystem_type == "S_L" | metaecosystem_type == "M_M")

