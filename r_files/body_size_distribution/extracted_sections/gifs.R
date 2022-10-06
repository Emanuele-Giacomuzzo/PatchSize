## ----fig.align='center'--------------------------------------------------------------------------------------------------------------------------------------------
#Takes about 7 minutes to run 

start = Sys.time()
p = list()
n = 0
first_level = c("isolated small", 
                "isolated small", 
                "isolated large", 
                "isolated large")
second_level = c("small connected to small", 
                 "small connected to small", 
                 "large connected to large", 
                 "large connected to large")
third_level = c("small connected to large", 
                "small connected to large", 
                "large connected to small", 
                "large connected to small")

for (patch_size_input in c("S", "L")){
  
  for(disturbance_input in c("low", "high")){
  
    n = n + 1
      
  title = paste(patch_size_input,
                "patches, Disturbance =", 
                disturbance_input, 
                ", Day: {round(frame_time, digits = 0)}")
  
  p[[n]] <- ds_classes %>%
  filter(disturbance == disturbance_input) %>%
  filter(patch_size == patch_size_input) %>%
  ggplot(aes(x = log_size,
             y = log_abundance,
             group = interaction(log_size, eco_metaeco_type),
             color = eco_metaeco_type)) +
  geom_point(stat = "summary", fun = "mean") +
  geom_line(stat = "summary", fun = "mean", aes(group=eco_metaeco_type)) +
  scale_color_discrete(labels = c(first_level[n], 
                                 second_level[n],
                                 third_level[n])) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.position = c(.95, .95),
          legend.justification = c("right", "top"),
          legend.box.just = "right",
          legend.margin = margin(6, 6, 6, 6)) +
  labs(title = title,
       x = 'Log size (μm2)', 
       y = 'Log abundance + 1 (indiv/μm2)',
       color = "") +
  transition_time(day) +
  ease_aes('linear')
  
  animate(p[[n]], 
        duration = 20,
        fps = 25, 
        width = 500, 
        height = 500, 
        renderer = gifski_renderer())
  
  file_name = paste0("transition_day_",patch_size_input,"_",disturbance_input,".gif")
  anim_save(here("gifs", file_name))
  
}
}

end = Sys.time()
running_time = end - start

