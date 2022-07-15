biomass_plots = function(disturbance_input){
  
  p = NULL
  
  p[[1]] <<- ds %>%
    filter ( disturbance == disturbance_input) %>%
    filter (metaecosystem_type == "S_L" | metaecosystem_type == "M_M") %>%
    group_by(culture_ID, day, metaecosystem_type, system_nr, patch_size) %>%
    summarise(patch_mean_bioarea_across_videos = mean(bioarea_per_volume)) %>%
    group_by(day,metaecosystem_type, system_nr) %>%
    summarise(bioarea_per_volume = mean(patch_mean_bioarea_across_videos)) %>%
    ggplot (aes(x = day,
                y = bioarea_per_volume,
                group = interaction(day, metaecosystem_type),
                fill = metaecosystem_type,
                color = metaecosystem_type)) +
    geom_boxplot() +
    labs(x = "Day", y = "Regional biomass (average bioarea between 2 patches/µl)", color='Meta-ecosystem type', fill='Meta-ecosystem type') +
    scale_y_continuous(limits = c(0, 6250)) +
    scale_x_continuous(limits = c(-2, 30)) +
    scale_fill_discrete(labels = c("Patches of same size", "Patches of different size")) +
    scale_color_discrete(labels = c("Patches of same size", "Patches of different size")) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  p[[2]] <<- ds %>%
    filter ( disturbance == disturbance_input) %>%
    filter (metaecosystem_type == "S_L" | metaecosystem_type == "M_M") %>%
    group_by(culture_ID, day, metaecosystem_type, system_nr, patch_size) %>%
    summarise(patch_mean_bioarea_across_videos = mean(bioarea_per_volume)) %>%
    group_by(day,metaecosystem_type, system_nr) %>%
    summarise(bioarea_per_volume = mean(patch_mean_bioarea_across_videos)) %>%
    ggplot (aes(x = day,
                y = bioarea_per_volume,
                fill = metaecosystem_type,
                color = metaecosystem_type)) +
    geom_point(stat = "summary", fun = "mean") +
    geom_line (stat = "summary", fun = "mean") +
    labs(x = "Day", y = "Regional biomass (average bioarea between 2 patches/µl)", color='Meta-ecosystem type', fill='Meta-ecosystem type') +
    scale_y_continuous(limits = c(0, 6250)) +
    scale_x_continuous(limits = c(-2, 30)) +
    scale_fill_discrete(labels = c("Patches of same size", "Patches of different size")) +
    scale_color_discrete(labels = c("Patches of same size", "Patches of different size")) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  p[[3]] <<- ds %>%
    filter(disturbance == disturbance_input)%>%
    filter(metaecosystem == "no") %>%
    ggplot(aes(x = day,
               y = bioarea_per_volume,
               group = interaction(day, eco_metaeco_type),
               fill = eco_metaeco_type,
               color = eco_metaeco_type)) +
    geom_boxplot() +
    labs(x = "Day", y = "Local biomass (bioarea/µl)", color='Closed ecosystem size', fill='Closed ecosystem size') +
    scale_y_continuous(limits=c(0, 6250)) +
    scale_x_continuous(limits = c(-2, 30)) +
    scale_fill_discrete(labels = c("Small", "Medium", "Large")) +
    scale_color_discrete(labels = c("Small", "Medium", "Large")) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  p[[4]] <<- ds %>%
    filter(disturbance == disturbance_input)%>%
    filter(metaecosystem == "no") %>%
    ggplot(aes(x = day,
               y = bioarea_per_volume,
               fill = eco_metaeco_type,
               color = eco_metaeco_type)) +
    geom_point(stat = "summary", fun = "mean") +
    geom_line(stat = "summary", fun = "mean") +
    labs(x = "Day", y = "Local biomass (bioarea/µl)", color='Closed ecosystem size', fill='Closed ecosystem size') +
    scale_y_continuous(limits=c(0, 6250)) +
    scale_x_continuous(limits = c(-2, 30)) +
    scale_fill_discrete(labels = c("Small", "Medium", "Large")) +
    scale_color_discrete(labels = c("Small", "Medium", "Large")) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  p[[5]] <<- ds %>%
    filter(disturbance == disturbance_input)%>%
    filter (eco_metaeco_type == "S" | eco_metaeco_type == "S (S_S)" | eco_metaeco_type == "S (S_L)") %>%
    filter (eco_metaeco_type != "S") %>% #If I want to add the S ecosystem patch I can just commment this out
    ggplot(aes(x = day,
               y = bioarea_per_volume,
               group = interaction(day, eco_metaeco_type),
               fill = eco_metaeco_type,
               color = eco_metaeco_type)) +
    geom_boxplot() +
    labs(x = "Day", y = "Local biomass (bioarea/µl)", color='Patch type', fill='Patch type') +
    scale_y_continuous(limits=c(0, 6250)) +
    scale_x_continuous(limits = c(-2, 30)) +
    scale_fill_discrete(labels = c("Connected to same sized patch", "Connected to larger patch")) +
    scale_color_discrete(labels = c("Connected to same sized patch", "Connected to larger patch")) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  p[[6]] <<- ds %>%
    filter(disturbance == disturbance_input)%>%
    filter (eco_metaeco_type == "S" | eco_metaeco_type == "S (S_S)" | eco_metaeco_type == "S (S_L)") %>%
    filter (eco_metaeco_type != "S") %>% #If I want to add the S ecosystem patch I can just commment this out
    ggplot(aes(x = day,
               y = bioarea_per_volume,
               fill = eco_metaeco_type,
               color = eco_metaeco_type)) +
    geom_point(stat = "summary", fun = "mean") +
    geom_line(stat = "summary", fun = "mean") +
    labs(x = "Day", y = "Local biomass (bioarea/µl)", color='Patch type', fill='Patch type') +
    scale_y_continuous(limits=c(0, 6250)) +
    scale_x_continuous(limits = c(-2, 30)) +
    scale_fill_discrete(labels = c("Connected to same sized patch", "Connected to larger patch")) +
    scale_color_discrete(labels = c("Connected to same sized patch", "Connected to larger patch")) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
}