size_class[[class]] = ds_body_size%>%
  filter(bin_lower_limit <= mean_area) %>%
  filter(mean_area <= bin_upper_limit) %>%
  group_by(culture_ID, 
           system_nr, 
           disturbance, 
           day, 
           patch_size, 
           metaecosystem, 
           metaecosystem_type, 
           eco_metaeco_type, 
           replicate_video) %>% #Group by video
  summarise(mean_abundance_across_videos = n()) %>%
  group_by(culture_ID, 
           system_nr, 
           disturbance, 
           day, 
           patch_size, 
           metaecosystem, 
           metaecosystem_type, 
           eco_metaeco_type) %>% #Group by ID
  summarise(abundance = mean(mean_abundance_across_videos)) %>%
  mutate(log_abundance = log(abundance)) %>%
  mutate(size_class = class) %>%
  mutate(size = mean_size) %>%
  mutate(log_size = log(size))