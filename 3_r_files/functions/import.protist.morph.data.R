import.protist.morph.data = function(culture_info,
                                     time_points) {
  
  time_points_ds = NULL
  for (time_point_input in time_points) {
    
    morph_file_path = here("data", "morphology", paste0("t",time_point_input,".csv"))
    
    time_points_ds[[time_point_input+1]] = read.csv(here("data", "morphology", paste0("t",time_point_input,".csv")))
    
    if(time_point_input == 0) {
      
      #Parameters
      n_cultures = nrow(culture_info)
      n_indiv_all_videos = nrow(time_points_ds[[time_point_input + 1]])
      
      #Elongate
      time_points_ds[[time_point_input + 1]] <- map_dfr(1:n_cultures, ~ time_points_ds[[time_point_input + 1]]) %>%
        arrange(id) %>%
        mutate(culture_ID = rep(1:n_cultures, times = n_indiv_all_videos))
      
    }
    
    #Finish up 
    time_points_ds[[time_point_input+1]] = time_points_ds[[time_point_input+1]] %>%
      mutate(time_point = time_point_input,
             day = sampling_days[time_point_input + 1],
             file = as.double(str_extract(file, "\\d+"))) %>%
      select(time_point,
             day,
             file,
             culture_ID,
             mean_area,
             N_frames)
    
    #Add the video replicate which I forgot to put into the description file 
    if (time_point_input == 0) {
      time_points_ds[[time_point_input + 1]] = time_points_ds[[time_point_input + 1]] %>%
        mutate(video_replicate = file,
               video_replicate = as.double(video_replicate))
      
    }
    
    if(time_point_input %in% 1:7){
      time_points_ds[[time_point_input + 1]] = time_points_ds[[time_point_input + 1]] %>%
        mutate(video_replicate = ifelse(file %in% 1:110, 
                                        yes = 1, 
                                        no = 2),
               video_replicate = as.double(video_replicate))
    }
    
  }
  
  ds_individuals = time_points_ds %>%
    bind_rows() %>%
    left_join(culture_info,
              by = "culture_ID") %>%
    select(time_point,
           day, 
           file, 
           video_replicate,
           disturbance, 
           culture_ID, 
           system_nr, 
           eco_metaeco_type,
           ecosystem_size, 
           ecosystem_size_volume,
           metaecosystem, 
           metaecosystem_type,
           ecosystem_size_volume, 
           mean_area,
           N_frames
           )
  
  return(ds_individuals)
  
}
