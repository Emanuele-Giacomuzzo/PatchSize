import.protist.pop.data = function(culture_info,
                                   time_points_ds,
                                   protist_species) {
  
  #Assemble the dataset. When importing the data, each row is a video.
  time_points_ds = NULL
  for (time_point_input in time_points) {
    
    #File paths
    pop_data_file_path = here("data", "population", paste0("t", time_point_input, ".csv"))
    Ble_ID_file_path = here("data","species_ID_Ble_analysis", paste0("species_ID_t", time_point_input, ".csv"))
    main_ID_file_path = here("data", "species_ID_main_analysis", paste0("species_ID_t", time_point_input, ".csv"))
    
    #Import data
    species_ID_Ble = read.csv(Ble_ID_file_path) %>%
      select(file,
             Ble,
             Cep,
             Spi)
    
    species_ID_main = read.csv(main_ID_file_path) %>%
      select(file,
             Ble:Tet) %>%
      rename(
        Ble_main_analysis = Ble,
        Cep_main_analysis = Cep,
        Spi_main_analysis = Spi
      )
    
    #Merge data
    time_points_ds[[time_point_input + 1]] = read.csv(pop_data_file_path) %>%
      left_join(species_ID_Ble,
                by = "file") %>%
      left_join(species_ID_main,
                by = "file")
    
    #Initially, at time point 0, 12 videos were captured of the large bottle from which the community was assembled. In order to analyze the data, each of these 12 videos are assigned to each culture.
    if (time_point_input == 0) {
      
      #Parameters
      n_videos = nrow(time_points_ds[[time_point_input + 1]])
      n_cultures = nrow(culture_info)
      
      #Elongate
      time_points_ds[[time_point_input + 1]] = map_dfr(1:n_cultures, ~ time_points_ds[[time_point_input + 1]]) %>%
        arrange(file) %>%
        mutate(culture_ID = rep(1:n_cultures, times = n_videos_taken_t0))
      
    }
    
    #Add the video replicate which I forgot to put into the description file 
    if(time_point_input == 0){
      time_points_ds[[time_point_input + 1]]$video_replicate = 1:12
    }
    
    if(time_point_input %in% 1:5){
      time_points_ds[[time_point_input + 1]]$video_replicate = 1
    }
    
    if(time_point_input %in% 6:7){
      time_points_ds[[time_point_input + 1]]$video_replicate = rep(1:2, each = 110)
    }
    
    #Manipulate columns
    time_points_ds[[time_point_input + 1]] = time_points_ds[[time_point_input + 1]] %>%
      mutate(time_point = time_point_input,
             day = sampling_days[(time_point_input + 1)],
             file = as.double(str_extract(file, "\\d+"))) %>%
      select(
        time_point,
        day,
        file,
        video_replicate,
        culture_ID,
        bioarea_per_volume,
        indiv_per_volume,
        Ble,
        Cep,
        Col,
        Eug,
        Eup,
        Lox,
        Pau,
        Pca,
        Spi,
        Spi_te,
        Tet,
        Ble_main_analysis,
        Cep_main_analysis,
        Spi_main_analysis
      )
    
  }
  
    ds_patches = time_points_ds %>%
    bind_rows() %>%
    left_join(culture_info,
              by = "culture_ID") 
  
  return(ds_patches)
    
}
