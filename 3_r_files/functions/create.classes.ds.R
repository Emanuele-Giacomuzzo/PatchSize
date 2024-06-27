create.classes.ds = function(ds_individuals,
                             n_size_classes) {
  smallest_size = min(ds_individuals$body_area_µm2)
  largest_size = max(ds_individuals$body_area_µm2)
  
  size_class_width_µm2 = (largest_size - smallest_size) / n_size_classes
  
  size_class_boundaries_µm2 = seq(from = smallest_size,
                                  to = largest_size,
                                  by = size_class_width_µm2)
  
  single_videos_rows = NULL
  row = 0
  for (class_input in 1:n_size_classes) {
    bin_lower_limit_µm2 = size_class_boundaries_µm2[class_input]
    bin_upper_limit_µm2 = size_class_boundaries_µm2[class_input + 1]
    mean_class_area_µm2 = (size_class_boundaries_µm2[class_input] + size_class_boundaries_µm2[class_input + 1]) / 2
    
    for (time_point_input in time_points) {
      for (culture_ID_input in 1:n_cultures) {
        if (culture_ID_input %in% ecosystems_to_take_off) {
          next
        }
        
        for (video_input in 1:videos_taken$nr_videos[videos_taken$time_point == time_point_input]) {
          row = row + 1
          
          video_class_indiv_per_µl = ds_individuals %>%
            filter(
              time_point == time_point_input,
              culture_ID == culture_ID_input,
              video_replicate == video_input,
              body_area_µm2 >= bin_lower_limit_µm2,
              body_area_µm2 <= bin_upper_limit_µm2
            ) %>%
            summarise(class_indiv_per_µl = ((sum (N_frames)) / total_frames) / volume_recorded_μl) %>%
            pull()
          
          single_videos_rows[[row]] = ds_ecosystems %>%
            filter(time_point == time_point_input,
                   culture_ID == culture_ID_input) %>%
            select(all_of(columns_ecosystems)) %>%
            mutate(
              replicate_video = video_input,
              size_class_n = class_input,
              mean_class_area_µm2 = mean_class_area_µm2,
              class_indiv_per_µl = video_class_indiv_per_µl
            )
          
        }
      }
    }
  }
  
  ds_classes = single_videos_rows %>%
    bind_rows()
  
  return(ds_classes)
  
}
