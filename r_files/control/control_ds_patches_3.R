#Check that the videos you took off are the right ones
setdiff(ds_patches_before_taking_off_videos,
        ds_patches) %>%
  select(time_point,
         file)