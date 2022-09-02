## -----------------------------------------------------------------------------------------------------------------------------------------
culture_info = read.csv(here("data", "PatchSizePilot_culture_info.csv"), header = TRUE)
load(here("data", "morphology", "t0.RData"));t0 = morph_mvt
load(here("data", "morphology", "t1.RData"));t1 = morph_mvt
load(here("data", "morphology", "t2.RData"));t2 = morph_mvt
load(here("data", "morphology", "t3.RData"));t3 = morph_mvt
load(here("data", "morphology", "t4.RData"));t4 = morph_mvt
load(here("data", "morphology", "t5.RData"));t5 = morph_mvt
load(here("data", "morphology", "t6.RData"));t6 = morph_mvt
load(here("data", "morphology", "t7.RData"));t7 = morph_mvt
rm(morph_mvt)

datatable(culture_info[,1:10],
          rownames = FALSE,
          options = list(scrollX = TRUE),
          filter = list(position = 'top', 
                        clear = FALSE))


## -----------------------------------------------------------------------------------------------------------------------------------------

### --- Tidy t0 - t7 data-sets --- ###

#Column: time
t0$time = NA
t1$time = NA

#Column: replicate_video
t0$replicate_video[t0$file == "sample_00001"] = 1
t0$replicate_video[t0$file == "sample_00002"] = 2
t0$replicate_video[t0$file == "sample_00003"] = 3
t0$replicate_video[t0$file == "sample_00004"] = 4
t0$replicate_video[t0$file == "sample_00005"] = 5
t0$replicate_video[t0$file == "sample_00006"] = 6
t0$replicate_video[t0$file == "sample_00007"] = 7
t0$replicate_video[t0$file == "sample_00008"] = 8
t0$replicate_video[t0$file == "sample_00009"] = 9
t0$replicate_video[t0$file == "sample_00010"] = 10
t0$replicate_video[t0$file == "sample_00011"] = 11
t0$replicate_video[t0$file == "sample_00012"] = 12
t1$replicate_video = 1 #In t1 I took only 1 video/culture
t2$replicate_video = 1 #In t2 I took only 1 video/culture
t3$replicate_video = 1 #In t3 I took only 1 video/culture
t4$replicate_video = 1 #In t4 I took only 1 video/culture
t5$replicate_video = 1 #In t5 I took only 1 video/culture
t6 = t6 %>% rename(replicate_video = replicate)
t7 = t7 %>% rename(replicate_video = replicate)


### --- Create ds_body_size dataset --- ###

long_t0 = t0 %>% slice(rep(1:n(), max(culture_info$culture_ID)))
ID_vector = NULL
ID_vector_elongating = NULL
for (ID in 1:max(culture_info$culture_ID)){
  ID_vector = rep(ID, times = nrow(t0))
  ID_vector_elongating = c(ID_vector_elongating, ID_vector)
}
long_t0$culture_ID = ID_vector_elongating
t0 = merge(culture_info,long_t0, by="culture_ID"); rm(long_t0)
t1 = merge(culture_info,t1,by="culture_ID")
t2 = merge(culture_info,t2,by="culture_ID")
t3 = merge(culture_info,t3,by="culture_ID")
t4 = merge(culture_info,t4,by="culture_ID")
t5 = merge(culture_info,t5,by="culture_ID")
t6 = merge(culture_info,t6,by="culture_ID")
t7 = merge(culture_info,t7,by="culture_ID")
ds_body_size = rbind(t0, t1, t2, t3, t4, t5, t6, t7); rm(t0, t1, t2, t3, t4, t5, t6, t7)

### --- Tidy ds_body_size data-set --- ###

#Column: day
ds_body_size$day = ds_body_size$time_point;
ds_body_size$day[ds_body_size$day=="t0"] = "0"
ds_body_size$day[ds_body_size$day=="t1"] = "4"
ds_body_size$day[ds_body_size$day=="t2"] = "8"
ds_body_size$day[ds_body_size$day=="t3"] = "12"
ds_body_size$day[ds_body_size$day=="t4"] = "16"
ds_body_size$day[ds_body_size$day=="t5"] = "20"
ds_body_size$day[ds_body_size$day=="t6"] = "24"
ds_body_size$day[ds_body_size$day=="t7"] = "28"
ds_body_size$day = as.numeric(ds_body_size$day)

#Column: time point
ds_body_size$time_point[ds_body_size$time_point=="t0"] = 0
ds_body_size$time_point[ds_body_size$time_point=="t1"] = 1
ds_body_size$time_point[ds_body_size$time_point=="t2"] = 2
ds_body_size$time_point[ds_body_size$time_point=="t3"] = 3
ds_body_size$time_point[ds_body_size$time_point=="t4"] = 4
ds_body_size$time_point[ds_body_size$time_point=="t5"] = 5
ds_body_size$time_point[ds_body_size$time_point=="t6"] = 6
ds_body_size$time_point[ds_body_size$time_point=="t7"] = 7
ds_body_size$time_point = as.character(ds_body_size$time_point)

#Column: eco_metaeco_type
ds_body_size$eco_metaeco_type = factor(ds_body_size$eco_metaeco_type, 
                             levels=c('S', 'S (S_S)', 'S (S_L)', 'M', 'M (M_M)', 'L', 'L (L_L)', 'L (S_L)'))

#Select useful columns
ds_body_size = ds_body_size %>% 
  select(culture_ID, 
         patch_size, 
         disturbance, 
         metaecosystem_type, 
         mean_area, 
         replicate_video, 
         day, 
         metaecosystem, 
         system_nr, 
         eco_metaeco_type)

#Reorder columns
ds_body_size = ds_body_size[, c("culture_ID", 
            "system_nr", 
            "disturbance", 
            "day",
            "patch_size", 
            "metaecosystem", 
            "metaecosystem_type", 
            "eco_metaeco_type", 
            "replicate_video",
            "mean_area")]

datatable(ds_body_size,
          rownames = FALSE,
          options = list(scrollX = TRUE),
          filter = list(position = 'top', 
                        clear = FALSE))


## -----------------------------------------------------------------------------------------------------------------------------------------

#### --- PARAMETERS & INITIALISATION --- ###

nr_of_size_classes = 12
largest_size = max(ds_body_size$mean_area)
size_class_width = largest_size/nr_of_size_classes
size_class = NULL

### --- CREATE DATASET --- ###

size_class_boundaries = seq(0, largest_size, by = size_class_width)

for (class in 1:nr_of_size_classes){
  
  bin_lower_limit = size_class_boundaries[class]
  bin_upper_limit = size_class_boundaries[class+1]
  size_input = (size_class_boundaries[class] + size_class_boundaries[class + 1])/2
  
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
    mutate(size = size_input) %>%
    mutate(log_size = log(size))
  
}

ds_classes = rbind(size_class[[1]], size_class[[2]], size_class[[3]], size_class[[4]],
                  size_class[[5]], size_class[[6]], size_class[[7]], size_class[[8]],
                  size_class[[9]], size_class[[10]], size_class[[11]], size_class[[12]],)

datatable(ds_classes,
          rownames = FALSE,
          options = list(scrollX = TRUE),
          filter = list(position = 'top', 
                        clear = FALSE))

