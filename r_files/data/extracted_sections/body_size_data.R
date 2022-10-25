## ------------------------------------------------------------------------------------------------------------------------------------
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


## ----body-size-tidy-time-points------------------------------------------------------------------------------------------------------
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


## ----ds-body-size-creation-----------------------------------------------------------------------------------------------------------
cultures_n = max(culture_info$culture_ID)
original_t0_rows = nrow(t0)
ID_vector = rep(1:cultures_n, each = original_t0_rows)
t0 = t0 %>%
  slice(rep(1:n(), cultures_n)) %>%
  mutate(culture_ID = ID_vector)

t0 = merge(culture_info, t0, by="culture_ID")
t1 = merge(culture_info, t1, by="culture_ID")
t2 = merge(culture_info, t2, by="culture_ID")
t3 = merge(culture_info, t3, by="culture_ID")
t4 = merge(culture_info, t4, by="culture_ID")
t5 = merge(culture_info, t5, by="culture_ID")
t6 = merge(culture_info, t6, by="culture_ID")
t7 = merge(culture_info, t7, by="culture_ID")
ds_body_size = rbind(t0, t1, t2, t3, t4, t5, t6, t7)
rm(t0, t1, t2, t3, t4, t5, t6, t7)


## ----tidy-body-size-ds---------------------------------------------------------------------------------------------------------------
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

#Select useful columns
ds_body_size = ds_body_size %>% 
  select(culture_ID, 
         patch_size, 
         patch_size_volume,
         disturbance, 
         metaecosystem_type, 
         mean_area, 
         replicate_video, 
         time_point,
         day, 
         metaecosystem, 
         system_nr, 
         eco_metaeco_type)

#Reorder columns
ds_body_size = ds_body_size[, c("culture_ID", 
            "system_nr", 
            "disturbance", 
            "time_point",
            "day",
            "patch_size", 
            "patch_size_volume",
            "metaecosystem", 
            "metaecosystem_type", 
            "eco_metaeco_type", 
            "replicate_video",
            "mean_area")]


## ------------------------------------------------------------------------------------------------------------------------------------
datatable(ds_body_size,
          rownames = FALSE,
          options = list(scrollX = TRUE),
          filter = list(position = 'top', 
                        clear = FALSE))




## ------------------------------------------------------------------------------------------------------------------------------------
ds_classes = readRDS(here("results", "ds_classes.RData")) #Watch out: it contains 27468 rows instead of 27720 because we excluded already culture_ID = 60, which I spilled during the experiment.

ds_classes_averaged = ds_classes %>%
  group_by(
    culture_ID,
    eco_metaeco_type,
    patch_size,
    disturbance,
    day,
    log_size_class
  ) %>%
  summarise(log_abundance = mean(log_size_class_abundance)) %>%
  group_by(eco_metaeco_type, patch_size, disturbance, day, log_size_class) %>%
  summarise(
    log_abundance_sd = sd(log_abundance),
    log_abundance = mean(log_abundance),
    sample_size = n(),
  ) %>%
  mutate(
    log_abundance_se = log_abundance_sd / sqrt(sample_size),
    log_abundance_lower_ci = log_abundance - qt(1 - (0.05 / 2), sample_size - 1) * log_abundance_se,
    log_abundance_upper_ci = log_abundance + qt(1 - (0.05 / 2), sample_size - 1) * log_abundance_se
  ) #Expected number of rows: 12 size classes * 8 eco_metaeco_types * 2 disturbance types * 8 time points = 1536

saveRDS(ds_classes_averaged,
        file = here("results", "ds_classes_averaged.RData"))


## ------------------------------------------------------------------------------------------------------------------------------------
ds_classes_averaged = readRDS(here("results", "ds_classes_averaged.RData"))

datatable(ds_classes_averaged,
          rownames = FALSE,
          options = list(scrollX = TRUE),
          filter = list(position = 'top', 
                        clear = FALSE))


## ----ds_median_body_size-creation----------------------------------------------------------------------------------------------------
eco_metaeco_types = unique(culture_info$eco_metaeco_type)

ds_median_body_size = ds_body_size %>%
        group_by(culture_ID, time_point, replicate_video) %>%
        mutate(median_body_size = median(mean_area)) %>%
        ungroup()


## ------------------------------------------------------------------------------------------------------------------------------------
datatable(ds_median_body_size,
          rownames = FALSE,
          options = list(scrollX = TRUE),
          filter = list(position = 'top', 
                        clear = FALSE))




## ------------------------------------------------------------------------------------------------------------------------------------
ds_lnRR_median_body_size = readRDS(here("results", "ds_lnRR_median_body_size.RData"))

datatable(ds_lnRR_median_body_size,
          rownames = FALSE,
          options = list(scrollX = TRUE),
          filter = list(position = 'top', 
                        clear = FALSE))


## ------------------------------------------------------------------------------------------------------------------------------------

#How do you calculate the quantiles in dplyr?

disturbance_input = "low"
eco_metaeco_input = "S (S_S)"
time_point_input = 4

ds_body_size %>%
  filter(disturbance == disturbance_input,
         eco_metaeco_type == eco_metaeco_input,
         time_point == time_point_input,
         culture_ID == 16,
         replicate_video == 1) #%>%
  #filter(mean_area > quantile(mean_area, 0.125), mean_area < quantile(x, 0.125))


